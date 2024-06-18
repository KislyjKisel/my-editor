(in-package #:my-editor)

(defclass inspector-activity-factory (api:activity-factory)
    ((api:key :initform :inspector)
     (api:path :initform #("General" "Inspector"))))

(defclass inspector-activity (api:activity)
    ((locked :initform nil
             :type boolean)
     (value :initform nil)
     (on-value-forgotten :initform nil
                         :type (or null (function (t) (values &optional))))))

(defmethod api:create-activity ((factory inspector-activity-factory))
  (make-instance 'inspector-activity
    :factory factory
    :title "Inspector"))

(defun set-inspector-activity-value (activity new-value new-on-value-forgotten)
  "Set inspected value (and call ON-VALUE-FORGOTTEN if necessary)."
  (with-slots (value on-value-forgotten) activity
    (when (and value on-value-forgotten)
          (funcall on-value-forgotten value))
    (setf value new-value)
    (setf on-value-forgotten new-on-value-forgotten)
    (api:recompose-activity activity))
  (values))

(defun reset-inspector (activity)
  "Reset inspector state without(!) calling ON-VALUE-FORGOTTEN."
  (with-slots (value on-value-forgotten) activity
    (setf value nil)
    (setf on-value-forgotten nil)
    (api:recompose-activity activity))
  (values))

(defmethod api:compose-activity-ui (ui (activity inspector-activity))
  (gui:macroexpand-with-ui ui
    (gui:w-visual ((gui:v-rectangle :fill-style (api:colors-background ui)))
      (:layout ((flex-grow 1.0)
                (padding yogalayout:+edge-all+ 4.0)))
      (gui:w-block ((width-percent 100.0)
                    (height 16.0)
                    (flex-direction yogalayout:+flex-direction-row-reverse+))
        (gui:w-button :layout ((margin yogalayout:+edge-vertical+ 2.0)
                               (width 12.0)
                               (height 12.0))
                      :on-click ((setf (slot-value activity 'locked) (not (slot-value activity 'locked))))
                      :visual ((gui:v-lambda ui x y w h
                                 (gui:fill-rectangle* ui x y w h #xFF000000)
                                 (when (slot-value activity 'locked)
                                       (gui:fill-rectangle* ui
                                                            (+ 2.0d0 x)
                                                            (+ 2.0d0 y)
                                                            (- w 2.0d0)
                                                            (- h 2.0d0)
                                                            #xFFFFFFFF))))
                      :sensor (gui:s-rectangle)))
      (gui:w-visual ((gui:v-rectangle :fill-style #xFF666666))
        (:layout ((width-percent 100.0) (height 2.0) (margin yogalayout:+edge-bottom+ 3.0))))
      (api:compose-inspector ui (slot-value activity 'value)))))
