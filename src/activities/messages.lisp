(in-package #:my-editor)

(defclass messages-activity-factory (api:activity-factory)
    ((api:key :initform :messages)
     (api:path :initform #("General" "Messages"))))

(defclass messages-activity (api:activity)
    ())

(defmethod api:create-activity ((factory messages-activity-factory))
  (make-instance 'messages-activity
    :factory factory
    :title "Messages"))

(defun compose-message (ui app index parity)
  (gui:macroexpand-with-ui ui
    (gui:w-visual ((gui:v-rectangle :fill-style (if parity
                                                    (api:colors-background-focus ui)
                                                    (api:colors-background ui))))
      (:layout ((width-percent 100.0)
                (margin yogalayout:+edge-horizontal+ 4.0)))
      (gui:w-block ((flex-direction yogalayout:+flex-direction-row+)
                    (align-items yogalayout:+align-center+))
        (gui:w-visual ((gui:v-checkmark :fill-style
                                        (ecase (svref (app-message-categories app) index)
                                          (:error api::+error-color+)
                                          (:warning api::+warning-color+)
                                          (:info api::+info-color+)
                                          (:debug api::+debug-color+))))
          (:layout ((width 18.0)
                    (height 18.0)
                    (align-items yogalayout:+align-center+)
                    (margin yogalayout:+edge-all+ 4.0)
                    (margin yogalayout:+edge-right+ 8.0))))
        (gui:w-label :text (svref (app-messages app) index))))))

(defmethod api:compose-activity-ui (ui (activity messages-activity))
  (let ((app **app**))
    (gui:macroexpand-with-ui ui
      (gui:w-visual ((gui:v-rectangle :fill-style (api:colors-background ui))) ()
        (gui:w-visual ((gui:v-rectangle :fill-style (api:colors-fill ui)))
          (:layout ((height 8.0)
                    (padding yogalayout:+edge-top+ 4.0)
                    (padding yogalayout:+edge-horizontal+ 2.0)
                    (margin yogalayout:+edge-bottom+ 10.0)))
          (gui:w-button :layout ((height 12.0)
                                 (width 12.0))
                        :on-click ((clear-messages))
                        :let widget
                        :visual ((gui:v-rectangle :fill-style (gui:wsd widget
                                                                (:active #xFFFFFFFF)
                                                                (:hover #xFFAAAAAA)
                                                                (t #xFF777777))
                                                  :border-style #xFF000000
                                                  :border-width 1.0))))
        (gui:w-dynamic
          (funcall (app-subscribe-messages **app**))
          (let ((start (app-messages-start app))
                (end (app-messages-end app))
                (parity nil))
            (cond
             ((app-messages-empty app)
               (values))
             ((< start end)
               (loop #:for i #:from start #:below end
                     #:do (compose-message ui app i (setf parity (not parity)))))
             ((>= start end)
               (loop #:for i #:from start #:below +messages-capacity+
                     #:do (compose-message ui app i (setf parity (not parity))))
               (loop #:for i #:from 0 #:below end
                     #:do (compose-message ui app i (setf parity (not parity))))))))))))
