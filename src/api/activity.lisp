(in-package #:my-editor/api)

(export '(activity
          cursor activity-cursor
          activity-factory
          activity-gui-window))
(defclass activity ()
    ((factory :initform (error "Activity FACTORY not set")
              :initarg :factory
              :type activity-factory
              :reader activity-factory)
     (gui-window :initform nil
                 :type (or null gui:window)
                 :reader activity-gui-window)
     (initial-title :initform "" ; todo: move to factory
                    :initarg :title
                    :type string)
     (get-title :initform nil
                :type (or null function))
     (set-title :initform nil
                :type (or null function))
     (cursor :initform :default ; todo: make a generic
             :type (or gui:cursor (function (single-float single-float) (values gui:cursor &optional)))
             :accessor activity-cursor)
     (container-widget :initform nil
                       :type (or null gui:widget))))

(export '(activity-factory
          key activity-factory-key
          path activity-factory-path
          gfx-backend-keys activity-factory-gfx-backend-keys))
(defclass activity-factory ()
    ((key :initform (error "KEY not specified")
          :type symbol
          :reader activity-factory-key)
     (path :initform (error "PATH not specified")
           :type (vector string)
           :reader activity-factory-path)
     (gfx-backend-keys :initform nil
                       :type (or symbol list)
                       :reader activity-factory-gfx-backend-key)))

(export 'create-activity)
(defgeneric create-activity (factory))

(export 'set-activity-title)
(declaim (ftype (function (activity string) (values boolean &optional)) set-activity-title))
(defun set-activity-title (activity title)
  (let ((setter (slot-value activity 'set-title)))
    (when setter
          (progn
           (funcall setter title)
           t))))

(export 'get-activity-title)
(declaim (ftype (function (activity) (values string &optional)) get-activity-title))
(defun get-activity-title (activity)
  (let ((getter (slot-value activity 'get-title)))
    (if getter (funcall getter) (slot-value activity 'initial-title))))

(export 'destroy-activity)
(defgeneric destroy-activity (activity)
  (:method (activity)
           (values)))

(export 'compose-activity-ui)
(defgeneric compose-activity-ui (ui activity)
  (:method (ui activity)
           nil))

(export 'recompose-activity)
(declaim
  (notinline recompose-activity)
  (ftype (function (activity) (values &optional)) recompose-activity))

(export 'render-activity)
(defgeneric render-activity (ui gfx activity x y w h ww wh)
  (:method (ui gfx activity x y w h ww wh)
           (declare (ignore ui gfx activity x y w h ww wh))
           (values)))

(export 'register-activity-factory)
(declaim
  (notinline register-activity-factory)
  (ftype (function (activity-factory) (values &optional)) register-activity-factory))

(export 'on-layout)
(defgeneric on-layout (ui activity x y w h)
  (:method (ui activity x y w h)
           (declare (ignore ui activity x y w h))
           (values)))

(export 'on-focus-recieved)
(defgeneric on-focus-recieved (ui activity)
  (:method (ui activity)
           (declare (ignore ui activity))
           (values)))

(export 'on-focus-lost)
(defgeneric on-focus-lost (ui activity)
  (:method (ui activity)
           (declare (ignore ui activity))
           (values)))

(export 'on-key-action)
(defgeneric on-key-action (ui activity action mod key)
  (:method (ui activity action mod key)
           (declare (ignore ui activity action mod key))
           (values)))

(export 'on-key-down)
(defgeneric on-key-down (ui activity val mod key)
  (:method (ui activity val mod key)
           (declare (ignore ui activity val mod key))
           (values)))

(export 'on-key-up)
(defgeneric on-key-up (ui activity val mod key)
  (:method (ui activity val mod key)
           (declare (ignore ui activity val mod key))
           (values)))

(export 'on-mouse-move)
(defgeneric on-mouse-move (ui activity x y event)
  (:method (ui activity x y event)
           (declare (ignore ui activity x y event))
           nil))

(export 'on-mouse-scroll)
(defgeneric on-mouse-scroll (ui activity x y sx sy event)
  (:method (ui activity x y sx sy event)
           (declare (ignore ui activity x y event))
           nil))

(export 'on-mouse-down-left)
(defgeneric on-mouse-down-left (ui activity x y event)
  (:method (ui activity x y event)
           (declare (ignore ui activity x y event))
           nil))

(export 'on-mouse-up-left)
(defgeneric on-mouse-up-left (ui activity x y event)
  (:method (ui activity x y event)
           (declare (ignore ui activity x y event))
           nil))

(export 'on-mouse-down-right)
(defgeneric on-mouse-down-right (ui activity x y event)
  (:method (ui activity x y event)
           (declare (ignore ui activity x y event))
           nil))

(export 'on-mouse-up-right)
(defgeneric on-mouse-up-right (ui activity x y event)
  (:method (ui activity x y event)
           (declare (ignore ui activity x y event))
           nil))

(export 'on-mouse-down-middle)
(defgeneric on-mouse-down-middle (ui activity x y event)
  (:method (ui activity x y event)
           (declare (ignore ui activity x y event))
           nil))

(export 'on-mouse-up-middle)
(defgeneric on-mouse-up-middle (ui activity x y event)
  (:method (ui activity x y event)
           (declare (ignore ui activity x y event))
           nil))

(export 'on-drag-enter)
(declaim (ftype (function (gui:ui activity single-float single-float) (values boolean &optional)) on-drag-enter))
(defgeneric on-drag-enter (ui activity x y)
  (:method (ui activity x y)
           (declare (ignore ui activity x y))
           nil))

(export 'on-drag-move)
(declaim (ftype (function (gui:ui activity single-float single-float) (values boolean &optional)) on-drag-move))
(defgeneric on-drag-move (ui activity x y)
  (:method (ui activity x y)
           (declare (ignore ui activity x y))
           nil))

(export 'on-drag-leave)
(declaim (ftype (function (gui:ui activity) (values &optional)) on-drag-leave))
(defgeneric on-drag-leave (ui activity)
  (:method (ui activity)
           (declare (ignore ui activity))
           (values)))

(export 'on-drag-drop)
(declaim (ftype (function (gui:ui activity) (values boolean &optional)) on-drag-drop))
(defgeneric on-drag-drop (ui activity)
  (:method (ui activity)
           (declare (ignore ui activity))
           nil))

(export 'map-activities)
(declaim
  (notinline map-activities)
  (ftype (function ((function (activity) (values &optional))) (values &optional))
         map-activities))

(export 'activity-x)
(declaim (ftype (function (activity) (values double-float &optional)) activity-x))
(defun activity-x (activity)
  (gui:window-x (activity-gui-window activity)))

(export 'activity-y)
(declaim (ftype (function (activity) (values double-float &optional)) activity-y))
(defun activity-y (activity)
  (gui:window-y (activity-gui-window activity)))

(export 'activity-width)
(declaim (ftype (function (activity) (values double-float &optional)) activity-width))
(defun activity-width (activity)
  (gui:window-width (activity-gui-window activity)))

(export 'activity-height)
(declaim (ftype (function (activity) (values double-float &optional)) activity-height))
(defun activity-height (activity)
  (gui:window-height (activity-gui-window activity)))
