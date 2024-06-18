(in-package #:my-editor)

(defclass file-browser-activity-factory (api:activity-factory)
    ((api:key :initform :file-browser)
     (api:path :initform #("General" "File Browser"))))

(defclass file-browser-activity (api:activity)
    ((directory :initarg :directory
                :type list)
     (contents :initform (make-observable-vector (api:sdet-context))
               :type observable-vector)
     (restore-focus :initform nil ;; T means to focus on ..
                    :type (member nil t symbol string))))

(defmethod api:create-activity ((factory file-browser-activity-factory))
  (let* ((directory (project-directory (app-project **app**)))
         (activity
          (make-instance 'file-browser-activity
            :factory factory
            :title "File Browser"
            :directory directory)))
    (set-file-browser-directory activity directory)
    activity))

(defconstant +file-browser-bar-height+ 24.0)

(defun v-directory-up (&key
                       (scale-x 1.0d0) (scale-y 1.0d0)
                       (fill-style #xFFFFFFFF) (border-style #xFF000000) (border-width 1.0d0))
  (let ((border-width (coerce border-width 'double-float)))
    (gui:v-path* (lambda (path width height)
                   (let ((dist (/ width 3.0d0))
                         (r (max (* (min width height) 0.08d0) (+ border-width 1.5d0)))
                         (y (* 0.9d0 height)))
                     (cffi:with-foreign-object (circle :char (autowrap:sizeof '%blend2d:circle))
                       (let ((circle-aw (autowrap:wrap-pointer circle '%blend2d:circle)))
                         (setf (%blend2d:circle.r circle-aw) r)
                         (setf (%blend2d:circle.cx circle-aw) dist)
                         (setf (%blend2d:circle.cy circle-aw) y)
                         (%blend2d:path-add-geometry path %blend2d:+geometry-type-circle+ circle (cffi:null-pointer) %blend2d:+geometry-direction-cw+)
                         (setf (%blend2d:circle.cx circle-aw) (+ dist dist))
                         (%blend2d:path-add-geometry path %blend2d:+geometry-type-circle+ circle (cffi:null-pointer) %blend2d:+geometry-direction-cw+)
                         ;      ;  (%blend2d:path-arc-to path dist y r r 0.0d0 (* 2.0d0 pi) %blend2d:+true+)
                         ;      ;  (%blend2d:path-arc-to path (+ dist dist) y r r 0.0d0 (* 2.0d0 pi) %blend2d:+true+)
                     )))
                   (values))
                 :scale-x scale-x
                 :scale-y scale-y
                 :alignment :center-bottom
                 :fill-style fill-style
                 :border-style border-style
                 :border-width border-width)))

(defun v-file (&key
               (scale-x 1.0d0) (scale-y 1.0d0)
               (fill-style #xFFFFFFFF) (border-style #xFF000000) (border-width 1.0d0))
  (gui:v-path #(0.0 0.0 1.0 1.0 0.8)
              #(0.0 1.0 1.0 0.2 0.0)
              :scale-x scale-x
              :scale-y scale-y
              :alignment :center-bottom
              :fill-style fill-style
              :border-style border-style
              :border-width border-width))

(defun v-directory (&key
                    (scale-x 1.0d0) (scale-y 1.0d0)
                    (fill-style #xFFFFFFFF) (border-style #xFF000000) (border-width 1.0d0))
  (gui:v-path #(0.0 0.2 0.25 1.0 1.0 0.0 0.0 nil 1.0 1.0 0.0 0.0)
              #(0.0 0.0 0.1 0.1 0.2 0.2 0.0 nil 0.2 1.0 1.0 0.2)
              :scale-x scale-x
              :scale-y scale-y
              :alignment :center-bottom
              :fill-style fill-style
              :border-style border-style
              :border-width border-width
              :close nil))

(defun w-directory-item (ui activity item &key (fill-style #xFFFFFFFF) (border-style #xFF000000) (border-width 1.0d0))
  (let (text visual)
    (cond
     ((null item) (setf visual (v-directory-up :fill-style fill-style
                                               :border-style border-style
                                               :border-width border-width)))
     ((null (pathname-name item))
       (setf text (car (last (pathname-directory item))))
       (setf visual (v-directory :scale-x 0.9 :scale-y 0.72
                                 :fill-style fill-style
                                 :border-style border-style
                                 :border-width border-width)))
     (t
       (setf text (if (pathname-type item)
                      (concatenate 'string (pathname-name item) "." (pathname-type item))
                      (pathname-name item)))
       (setf visual (v-file :scale-x 0.8 :scale-y 1.0
                            :fill-style fill-style
                            :border-style border-style
                            :border-width border-width))))

    (let ((widget
           (gui:macroexpand-with-ui ui
             (gui:w-visual ((gui:v-rectangle :fill-style (gui:wsd widget
                                                           (:hover #x9900FFFF)
                                                           (t nil))
                                             :border-style (gui:wsd widget
                                                             (:focus #xAA00AAAA)
                                                             (t nil))
                                             :border-width 2))
               (:let widget
                     :layout ((min-width 50.0)
                              (padding yogalayout:+edge-all+ 3.0)
                              (align-items yogalayout:+align-center+)))
               (gui:w-visual (visual) (:layout ((width 50.0)
                                                (height 56.0))))
               (when text
                     (gui:w-label :text text :cursor nil))))))
      (setf (gui:widget-cursor widget) :press)
      (setf (gui:widget-hitp widget) #'gui:default-widget-hitp)
      (setf (gui::widget-focus-behavior-as-sibling widget) :focus)
      (let ((on-used (cond
                      ((null item)
                        (lambda ()
                          (let* ((reversed (reverse (slot-value activity 'directory)))
                                 (new-directory (reverse (cdr reversed)))
                                 (old-directory-name (car reversed)))
                            (when new-directory
                                  (set-file-browser-directory activity new-directory)
                                  (setf (slot-value activity 'restore-focus) old-directory-name)
                                  t))))
                      ((null (pathname-name item))
                        (lambda ()
                          (set-file-browser-directory activity (pathname-directory item))
                          (setf (slot-value activity 'restore-focus) t)
                          t)))))
        (when on-used
              (setf (gui:widget-on-key-action widget)
                (lambda (ui widget action keymod event)
                  (declare (ignore ui widget keymod event))
                  (when (and (eq action :enter))
                        (funcall on-used)))))
        (setf (gui:widget-on-mouse-click-left widget)
          (lambda (ui widget x y event)
            (declare (ignore x y))
            (let ((clicks (%sdl3:mouse-button-event.clicks event)))
              (cond
               ((= clicks 1)
                 (gui:set-keyboard-focus ui widget))
               ((= clicks 2)
                 (when on-used (funcall on-used))))))))
      (when (and item (pathname-name item))
            (let ((dragdrop nil)
                  dragdrop-x0 dragdrop-y0)
              (setf (gui:widget-on-mouse-down-left widget)
                (lambda (ui widget x y event)
                  (declare (ignore event))
                  (setf dragdrop t dragdrop-x0 x dragdrop-y0 y)
                  (gui:own-mouse ui widget)
                  t))
              (setf (gui:widget-on-mouse-up-left widget)
                (lambda (ui widget x y event)
                  (declare (ignore x y event))
                  (when dragdrop
                        (gui:disown-mouse ui widget)
                        (setf dragdrop nil)
                        t)))
              (setf (gui:widget-on-mouse-ownership-lost widget)
                (lambda (ui widget)
                  (declare (ignore ui widget))
                  (set dragdrop nil)
                  (values)))
              (setf (gui:widget-on-mouse-move widget)
                (lambda (ui widget x y event)
                  (when dragdrop
                        (let ((dx (- x dragdrop-x0))
                              (dy (- y dragdrop-y0)))
                          (when (> (+ (* dx dx) (* dy dy)) (* 6 6))
                                (setf dragdrop nil)
                                (gui:disown-mouse ui widget)
                                (api:start-dragging item nil nil :parent-window (%sdl3:mouse-motion-event.window-id event))))
                        t)))))
      widget)))

(defun set-file-browser-directory (activity directory)
  (setf (slot-value activity 'directory) directory)
  (let* ((contents (slot-value activity 'contents))
         (new-entries (uiop:directory* (make-pathname :directory directory :name :wild :type :wild)))
         (contents-v (observable-vector-values contents)))
    (loop #:for i #:below (length contents-v)
          #:do (setf (aref contents-v i) nil))
    (setf (fill-pointer contents-v) 0)
    (unless (equalp (api:project-directory) directory)
      (vector-push-extend nil contents-v))
    (loop #:for new-entry #:in new-entries
          #:do
          (vector-push-extend new-entry contents-v))
    (sort contents-v (lambda (a b)
                       (block lambda
                         (when (null a) (return-from lambda t))
                         (when (null b) (return-from lambda nil))
                         (let* ((a-name (pathname-name a))
                                (b-name (pathname-name b))
                                (a-name-n (if a-name 1 0))
                                (b-name-n (if b-name 1 0))
                                (a-dir (pathname-directory a))
                                (b-dir (pathname-directory b))
                                (a-type (pathname-type a))
                                (b-type (pathname-type b)))
                           (cond
                            ((< a-name-n b-name-n) t)
                            ((> a-name-n b-name-n) nil)
                            (t (loop #:while (and a-dir b-dir)
                                     #:for ax = (car a-dir)
                                     #:for bx = (car b-dir)
                                     #:do
                                     (setf a-dir (cdr a-dir) b-dir (cdr b-dir))
                                     (cond
                                      ((eq ax bx))
                                      ((symbolp ax) (return-from lambda t))
                                      ((symbolp bx) (return-from lambda nil))
                                      ((string< ax bx) (return-from lambda t))
                                      ((string> ax bx) (return-from lambda nil))))
                               (cond
                                ((and (null a-dir) (null b-dir))
                                  (if a-name
                                      (cond
                                       ((string< a-name b-name) t)
                                       ((string> a-name b-name) nil)
                                       ((and (null a-type) (null b-type)) nil)
                                       ((null a-type) t)
                                       ((null b-type) nil)
                                       (t (string< a-type b-type)))
                                      nil))
                                ((null a-dir) t)
                                ((null b-dir) nil))))))))
    (funcall (observable-vector-notify contents)))
  (values))

(defmethod api:compose-activity-ui (ui (activity file-browser-activity))
  (with-slots (directory contents restore-focus) activity
    (multiple-value-bind (get-separator set-separator)
        (sdet:make-state (gui:sdet-context ui) 0.0d0)
      (gui:macroexpand-with-ui ui
        (gui:w-visual ((gui:v-rectangle :fill-style (api:colors-background ui)))
          (:layout ((flex-direction yogalayout:+flex-direction-row+)
                    (flex-grow 1.0)))
          (gui:w-two-pane (:layout ((flex-grow 1.0)) :threshold (funcall get-separator) :on-threshold-changed 'set-separator)
                          (nil (gui:w-label :text "Area non implementata numero quattro"))
                          (nil
                           (gui:w-visual ((gui:v-rectangle :fill-style (api:colors-background-focus ui)
                                                           :border-style (api:colors-border ui)
                                                           :border-width 1.0))
                             (:layout ((height +file-browser-bar-height+)
                                       (width-percent 100.0)
                                       (justify-content yogalayout:+justify-space-between+)
                                       (flex-direction yogalayout:+flex-direction-row+)))
                             (gui:w-label :font-size 14.0
                                          :text (let ((dst ""))
                                                  (funcall (observable-vector-subscribe contents)) ; directory slot is not reactive but is synced with contents mostly
                                                  (loop #:for item #:in directory
                                                        #:do (if (symbolp item)
                                                                 (case item
                                                                   (:absolute (setf dst "/"))
                                                                   (:up (setf dst (concatenate 'string dst "../"))))
                                                                 (setf dst (concatenate 'string dst item "/"))))
                                                  dst))
                             (gui:w-block ((height-percent 100.0)
                                           (flex-direction yogalayout:+flex-direction-row+))
                              ;  (gui:w-button :layout ((height-percent 100.0)
                              ;                         (aspect-ratio 1.0))
                              ;                :on-click ((values))
                              ;                :visual ((gui:v-checkmark :fill-style #xFFFFFF00
                              ;                                          :border-style #xFF000000
                              ;                                          :border-width 1.0)))
                              ;  (gui:w-button :layout ((height-percent 100.0)
                              ;                         (aspect-ratio 1.0))
                              ;                :on-click ((values))
                              ;                :visual ((gui:v-rectangle :fill-style #xFFFFFFFF)))
                               ))
                           (gui:w-scroll ((flex-direction yogalayout:+flex-direction-row+)
                                          (flex-wrap yogalayout:+wrap-wrap+)
                                          (padding yogalayout:+edge-all+ 7.0)
                                          (gap yogalayout:+gutter-column+ 7.0)
                                          (gap yogalayout:+gutter-row+ 12.0))
                             (:focus-behavior-as-parent :cycle)
                             (gui:w-map-fix-index (progn
                                                   (funcall (observable-vector-subscribe contents))
                                                   (observable-vector-values contents))
                               (get-item nil :equal nil)
                               (let* ((item (funcall get-item))
                                      (widget (w-directory-item ui activity item)))
                                 (when (or (and (eq t restore-focus)
                                                (null item))
                                           (and (not (null restore-focus))
                                                (equalp restore-focus
                                                        (car (last (pathname-directory item))))))
                                       (gui:set-keyboard-focus ui widget)
                                       (setf restore-focus nil)))))))))))
  (values))

; TODO:
; Tree view on the left
; Dynamically generated thumbnails
;; Thumbnail caching
; Sort/View modes
; Ctrl+C Ctrl+V + API
; Drag-n-drop + API
; Show in file browser API
; Activity-tree like movable separator (abstract widget + expose in api)

; General TODO:
; Ability for activity to be "raised" (aka programatically select tab with certain activity (and perhaps "flash"))
