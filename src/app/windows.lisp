(in-package #:my-editor)

(alexandria:define-constant +default-title+ "My editor" :test #'equal)

(defclass window ()
    ((sdl-window :initarg :sdl-window
                 :type cffi:foreign-pointer
                 :reader window-sdl-window)
     (visible :initform t
              :type boolean
              :accessor window-visible)
     (destroying :initform nil
                 :type boolean)))

(defclass gfx-window (window)
    ((backend-key :initarg :gfx-backend-key
                  :type symbol
                  :reader gfx-window-backend-key)
     (backend :initarg :gfx-backend
              :reader gfx-window-backend)
     (gui-window :initarg :gui-window
                 :type (or null gui:window)
                 :reader gfx-window-gui-window)
     (resize-to-contents-x :initform nil
                           :initarg :resize-to-contents-x
                           :type boolean)
     (resize-to-contents-y :initform nil
                           :initarg :resize-to-contents-y
                           :type boolean)
     (checked-gui-window-width :type (signed-byte 32))
     (checked-gui-window-height :type (signed-byte 32))))

(defclass activities-window (gfx-window)
    ((activities :type (or activity-tree-node observable-vector))
     (subscribe-activities :type function)
     (notify-activities :type function)))

(defun gfx-window-gui-layer (gfx-window)
  (gui:window-layer (gfx-window-gui-window gfx-window)))

(defun window-sdl-id (window)
  (%sdl3:get-window-id (window-sdl-window window)))

(defun show-window (window)
  (%sdl3:show-window (window-sdl-window window))
  (setf (window-visible window) t)
  (values))

(defun hide-window (window)
  (%sdl3:hide-window (window-sdl-window window))
  (setf (window-visible window) nil)
  (values))

(defun window-activities (activities-window)
  (funcall (slot-value activities-window 'subscribe-activities))
  (slot-value activities-window 'activities))

(defun set-window-activities (activities-window activities)
  (setf (slot-value activities-window 'activities) activities)
  (funcall (slot-value activities-window 'notify-activities))
  (values))

(defmethod initialize-instance :after ((window window) &key &allow-other-keys)
  (let ((app **app**)
        (id (window-sdl-id window)))
    (setf (gethash id (app-windows app)) window)
    (when (null (app-main-window-id app))
          (setf (app-main-window-id app) id)))
  (values))

(defmethod initialize-instance :around
  ((window gfx-window)
   &rest args
   &key
   gfx-backend-key with-ui
   title width height flags
   resize-to-contents-x resize-to-contents-y
   parent offset-x offset-y
   &allow-other-keys)
  (when (and (not with-ui)
             (or resize-to-contents-x resize-to-contents-y))
        (error "Can't create GFX-WINDOW without UI but resized to contents"))
  (when (null gfx-backend-key)
        (setf gfx-backend-key +gfx-default-backend-key+))
  (setf width (if resize-to-contents-x 1 (or width 800)))
  (setf height (if resize-to-contents-y 1 (or height 600)))
  (multiple-value-bind (gfx-backend sdl-window layer)
      (api:gfx-initialize gfx-backend-key
                          with-ui
                          :window-title (or title +default-title+)
                          :window-width width
                          :window-height height
                          :window-flags (or flags 0)
                          :window-parent (if (typep parent 'window)
                                             (window-sdl-window parent)
                                             parent)
                          :window-offset-x offset-x
                          :window-offset-y offset-y)
    (let* ((app **app**)
           (gui-window (when with-ui
                             (let ((gui-window (gui:create-window layer
                                                                  :width (coerce width 'double-float)
                                                                  :height (coerce height 'double-float)
                                                                  :resize-to-contents-x resize-to-contents-x
                                                                  :resize-to-contents-y resize-to-contents-y)))
                               (gui:insert-window (app-ui app) gui-window)
                               gui-window))))
      (unless (eq :sdl-surface gfx-backend-key)
        (incf (app-non-gui-windows app)))
      (apply #'call-next-method window
        :gfx-backend-key gfx-backend-key
        :gfx-backend gfx-backend
        :sdl-window sdl-window
        :gui-window gui-window
        args))))

(defmethod initialize-instance :after ((window gfx-window) &key &allow-other-keys)
  (let ((gui-window (gfx-window-gui-window window)))
    (when gui-window
          (setf (slot-value window 'checked-gui-window-width) (floor (gui:window-width gui-window)))
          (setf (slot-value window 'checked-gui-window-height) (floor (gui:window-height gui-window)))))
  (values))

(defmethod initialize-instance :around ((window activities-window) &rest args &key flags &allow-other-keys)
  (apply #'call-next-method window :with-ui t :flags (logior %sdl3:+window-resizable+ (or flags 0)) args))

(defmethod initialize-instance :after ((window activities-window) &key &allow-other-keys)
  (let* ((app **app**)
         (ui (app-ui app))
         (sdet-ctx (gui:sdet-context ui)))
    (setf (slot-value window 'activities) (make-observable-vector sdet-ctx))
    (multiple-value-bind (subscribe-activities notify-activities)
        (sdet:make-notifier sdet-ctx)
      (setf (slot-value window 'subscribe-activities) subscribe-activities)
      (setf (slot-value window 'notify-activities) notify-activities))
    (gui:compose ui
                 (gfx-window-gui-window window)
                 (gui:macroexpand-with-ui ui
                   (gui:w-block ((flex-grow 1.0))
                     (w-activity-tree-node ui
                                           (lambda ()
                                             (window-activities window))
                                           (lambda (new-node)
                                             (set-window-activities window new-node))
                                           (lambda ()
                                             (when (loop #:for other-window #:being #:the #:hash-values #:of (app-windows app)
                                                         #:do (when (and (not (eq window other-window))
                                                                         (typep other-window 'activities-window))
                                                                    (return t)))
                                                   (destroy-window app window))
                                             (values))
                                           (lambda (widget)
                                             (let ((yoga-node (gui:widget-yoga-node widget)))
                                               (yogalayout:node-style-set-flex-grow yoga-node 1.0)
                                               ;  (yogalayout:node-style-set-width-percent yoga-node 100.0)
                                               ;  (yogalayout:node-style-set-height-percent yoga-node 100.0)
                                               )))
                     (gui:w-visual ((gui:v-rectangle :fill-style (api:colors-background-focus ui)))
                       (:layout ((height 14.0)
                                 (flex-direction yogalayout:+flex-direction-row+)
                                 (align-items yogalayout:+align-center+)
                                 (gap yogalayout:+gutter-column+ 4.0)
                                 (padding yogalayout:+edge-left+ 4.0)))
                       (gui:w-visual ((gui:v-checkmark :fill-style api::+info-color+)) (:layout ((width 13.0) (height 13.0))))
                       (gui:w-label :text (progn
                                           (funcall (app-subscribe-messages app))
                                           (write-to-string (gethash :info (app-messages-count app)))))
                       (gui:w-visual ((gui:v-checkmark :fill-style api::+warning-color+)) (:layout ((width 13.0) (height 13.0))))
                       (gui:w-label :text (progn
                                           (funcall (app-subscribe-messages app))
                                           (write-to-string (gethash :warning (app-messages-count app)))))
                       (gui:w-visual ((gui:v-checkmark :fill-style api::+error-color+)) (:layout ((width 13.0) (height 13.0))))
                       (gui:w-label :text (progn
                                           (funcall (app-subscribe-messages app))
                                           (write-to-string (gethash :error (app-messages-count app))))))))))
  (values))

(defun map-activity-tree-leaves (sdet-ctx tree f)
  (etypecase tree
    (activity-tree-node
     (multiple-value-bind (low high)
         (sdet:unobserved sdet-ctx
           (values
             (funcall (activity-tree-node-get-low tree))
             (funcall (activity-tree-node-get-high tree))))
       (map-activity-tree-leaves sdet-ctx low f)
       (map-activity-tree-leaves sdet-ctx high f)))
    (observable-vector
     (loop #:for activity #:across (observable-vector-values tree)
           #:do (funcall f activity))))
  (values))

(defun destroy-activity-tree (app tree)
  (map-activity-tree-leaves (sdet-context app)
                            tree
                            (lambda (activity)
                              (destroy-activity app activity)
                              (values))))

(defgeneric destroy-window-impl (app window)
  (:method (app (window window))
           (let ((sdl-window (window-sdl-window window)))
             (when sdl-window
                   (%sdl3:destroy-window sdl-window)))
           (values))
  (:method (app (window gfx-window))
           (unless (eq :sdl-surface (gfx-window-backend-key window))
             (decf (app-non-gui-windows app)))
           (let ((gui-window (gfx-window-gui-window window)))
             (when gui-window
                   (gui:delete-window (app-ui app) gui-window)
                   (gui:destroy-layer (gui:window-layer gui-window))))
           (api:gfx-terminate (gfx-window-backend window) (window-sdl-window window))
           (values))
  (:method (app (window activities-window))
           (destroy-activity-tree app (slot-value window 'activities))
           (call-next-method)))

(defun destroy-window (app window)
  (when (slot-value window 'destroying)
        (return-from destroy-window (values)))
  (setf (slot-value window 'destroying) t)
  (setf (app-active-popups app)
    (gui::vector-delete (window-sdl-id window) (app-active-popups app)))
  (remhash (window-sdl-id window) (app-windows app))
  (when (and (app-main-window-id app)
             (= (window-sdl-id window) (app-main-window-id app)))
        (setf (app-main-window-id app) nil))
  (destroy-window-impl app window)
  (values))

(defun render-activity-tree (app gfx window-width window-height tree)
  (let ((ui (app-ui app)))
    (map-activity-tree-leaves (gui:sdet-context ui)
                              tree
                              (lambda (activity)
                                (let ((gui-window (slot-value activity 'api::gui-window)))
                                  (api:render-activity ui
                                                       gfx
                                                       activity
                                                       (gui:window-x gui-window)
                                                       (gui:window-y gui-window)
                                                       (gui:window-width gui-window)
                                                       (gui:window-height gui-window)
                                                       window-width
                                                       window-height))
                                (values)))))

(defgeneric window-on-render (window &key &allow-other-keys)
  (:method ((window window) &key &allow-other-keys)
           (values))
  (:method ((window gfx-window) &key render-content &allow-other-keys)
           (let* ((gui-window (gfx-window-gui-window window))
                  (gui-layer (when gui-window (gui:window-layer gui-window))))
             (unless (and (eq :sdl-surface (gfx-window-backend-key window))
                          gui-layer
                          (not (gui:layer-dirty gui-layer)))
               (api:gfx-render (gfx-window-backend window)
                               (window-sdl-window window)
                               gui-layer
                               render-content)
               (when gui-layer
                     (setf (gui:layer-dirty gui-layer) nil))
               (when gui-window
                     (let* ((gui-width (floor (gui:window-width gui-window)))
                            (gui-height (floor (gui:window-height gui-window)))
                            (old-gui-width (slot-value window 'checked-gui-window-width))
                            (old-gui-height (slot-value window 'checked-gui-window-height)))
                       (when (or (/= old-gui-width gui-width) (/= old-gui-height gui-height))
                             (setf (slot-value window 'checked-gui-window-width) (gui:window-width gui-window))
                             (setf (slot-value window 'checked-gui-window-height) (gui:window-height gui-window))
                             (%sdl3:set-window-size (window-sdl-window window) gui-width gui-height))))))
           (values))
  (:method :around ((window activities-window) &key render-content &allow-other-keys)
           (assert (null render-content))
           (multiple-value-bind (window-width window-height)
               (sdl3:get-window-size-in-pixels (window-sdl-window window))
             (call-next-method window
                               :render-content (lambda (gfx-ctx)
                                                 (render-activity-tree **app**
                                                                       gfx-ctx
                                                                       window-width
                                                                       window-height
                                                                       (slot-value window 'activities)))))))

(defgeneric window-on-close-requested (window)
  (:method ((window window))
           t))

(defgeneric window-on-pixel-size-changed (window new-px-width new-px-height)
  (:method ((window window) new-px-width new-px-height)
           (values))
  (:method ((window gfx-window) new-px-width new-px-height)
           (let ((gui-window (gfx-window-gui-window window)))
             (when gui-window
                   (when (gui:window-layer gui-window)
                         (setf (gui:layer-dirty (gui:window-layer gui-window)) t))
                   (gui:set-window-layout gui-window
                                          0.0d0
                                          0.0d0
                                          (coerce new-px-width 'double-float)
                                          (coerce new-px-height 'double-float)))
             (api:gfx-resize (gfx-window-backend window)
                             (window-sdl-window window)
                             (when gui-window (gui:window-layer gui-window))
                             new-px-width
                             new-px-height))
           (values)))

(defun sdl-keysym-to-char (key)
  (let ((charcode (%sdl3:get-key-from-scancode (%sdl3:keysym.scancode key))))
    (when (< charcode char-code-limit)
          (code-char charcode))))

(defun sdl-keysym-to-gui-mod (sdl-keysym)
  (let ((sdl-keymod (%sdl3:keysym.mod sdl-keysym))
        (gui-keymod 0))
    (macrolet ((m (sdl-flag gui-flag)
                  `(when (/= 0 (logand sdl-keymod ,sdl-flag))
                         (setf gui-keymod (logior gui-keymod ,gui-flag)))))
      (m sdl3:+kmod-lshift+ gui:+key-modifier-shift-left+)
      (m sdl3:+kmod-rshift+ gui:+key-modifier-shift-right+)
      (m sdl3:+kmod-lctrl+ gui:+key-modifier-ctrl-left+)
      (m sdl3:+kmod-rctrl+ gui:+key-modifier-ctrl-right+)
      (m sdl3:+kmod-lalt+ gui:+key-modifier-alt-left+)
      (m sdl3:+kmod-ralt+ gui:+key-modifier-alt-right+)
      (m sdl3:+kmod-mode+ gui:+key-modifier-alt-gr+)
      (m sdl3:+kmod-lgui+ gui:+key-modifier-gui-left+)
      (m sdl3:+kmod-rgui+ gui:+key-modifier-gui-right+)
      (m sdl3:+kmod-num+ gui:+key-modifier-num-lock+)
      (m sdl3:+kmod-caps+ gui:+key-modifier-caps-lock+)
      (m sdl3:+kmod-scroll+ gui:+key-modifier-scroll-lock+))
    gui-keymod))

(defgeneric window-on-key-down (window event)
  (:method ((window window) event)
           (values))
  (:method ((window gfx-window) event)
           (let ((gui-window (gfx-window-gui-window window)))
             (when gui-window
                   (let* ((keysym (%sdl3:keyboard-event.keysym event))
                          (scancode (%sdl3:keysym.scancode keysym))
                          (keymod (sdl-keysym-to-gui-mod keysym))
                          (keychar (let ((charcode (%sdl3:get-key-from-scancode scancode)))
                                     (when (< charcode char-code-limit)
                                           (code-char charcode))))
                          (keyval
                           (cond
                            ((= scancode %sdl3:+scancode-return+) :enter)
                            ((= scancode %sdl3:+scancode-backspace+) :backspace)
                            ((= scancode %sdl3:+scancode-delete+) :delete)
                            ((= scancode %sdl3:+scancode-up+) :up)
                            ((= scancode %sdl3:+scancode-down+) :down)
                            ((= scancode %sdl3:+scancode-left+) :left)
                            ((= scancode %sdl3:+scancode-right+) :right)
                            ((/= 0 (logior (logand sdl3:+kmod-lctrl+ keymod)))
                              (when (characterp keychar)
                                    (cond
                                     ((char= #\c keychar) :copy)
                                     ((char= #\v keychar) :paste)
                                     (t keychar))))
                            (t keychar))))
                     (gui:emit-key-down (app-ui **app**)
                                        keyval
                                        keymod
                                        event
                                        :layer (gui:window-layer gui-window)))))
           (values)))

(defgeneric window-on-key-up (window event)
  (:method ((window window) event)
           (values))
  (:method ((window gfx-window) event)
           (let ((gui-window (gfx-window-gui-window window)))
             (when gui-window
                   (let* ((keysym (%sdl3:keyboard-event.keysym event))
                          (charcode (%sdl3:get-key-from-scancode (%sdl3:keysym.scancode keysym))))
                     (gui:emit-key-up (app-ui **app**)
                                      (when (< charcode char-code-limit)
                                            (code-char charcode))
                                      (sdl-keysym-to-gui-mod keysym)
                                      event
                                      :layer (gui:window-layer gui-window)))))
           (values)))

(defgeneric window-on-mouse-button (window event)
  (:method ((window window) event)
           (values))
  (:method ((window gfx-window) event)
           (when (gfx-window-gui-window window)
                 (let ((ui (app-ui **app**))
                       (x (%sdl3:mouse-button-event.x event))
                       (y (%sdl3:mouse-button-event.y event)))
                   (if (= (%sdl3:mouse-button-event.type event) %sdl3:+event-mouse-button-down+)
                       (case (%sdl3:mouse-button-event.button event)
                         (#.%sdl3:+button-left+
                          (gui:emit-mouse-down-left ui (gfx-window-gui-layer window) x y event))
                         (#.%sdl3:+button-middle+
                          (gui:emit-mouse-down-middle ui (gfx-window-gui-layer window) x y event))
                         (#.%sdl3:+button-right+
                          (gui:emit-mouse-down-right ui (gfx-window-gui-layer window) x y event)))
                       (case (%sdl3:mouse-button-event.button event)
                         (#.%sdl3:+button-left+
                          (gui:emit-mouse-up-left ui (gfx-window-gui-layer window) x y event))
                         (#.%sdl3:+button-middle+
                          (gui:emit-mouse-up-middle ui (gfx-window-gui-layer window) x y event))
                         (#.%sdl3:+button-right+
                          (gui:emit-mouse-up-right ui (gfx-window-gui-layer window) x y event))))))
           (values)))

(defgeneric window-on-mouse-scroll (window event)
  (:method ((window window) event)
           (values))
  (:method ((window gfx-window) event)
           (when (gfx-window-gui-window window)
                 (gui:emit-mouse-scroll (app-ui **app**)
                                        (%sdl3:mouse-wheel-event.x event)
                                        (%sdl3:mouse-wheel-event.y event)
                                        event))
           (values)))

(defgeneric window-on-mouse-move (window event)
  (:method ((window window) event)
           (values))
  (:method ((window gfx-window) event)
           (when (gfx-window-gui-window window)
                 (gui:emit-mouse-move (app-ui **app**)
                                      (gfx-window-gui-layer window)
                                      (%sdl3:mouse-motion-event.x event)
                                      (%sdl3:mouse-motion-event.y event)
                                      event))
           (values)))

(defun create-compose-window (title compose)
  (let ((window (make-instance 'gfx-window
                  :title title
                  :width 400
                  :height 300
                  :gfx-backend-key :sdl-surface
                  :with-gui t)))
    (gui:compose (app-ui **app**)
                 (gfx-window-gui-window window)
                 (funcall compose (app-ui **app**)
                   (lambda () (destroy-window **app** window))))
    window))

(defun api:message-box (title text buttons)
  (create-compose-window title
                         (lambda (ui close)
                           (gui:macroexpand-with-ui ui
                             (gui:w-visual ((gui:v-rectangle :fill-style (colors-background ui)))
                               (:layout ((flex-direction yogalayout:+flex-direction-column+)
                                         (width-percent 100.0)
                                         (height-percent 100.0)))
                               (gui:w-block ((justify-content yogalayout:+justify-center+)
                                             (align-items yogalayout:+align-center+)
                                             (flex-grow 0.9)
                                             (flex-shrink 0.9))
                                 (gui:w-label :text text
                                              :let widget
                                              :text-style (colors-main ui)
                                              :font-size 32))
                               (gui:w-visual ()
                                 (:layout ((flex-direction yogalayout:+flex-direction-row+)
                                           (justify-content yogalayout:+justify-space-around+)
                                           (align-items yogalayout:+align-center+)
                                           (min-height 60.0)
                                           (flex-grow 0.1)
                                           (flex-shrink 0.1)))
                                 (loop #:for button #:across buttons
                                       #:do
                                       (if (typep button 'message-box-button)
                                           ; (sdet:make-effect (gui:sdet-context ui)
                                           (gui:w-button :ui ui
                                                         :layout ((width 110.0) (height 40.0))
                                                         :on-click ((funcall (message-box-button-on-click button))
                                                                    (funcall close))
                                                         :focus (message-box-button-focus button)
                                                         :let widget
                                                         :visual ((gui:v-rectangle :fill-style (colors-fill-dispatch ui widget)
                                                                                   :border-style (colors-fill-dispatch ui widget)
                                                                                   :border-width 1.0)
                                                                  (gui:v-text :text (message-box-button-text button)
                                                                              :font-size 18
                                                                              :style (colors-main ui))))
                                           ; )
                                           (format t "Invalid alert button ~a" button)))
                                 nil)))))
  (values))

(defun sdl-window (designator)
  (etypecase designator
    (null (window-sdl-window (app-main-window **app**)))
    (window (window-sdl-window designator))
    (cffi:foreign-pointer designator)
    (%sdl3:window designator)
    (integer
     (let ((app-window (app-window **app** designator)))
       (if app-window
           (window-sdl-window app-window)
           (window-sdl-window (app-main-window **app**)))))))

(defun create-popup (app parent-window offset-x offset-y compose-callback)
  (let* ((parent-sdl-window (sdl-window parent-window))
         (window (make-instance 'gfx-window
                   :gfx-backend-key :sdl-surface
                   :flags (logior %sdl3:+window-transparent+ %sdl3:+window-popup-menu+)
                   :parent parent-sdl-window
                   :offset-x offset-x
                   :offset-y offset-y
                   :resize-to-contents-x t
                   :resize-to-contents-y t
                   :with-ui t)))
    (gui:compose (app-ui app)
                 (gfx-window-gui-window window)
                 (funcall compose-callback
                   (app-ui app)
                   (lambda ()
                     (destroy-window app window)
                     (values))))
    window))

(defun api:context-menu (parent-window x y menu-items-computed)
  (let* ((app **app**)
         (window (create-popup app
                               parent-window
                               (floor x)
                               (floor y)
                               (lambda (ui close)
                                 (w-menu-list ui
                                              menu-items-computed
                                              close)))))
    (vector-push-extend (window-sdl-id window) (app-active-popups app)))
  (values))

(defun api:create-tooltip (parent-window x y text)
  (create-popup **app**
                parent-window
                (floor x)
                (floor y)
                (lambda (ui close)
                  (declare (ignore close))
                  (gui:w-visual ((gui:v-rectangle :fill-style #xBB000000))
                    (:ui ui :layout ((max-width 300.0)
                                     (padding yogalayout:+edge-all+ 5.0)))
                    (gui:w-label :ui ui :text text :font-size 12.0)))))

(defun api:move-tooltip (window x y)
  (%sdl3:set-window-position window (floor x) (floor y))
  (values))

(defun api:destroy-tooltip (window)
  (destroy-window **app** window)
  (values))

(defun relative-mouse-position (window)
  (multiple-value-bind (mouse-x mouse-y)
      (sdl3:get-global-mouse-state)
    (multiple-value-bind (window-x window-y)
        (sdl3:get-window-position (if (typep window 'window) (window-sdl-window window) window))
      (values (- mouse-x window-x) (- mouse-y window-y)))))
