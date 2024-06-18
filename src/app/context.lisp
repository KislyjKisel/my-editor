(in-package #:my-editor)

(global-vars:define-global-var **app** nil)

(defconstant +messages-capacity+ 1000)

(defstruct (app (:copier nil)
                (:constructor create-app (&aux
                                          (ui (gui:create-ui))
                                          (subscribe-notify-messages (multiple-value-list (sdet:make-notifier (gui:sdet-context ui))))
                                          (subscribe-messages (first subscribe-notify-messages))
                                          (notify-messages (second subscribe-notify-messages)))))
  (ui)
  (quit nil :type boolean)
  (project nil)
  (activity-factories (make-activity-factory-tree nil) :type activity-factory-tree)
  (windows (make-hash-table) :type hash-table)
  (last-sdl-cursor-id nil :type (or null fixnum))
  (sdl-cursor nil :type (or null %sdl3:cursor))
  (main-window-id nil :type (or null (unsigned-byte 32)))
  (non-gui-windows 0 :type fixnum)
  (dragged-window nil :type (or null %sdl3:window))
  (on-drag-stop nil :type (or null function))
  (active-popups (make-array 0 :adjustable t :fill-pointer 0 :element-type '(unsigned-byte 32)) :type (vector (unsigned-byte 32)))
  (messages (make-array +messages-capacity+) :type simple-vector)
  (message-categories (make-array +messages-capacity+ :initial-element :debug :element-type 'symbol) :type simple-vector)
  (messages-start 0 :type fixnum)
  (messages-end 0 :type fixnum)
  (messages-empty t :type boolean)
  (messages-count (make-hash-table) :type hash-table)
  (subscribe-messages (gui::unreachable))
  (notify-messages (gui::unreachable)))

(defun app-window (app id)
  (gethash id (app-windows app)))

(defun app-main-window (app)
  (app-window app (app-main-window-id app)))

(defun sdet-context (app)
  (gui:sdet-context (app-ui app)))

(defun api:project-directory ()
  (let ((project (app-project **app**)))
    (when project
          (make-pathname :directory (project-directory project)))))

(defun destroy-app (app)
  (alexandria:maphash-values (lambda (sdl-window) (destroy-window app sdl-window)) (app-windows app))
  ; (gui:destroy-cursor-renderer (app-cursor-renderer app))
  (when (app-sdl-cursor app)
        (%sdl3:destroy-cursor (app-sdl-cursor app)))
  (gui:map-fonts (app-ui app) (lambda (key font-face)
                                (declare (ignore key))
                                ;; Fonts may appear multiple times under different keys, don't double-free them
                                (when (autowrap:valid-p font-face)
                                      (%blend2d:font-face-destroy font-face)
                                      (autowrap:free font-face))
                                (values)))
  (gui:destroy-ui (app-ui app))
  (values))

(defun load-font (app key data-file-suffix)
  (let ((font (autowrap:alloc '%blend2d:font-face-core)))
    (%blend2d:font-face-init font)
    (%blend2d:font-face-create-from-file font
                                         (namestring (merge-pathnames (concatenate 'string "data/fonts/" data-file-suffix) (deploy:data-directory)))
                                         0)
    (gui:set-font (app-ui app) key font))
  (values))

(defun set-default-font (app key)
  (multiple-value-bind (font-face present)
      (gui:get-font (app-ui app) key)
    (unless present (error "Font ~s not found when trying to set it to be the default font" key))
    (gui:set-font (app-ui app) nil font-face))
  (values))

(defun find-activity-factory (app key &key (node nil node-supplied-p))
  (unless node-supplied-p
    (setf node (app-activity-factories app)))
  (let ((leaf (activity-factory-tree-value node)))
    (when (and leaf
               (eq key (api:activity-factory-key leaf)))
          (return-from find-activity-factory leaf)))
  (loop #:for child #:across (activity-factory-tree-children node)
        #:do (let ((res (find-activity-factory app key :node child)))
               (when res (return-from find-activity-factory res))))
  nil)

(defun api:start-dragging (value on-drop on-stop &key (parent-window nil))
  (%sdl3:capture-mouse sdl3:+false+)
  (let* ((app **app**)
         (parent-sdl-window (sdl-window parent-window))
         (window (multiple-value-bind (rmx rmy)
                     (relative-mouse-position parent-sdl-window)
                   (%sdl3:create-popup-window parent-sdl-window
                                              (floor (+ +dragged-window-offset-x+ rmx))
                                              (floor (+ +dragged-window-offset-y+ rmy))
                                              32 32
                                              (logior
                                                %sdl3:+window-tooltip+
                                                %sdl3:+window-transparent+)))))
    (let ((surface (%sdl3:get-window-surface window)))
      (%sdl3:fill-surface-rect surface (cffi:null-pointer) #xFFFFBB66)
      (%sdl3:update-window-surface surface))
    (setf (app-dragged-window app) window)
    (setf (app-on-drag-stop app) on-stop)
    (gui:start-dragging (app-ui app) value on-drop)))

(defun render-app (app)
  (loop #:for window #:being #:the #:hash-value #:of (app-windows app)
        #:when (and (typep window 'gfx-window) (not (eq :sdl-surface (gfx-window-backend-key window))))
        #:do
        (let ((gui-window (gfx-window-gui-window window)))
          (when gui-window
                (setf (gui::layer-dirty (gui:window-layer gui-window)) t))))
  (gui:render (app-ui app)
              (/ (coerce (%sdl3:get-performance-counter) 'double-float)
                 (coerce (%sdl3:get-performance-frequency) 'double-float)))
  (alexandria:maphash-values #'window-on-render (app-windows app))
  (let* ((cursor (gui:cursor (app-ui app)))
         (new-sdl-cursor-id (case cursor
                              (:none nil)
                              (:default %sdl3:+system-cursor-default+)
                              (:text %sdl3:+system-cursor-text+)
                              ((:press :release) %sdl3:+system-cursor-pointer+)
                              (:grab %sdl3:+system-cursor-pointer+)
                              (:move %sdl3:+system-cursor-move+)
                              (:move-x %sdl3:+system-cursor-ew-resize+)
                              (:move-y %sdl3:+system-cursor-ns-resize+)
                              (:zoom-in %sdl3:+system-cursor-default+)
                              (:zoom-out %sdl3:+system-cursor-default+)
                              (:progress %sdl3:+system-cursor-progress+)
                              (:forbidden %sdl3:+system-cursor-not-allowed+)
                              (t %sdl3:+system-cursor-default+))))
    (unless (eql (app-last-sdl-cursor-id app) new-sdl-cursor-id)
      (let ((old-sdl-cursor (app-sdl-cursor app)))
        (setf (app-sdl-cursor app) (when new-sdl-cursor-id (%sdl3:create-system-cursor new-sdl-cursor-id)))
        (when (app-sdl-cursor app)
              (%sdl3:set-cursor (app-sdl-cursor app)))
        (when old-sdl-cursor
              (%sdl3:destroy-cursor old-sdl-cursor)))
      (setf (app-last-sdl-cursor-id app) new-sdl-cursor-id)))
  (values))

(defun api:sdet-context ()
  (sdet-context **app**))

(defun api:recompose-activity (activity)
  (let ((ui (app-ui **app**)))
    (gui:compose ui
                 (slot-value activity 'api::gui-window)
                 (api:compose-activity-ui ui activity)))
  (values))

(defun api:map-activities (f)
  (alexandria:maphash-values (lambda (window)
                               (when (typep window 'activities-window)
                                     (map-activity-tree-leaves (sdet-context **app**)
                                                               (slot-value window 'activities)
                                                               f)))
                             (app-windows **app**))
  (values))

(defun api:open-inspector (value on-value-forgotten)
  (let ((inspectors (make-array 0 :adjustable t :fill-pointer 0)))
    (api:map-activities (lambda (activity)
                          (when (and (typep activity 'inspector-activity)
                                     (not (slot-value activity 'locked)))
                                (vector-push-extend activity inspectors)
                                (set-inspector-activity-value activity
                                                              value
                                                              (lambda ()
                                                                (setf inspectors (gui::vector-delete activity inspectors))
                                                                (when (= 0 (length inspectors))
                                                                      (funcall on-value-forgotten value))
                                                                (values))))
                          (values)))
    (when (< 0 (length inspectors))
          (lambda ()
            (loop #:for inspector #:across inspectors
                  #:do (reset-inspector inspector))
            value))))

(defun api:message (category text)
  (let* ((app **app**)
         (start (app-messages-start app))
         (end (app-messages-end app))
         (next-end (1+ end)))
    (when (= next-end +messages-capacity+)
          (setf next-end 0))
    (when (and (not (app-messages-empty app))
               (= start end))
          (decf (gethash (svref (app-message-categories app) start) (app-messages-count app)))
          (setf (app-messages-start app) next-end))
    (setf (svref (app-messages app) end) text)
    (setf (svref (app-message-categories app) end) category)
    (incf (gethash category (app-messages-count app)))
    (setf (app-messages-end app) next-end)
    (setf (app-messages-empty app) nil)
    (funcall (app-notify-messages app)))
  (values))

(defun clear-messages ()
  (let ((app **app**))
    (when (app-messages-empty app)
          (return-from clear-messages (values)))
    (setf (app-messages-start app) 0)
    (setf (app-messages-end app) 0)
    (setf (app-messages-empty app) t)
    (loop #:for i #:below +messages-capacity+
          #:do (setf (svref (app-messages app) i) nil))
    (loop #:for category #:in +message-categories+
          #:do (setf (gethash category (app-messages-count app)) 0))
    (funcall (app-notify-messages app)))
  (values))