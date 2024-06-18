(in-package #:my-editor)

(defconstant +gfx-default-backend-key+ :sdl-surface)

(defun with-sdl (flags f)
  (let (error-code)
    (setf error-code
      (%sdl3:init flags))
    (when (/= 0 error-code)
          (format t "SDL initialization error (~a): ~a" error-code (%sdl3:get-error))
          (return-from with-sdl))
    (prog1
        (funcall f)
      (%sdl3:quit))))

(defconstant +dragged-window-offset-x+ 14)
(defconstant +dragged-window-offset-y+ 11)

(alexandria:define-constant +message-categories+ '(:debug :info :warning :error) :test #'equalp)

(defun main-loop (project)
  (with-sdl
    (logior %sdl3:+init-events+
      %sdl3:+init-video+
      (project-sdl-init-flags project))
    (lambda ()
      (api::invoke-post-sdl-init)
      (let* ((app (create-app))
             (event (autowrap:alloc '%sdl3:event)))
        (loop #:for category #:in +message-categories+
              #:do (setf (gethash category (app-messages-count app)) 0))
        (setf (app-project app) project)
        (setf **app** app)
        (gui:set-cursor-visibility (app-ui app) nil)
        (gui:set-value (app-ui app) api::+ui-value-colors+ (api::make-colors (sdet-context app)))
        (gui:set-text-input-started-handler (app-ui app)
                                            (lambda ()
                                              (%sdl3:start-text-input)
                                              (values)))
        (gui:set-text-input-finished-handler (app-ui app)
                                             (lambda ()
                                               (%sdl3:stop-text-input)
                                               (values)))
        (gui:set-copy-handler (app-ui app)
                              (lambda (text)
                                (%sdl3:set-clipboard-text text)
                                (values)))
        (gui:set-paste-handler (app-ui app)
                               (lambda ()
                                 (when (= %sdl3:+true+ (%sdl3:has-clipboard-text))
                                       (%sdl3:get-clipboard-text))))
        (load-font app :montserrat "Montserrat.ttf")
        (load-font app :plex-mono "IBMPlexMono.ttf")
        (set-default-font app :montserrat)
        (api:register-activity-factory (make-instance 'messages-activity-factory))
        (api:register-activity-factory (make-instance 'repl-activity-factory))
        (api:register-activity-factory (make-instance 'inspector-activity-factory))
        (api:register-activity-factory (make-instance 'file-browser-activity-factory))
        (funcall (project-register project))
        (make-instance 'activities-window :gfx-backend-key (project-default-gfx-backend-key project))
        ; (let ((w (create-gfx-window app +title+ 800 600 (or (project-default-gfx-backend-key project) +gfx-default-backend-key+) t)))
        ;   (setf (sdl-window-activities w)
        ;     (make-activity-tree-node sdet-ctx :x
        ;                              (make-activity-tree-node sdet-ctx :y
        ;                                                       (make-observable-vector sdet-ctx (vector (create-activity app (find-activity-factory app :scene))))
        ;                                                       (make-observable-vector sdet-ctx (vector (create-activity app (find-activity-factory app :file-browser)) (create-activity app (find-activity-factory app :repl)))))
        ;                              (make-observable-vector sdet-ctx (vector (create-activity app (find-activity-factory app :inspector))))))
        ;   (gui:compose (app-ui app)
        ;                (gfx-window-gui-window w)
        ;                (gui:macroexpand-with-ui (app-ui app)
        ;                  (w-activity-tree-node (app-ui app)
        ;                                        (lambda () (sdl-window-activities w))
        ;                                        (lambda (widget)
        ;                                          (let ((ygn (gui:widget-yoga-node widget)))
        ;                                            (yogalayout:node-style-set-width-percent ygn 100.0)
        ;                                            (yogalayout:node-style-set-height-percent ygn 100.0)))))))
        ; (api:alert "Hi" "Hello" (vector (api:make-alert-button "Ok" (lambda () (format t "Nihao!~%") (values)) :focus t)))
        (loop
         (when (app-quit app) (return))
         (when (and (= 0 (app-non-gui-windows app))
                    (= %sdl3:+false+ (%sdl3:has-events %sdl3:+event-first+ %sdl3:+event-last+)))
               (%sdl3:delay 3))
         (loop #:while (/= 0 (%sdl3:poll-event event))
               #:do
               (case (%sdl3:event.type event)
                 (#.%sdl3:+event-quit+
                  (when (window-on-close-requested (app-main-window app))
                        (setf (app-quit app) t)
                        (return)))
                 (#.%sdl3:+event-window-close-requested+
                  (let* ((quit-window-id (%sdl3:event.window.window-id event))
                         (quit-window (app-window app quit-window-id)))
                    (if (= quit-window-id (app-main-window-id app))
                        (let ((other-activities-window nil))
                          (maphash (lambda (id window)
                                     (when (and (null other-activities-window)
                                                (typep window 'activities-window)
                                                (/= id quit-window-id))
                                           (setf other-activities-window id))
                                     (values))
                                   (app-windows app))
                          (unless other-activities-window
                            (setf (app-quit app) t)
                            (return))
                          (when (and quit-window (window-on-close-requested quit-window))
                                (destroy-window app quit-window))
                          (setf (app-main-window-id app) other-activities-window))
                        (when (and quit-window (window-on-close-requested quit-window))
                              (destroy-window app quit-window)))))
                 (#.%sdl3:+event-window-exposed+
                  (setf (window-visible (app-main-window app)) t))
                 (#.%sdl3:+event-window-hidden+
                  (setf (window-visible (app-main-window app)) nil))
                 (#.%sdl3:+event-window-minimized+
                  (setf (window-visible (app-main-window app)) nil))
                 (#.%sdl3:+event-window-restored+
                  (setf (window-visible (app-main-window app)) t))
                 (#.%sdl3:+event-window-pixel-size-changed+
                  (let ((window (app-window app (%sdl3:event.window.window-id event))))
                    (when window
                          (window-on-pixel-size-changed window
                                                        (%sdl3:event.window.data1 event)
                                                        (%sdl3:event.window.data2 event)))))
                 (#.%sdl3:+event-key-down+
                  (let ((window (app-window app (%sdl3:event.key.window-id event))))
                    (when window
                          (window-on-key-down window (%sdl3:event.key event)))))
                 (#.%sdl3:+event-key-up+
                  (let ((window (app-window app (%sdl3:event.key.window-id event))))
                    (when window
                          (window-on-key-up window (%sdl3:event.key event)))))
                 (#.%sdl3:+event-window-destroyed+
                  (let ((window (app-window app (%sdl3:event.window.window-id event))))
                    (when window
                          (destroy-window app window))))
                 (#.%sdl3:+event-mouse-motion+
                  ; (format t "MM win=~A x=~A xr=~A ~%"
                  ;   (%sdl3:event.motion.window-id event)
                  ;   (%sdl3:event.motion.x event)
                  ;   (%sdl3:event.motion.xrel event)
                  ;  )
                  (when (app-dragged-window app)
                        (%sdl3:set-window-position (app-dragged-window app)
                                                   (floor (+ +dragged-window-offset-x+
                                                             (%sdl3:event.motion.x event)))
                                                   (floor (+ +dragged-window-offset-y+
                                                             (%sdl3:event.motion.y event)))))
                  (let ((window (app-window app (%sdl3:event.motion.window-id event))))
                    (when window
                          (window-on-mouse-move window (%sdl3:event.motion event)))))
                 (#.%sdl3:+event-mouse-wheel+
                  (let ((window (app-window app (%sdl3:event.wheel.window-id event))))
                    (when window
                          (window-on-mouse-scroll window (%sdl3:event.wheel event)))))
                 ;  (#.%sdl3:+event-window-focus-lost+
                 ;   (let ((window (app-window app (%sdl3:event.window.window-id event))))
                 ;     (when window
                 ;           (when (/= 0 (logand %sdl3:+window-popup-menu+
                 ;                         (%sdl3:get-window-flags (window-sdl-window window))))
                 ;                 (destroy-window app window)))))
                 ((#.%sdl3:+event-mouse-button-down+ #.%sdl3:+event-mouse-button-up+)
                  (when (= %sdl3:+event-mouse-button-down+ (%sdl3:event.type event))
                        (let ((target-id (%sdl3:event.button.window-id event))
                              (is-target-popup nil))
                          (loop #:for popup-id #:across (app-active-popups app)
                                #:do (if (= popup-id target-id)
                                         (setf is-target-popup t)
                                         (destroy-window app (app-window app popup-id))))
                          (if is-target-popup
                              (progn
                               (setf (fill-pointer (app-active-popups app)) 1)
                               (setf (aref (app-active-popups app) 0) target-id))
                              (setf (fill-pointer (app-active-popups app)) 0)))
                        ; (loop #:with index = 0
                        ;       #:while (< index (length (app-active-popups app)))
                        ;       #:for id = (aref (app-active-popups app) (1- (fill-pointer (app-active-popups app))))
                        ;       #:do
                        ;       (if (= id (%sdl3:event.button.window-id event))
                        ;         (incf index)
                        ;         ()
                        ;         ))
                        ; (loop #:while (/= 0 (fill-pointer (app-active-popups app)))
                        ;       #:for id = (aref (app-active-popups app) (1- (fill-pointer (app-active-popups app))))
                        ;       #:while (/= id (%sdl3:event.button.window-id event))
                        ;       #:do
                        ;       (destroy-window app (app-window app id)))
                        )
                  ; (when (and (= %sdl3:+event-mouse-button-down+ (%sdl3:event.type event))
                  ;            (= %sdl3:+button-right+ (%sdl3:event.button.button event))
                  ;            (app-window app (%sdl3:event.button.window-id event)))
                  ;       (create-popup app
                  ;                     (app-window app (%sdl3:event.button.window-id event))
                  ;                     (floor (%sdl3:event.button.x event))
                  ;                     (floor (%sdl3:event.button.y event))
                  ;                     (lambda (ui close)
                  ;                       ; (gui:w-label :ui ui :text "Hello world")
                  ;                       (w-menu-list ui
                  ;                                    (vector
                  ;                                      "Hello"
                  ;                                      nil
                  ;                                      (make-menu-item-button "Click me" (lambda ()
                  ;                                                                          (format t "Hello world~%")))
                  ;                                                                  (make-menu-item-category "Cats" (vector "Meaow" "Mewt" (make-menu-item-category "Nani" #("Hi" "Hello")))))
                  ;                                    close)
                  ;                       ; (gui:macroexpand-with-ui ui
                  ;                       ;   (gui:w-visual ((gui:v-rectangle :fill-style #x9900ACAC :border-style #xFFAA5511 :border-width 1.0))
                  ;                       ;     (:layout ((width 32.0) (height 32.0)))))
                  ;                       )))
                  ;; just for testing
                  ; (when (= %sdl3:+event-mouse-button-down+ (%sdl3:event.type event))
                  ;       (start-dragging app nil nil))
                  (when (= %sdl3:+event-mouse-button-up+ (%sdl3:event.type event))
                        (when (app-dragged-window app)
                              (%sdl3:destroy-window (app-dragged-window app)))
                        (unless (gui:stop-dragging (app-ui app))
                          (when (app-on-drag-stop app)
                                (funcall (app-on-drag-stop app))))
                        (setf (app-dragged-window app) nil)
                        (setf (app-on-drag-stop app) nil))
                  (let ((window (app-window app (%sdl3:event.button.window-id event))))
                    (when window
                          (window-on-mouse-button window (%sdl3:event.button event)))))
                 (#.%sdl3:+event-text-input+
                  (gui:emit-text-input (app-ui app)
                                       (cffi:foreign-string-to-lisp (%sdl3:event.text.text event))))))
         (when (app-quit app) (return))
         (gui:update-cursor (app-ui app))
         (render-app app))
        (autowrap:free event)
        (destroy-app app)
        (api::invoke-pre-sdl-quit))))
  (values))
