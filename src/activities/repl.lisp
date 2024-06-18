(in-package #:my-editor)

(defclass repl-activity-factory (api:activity-factory)
    ((api:key :initform :repl)
     (api:path :initform #("General" "REPL"))))

(defclass repl-activity (api:activity)
    ((history :initform (make-array 0 :adjustable t :fill-pointer 0)
              :type (vector string))
     (output :initform (make-observable-vector (api:sdet-context))
             :type observable-vector)
     (get-input :type (function () (values string &optional)))
     (set-input :type (function (string) (values &optional)))
     (history-index :initform nil
                    :type (or null fixnum))
     (saved-input :initform nil
                  :type (or null string))))

(defmethod initialize-instance :after ((activity repl-activity) &key &allow-other-keys)
  (multiple-value-bind (get-input set-input)
      (sdet:make-state (api:sdet-context) "")
    (setf (slot-value activity 'get-input) get-input)
    (setf (slot-value activity 'set-input) set-input))
  (values))

(defmethod api:create-activity ((factory repl-activity-factory))
  (make-instance 'repl-activity
    :factory factory
    :title "REPL"))

(defmethod api:compose-activity-ui (ui (activity repl-activity))
  (with-slots (history history-index get-input set-input saved-input output) activity
    (gui:macroexpand-with-ui ui
      (gui:w-visual ((gui:v-rectangle :fill-style (api:colors-background ui)))
        (:layout ((padding yogalayout:+edge-all+ 6.0)))
        (gui:w-scroll ((flex 1.0)) (:axis :y :background-visual ())
          (gui:w-map-fix-index (progn
                                (funcall (observable-vector-subscribe output))
                                (observable-vector-values output))
            (get-val nil)
            (gui:w-label :font-size 15.0
                         :text (funcall get-val))))
        (let ((textbox (gui:w-textbox :layout ((height 26.0)
                                               (margin yogalayout:+edge-all+ 6.0))
                                      :let textbox
                                      :on-enter ((let* ((input (funcall get-input))
                                                        (results-initial-length (+ (length input) 3))
                                                        (results (make-array results-initial-length :adjustable t :fill-pointer results-initial-length :element-type 'character)))
                                                   (setf history-index nil)
                                                   (setf saved-input nil)
                                                   (vector-push-extend input history)
                                                   (funcall set-input "")
                                                   (setf (aref results 0) #\>)
                                                   (setf (aref results 1) #\Space)
                                                   (setf results (replace results input :start1 2))
                                                   (setf (aref results (1- results-initial-length)) #\Newline)
                                                   (with-output-to-string (results-stream results)
                                                     (dolist (res (multiple-value-list (eval (read-from-string input))))
                                                       (format results-stream "~S~%" res)))
                                                   (vector-push-extend results (observable-vector-values output))
                                                   (funcall (observable-vector-notify output))))
                                      :on-changed (new-text (funcall set-input new-text))
                                      :background-visual ((gui:v-rectangle :fill-style (api:colors-input ui)
                                                                           :border-style (api:colors-border-dispatch ui textbox)
                                                                           :border-width 1.0))
                                      :cursor-visual ((gui:v-rectangle :fill-style #xFF4F3311))
                                      :text (funcall get-input)
                                      :font :plex-mono
                                      :font-size 15.0
                                      :text-style (api:colors-input-content ui)
                                      :padding 2.0d0)))
          (let ((on-text-input (gui:widget-on-text-input textbox)))
            (setf (gui:widget-on-text-input textbox)
              (lambda (ui widget text)
                (when history-index
                      (setf history-index 0)
                      (setf saved-input nil))
                (funcall on-text-input ui widget text))))))))
  (values))

(defmethod api:on-key-action (ui (activity repl-activity) action mod key)
  (with-slots (history history-index get-input set-input saved-input) activity
    (let ((update-input nil)
          (handled nil))
      (case action
        (:up
         (if history-index
             (when (< (1+ history-index) (length history))
                   (setf
                     history-index (1+ history-index) update-input t))
             (when (> (length history) 0)
                   (setf saved-input (sdet:unobserved (api:sdet-context) (funcall get-input)))
                   (setf history-index 0 update-input t)))
         (setf handled t))
        (:down
         (when history-index
               (decf history-index)
               (if (< history-index 0)
                   (progn
                    (setf history-index nil)
                    (funcall set-input saved-input))
                   (setf update-input t)))
         (setf handled t)))
      (when update-input
            (funcall set-input (aref history (- (length history) 1 history-index))))
      handled)))

; (defstruct (repl (:copier nil)
;                  (:predicate nil))
;   * ** ***
;   -)
;; REPL TODO:
; Global repl state saved in app (?)
; Input history
; Error handling with restart selection
; * ** *** + ++ +++ / // /// - (binded via "let ((* (repl-* (app-repl app))))")
; Special variables to refere to some state: window, activity, time, UI, sdet-context etc
; Output should be limited (ring buffer)
; Use lazy/virtual scroll
; Re-route stdout/stderr, stderr colored, print them in correct(!) interleaved order
; Print input
