(in-package #:my-editor/api)

(defmacro define-event (event-name &rest args)
  (let ((var (alexandria:symbolicate '#:* event-name '#:-handlers*))
        (handler (gensym "handler"))
        (arg-names (mapcar (lambda (arg) (first arg)) args))
        (arg-types (mapcar (lambda (arg) (second arg)) args))
        (sub (alexandria:symbolicate '#:subscribe- event-name))
        (unsub (alexandria:symbolicate '#:unsubscribe- event-name))
        (invoke (alexandria:symbolicate '#:invoke- event-name)))
    `(progn
      (defvar ,var (make-hash-table :test #'eq))
      (declaim (ftype (function ,arg-types (values &optional)) ,invoke))
      (defun ,invoke ,arg-names
        (alexandria:maphash-keys (lambda (,handler) (funcall ,handler . ,arg-names)) ,var)
        (values))
      (export ',sub)
      (declaim (ftype (function ((function ,arg-types (values &optional))) (values &optional)) ,sub))
      (defun ,sub (,handler)
        (setf (gethash ,handler ,var) nil)
        (values))
      (export ',unsub)
      (declaim (ftype (function ((function ,arg-types (values &optional))) (values &optional)) ,unsub))
      (defun ,unsub (,handler)
        (remhash ,handler ,var)
        (values)))))
