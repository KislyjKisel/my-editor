(in-package #:my-editor)

(export '(thread-barrier make-thread-barrier))
(declaim (ftype (function ((integer 1) &key (:name (or null string))) (values thread-barrier &optional)) make-thread-barrier))
(defstruct (thread-barrier (:copier nil)
                           (:constructor make-thread-barrier (count
                                                                 &key name
                                                               &aux
                                                               (lock (bt2:make-lock :name name))
                                                               (condvar (bt2:make-condition-variable :name name)))))
  (in 0 :type (integer 0))
  (out 0 :type (integer 0))
  (count (gui::unreachable) :type integer)
  (lock (gui::unreachable) :type bt2:lock)
  (condvar (gui::unreachable) :type bt2:condition-variable))

(export 'thread-barrier-change-count)
(declaim (ftype (function (thread-barrier &key (:delta integer)) (values &optional)) thread-barrier-change-count))
(defun thread-barrier-change-count (barrier &key (delta 1))
  (bt2:with-lock-held ((thread-barrier-lock barrier))
    (incf (thread-barrier-count barrier) delta)
    (check-type (thread-barrier-count barrier) (integer 1)))
  (values))

(export 'thread-barrier-wait)
(declaim (ftype (function (thread-barrier) (values &optional)) thread-barrier-wait))
(defun thread-barrier-wait (barrier)
  (bt2:with-lock-held ((thread-barrier-lock barrier))
    (let ((participant-number (incf (thread-barrier-in barrier))))
      (if (>= participant-number (+ (thread-barrier-out barrier) (thread-barrier-count barrier)))
          (progn
           (incf (thread-barrier-out barrier) (thread-barrier-count barrier))
           (bt2:condition-broadcast (thread-barrier-condvar barrier)))
          (loop #:while (> participant-number (thread-barrier-out barrier))
                #:do
                (bt2:condition-wait (thread-barrier-condvar barrier) (thread-barrier-lock barrier))))))
  (values))
