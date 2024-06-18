(in-package #:my-editor/api)

(export 'register-command)
(declaim
  (notinline register-command)
  (ftype (function (symbol (function () (values (or null (function () (values &optional))) &optional)))
                   (values &optional))
         register-command))

(export 'perform-command)
(declaim
  (notinline perform-command)
  (ftype (function (symbol) (values &optional)) perform-command))
