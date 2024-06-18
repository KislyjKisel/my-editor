(in-package #:my-editor/api)

(define-event #:post-sdl-init)
(define-event #:pre-sdl-quit)

(export 'project-directory)
(declaim
  (notinline project-directory)
  (ftype (function () (values (or null pathname) &optional)) project-directory))
