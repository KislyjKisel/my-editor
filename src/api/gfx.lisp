(in-package #:my-editor/api)

(export 'gfx-initialize)
(declaim (ftype (function (t boolean
                             &key
                             (:window-title (or null string))
                             (:window-width (signed-byte 32))
                             (:window-height (signed-byte 32))
                             (:window-flags (signed-byte 32))
                             (:window-parent (or null t cffi:foreign-pointer))
                             (:window-offset-x (or null (signed-byte 32)))
                             (:window-offset-y (or null (signed-byte 32))))
                          (values t cffi:foreign-pointer (or null layer) &optional))
                gfx-initialize))
(defgeneric gfx-initialize (backend-key with-ui
                                        &key
                                        window-title window-width window-height window-flags
                                        window-parent window-offset-x window-offset-y))

(export 'gfx-resize)
(defgeneric gfx-resize (state window layer px-width px-height))

(export 'gfx-render)
(defgeneric gfx-render (state window gui-layer render-content))

(export 'gfx-terminate)
(defgeneric gfx-terminate (state window))
