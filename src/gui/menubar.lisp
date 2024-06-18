; (in-package #:my-editor)

; (defstruct (menubar-widget (:include gui:widget)
;                            (:copier nil)
;                            (:constructor make-menubar-widget (&aux
;                                                               (focus-behavior-as-parent-x :passthrough)
;                                                               (focus-behavior-as-parent-y :passthrough))))
;   )
; ;; TODO window decorations visuals: 
; ;; title text ("<project-key> - my editor" ?), close, minimize, maximize,
; ;; plus a bunch of labels + menu system (nested tree of labels, data defined)

; (defun w-menubar-impl (ui &key
;                         set-layout
;                         make-children
;                         z-index position-type)
;   (let ((widget (make-menubar-widget)))
;     (initialize-widget ui widget :z-index z-index :position-type position-type)
;     (when set-layout (funcall set-layout widget))
;     (when make-children (append-children ui widget make-children))
;     (lambda ()
;       (destroy-widget widget)
;       (values))))

; (export 'w-menubar*)
; (defmacro w-menubar* (layout (&key ui let z-index position-type)
;                            &body children)
;   (macroexpand-with-ui ui
;     `(w-menubar-impl ,*ui*
;                    :set-layout ,(make-layout-setting-lambda *ui* layout)
;                    :make-children ,(make-children-making-lambda *ui* children :let let)
;                    :z-index ,(make-computed-prop z-index :let let)
;                    :position-type ,(make-computed-prop position-type :let let))))
