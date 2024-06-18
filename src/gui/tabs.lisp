(in-package #:my-editor)

(defun w-tabs-impl (ui items-computed selected-index set-selected-index
                       &key
                       z-index position-type
                       set-container-layout
                       set-header-layout
                       header-visual
                       tab-header-item
                       tab-contents
                       equal
                       initialize-header)
  (let ((sdet-ctx (gui:sdet-context ui)))
    (let ((item-computed (sdet:make-computed sdet-ctx
                           (let ((items (sdet:compute items-computed)))
                             (unless (= 0 (length items))
                               (let ((index (funcall selected-index)))
                                 (when (<= 0 index (1- (length items)))
                                       (aref items (funcall selected-index)))))))))
      (gui:w-block* 'set-container-layout (:ui ui :z-index 'z-index :position-type 'position-type)
        (gui:w-scroll 'set-header-layout (:ui ui
                                              :axis :x
                                              :let header
                                              :background-visual 'header-visual
                                              :bar-width-base 3.0d0
                                              :bar-width-scale 0.0d0
                                              :thumb-width-base 3.0d0
                                              :thumb-width-scale 0.0d0
                                              :thumb-visual ((gui:v-rectangle :fill-style #xFF999999)))
          (gui:w-map-fix-index 'items-computed (get-val index :ui ui :equal equal)
            (funcall tab-header-item get-val index selected-index set-selected-index))
          (funcall initialize-header header))
        (gui:w-dynamic* ui
                        (lambda ()
                          (funcall tab-contents (funcall item-computed))))
        ; (let ((placeholder (gui:insert-placeholder ui)))
        ;   (sdet:make-effect sdet-ctx
        ;     (gui:with-composition-after-placeholder ui placeholder
        ;       (funcall tab-contents (funcall item-computed))))
        ;   (lambda ()
        ;     (gui:delete-placeholder placeholder)
        ;     (values)))
        ))))

(export 'w-tabs)
(defmacro w-tabs (items selected-index set-selected-index
                        (&key container-layout header-layout header-visual ui z-index position-type (equal '#'eql) initialize-header)
                        (hi-item-sym index-sym get-selected-index-sym set-selected-index-sym &body header-item)
                        (cs-item-sym &body contents))
  (gui:macroexpand-with-ui* ui
    `(w-tabs-impl ,gui:*ui*
                  ,(gui:make-computed-prop items :initialized nil)
                  ,selected-index
                  ,set-selected-index
                  :z-index ,(gui:make-computed-prop z-index)
                  :position-type ,(gui:make-computed-prop position-type)
                  :set-container-layout ,(gui:make-layout-setting-lambda gui:*ui* container-layout)
                  :set-header-layout ,(gui:make-layout-setting-lambda gui:*ui* header-layout)
                  :header-visual ,(gui:make-visual-prop ui header-visual)
                  :tab-header-item (lambda (,hi-item-sym ,index-sym ,get-selected-index-sym ,set-selected-index-sym)
                                     ,@header-item)
                  :tab-contents (lambda (,cs-item-sym)
                                  ,@contents)
                  :equal ,equal
                  :initialize-header ,initialize-header)))
