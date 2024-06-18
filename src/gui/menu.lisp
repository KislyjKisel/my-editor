(in-package #:my-editor)

(defconstant +context-menu-font-size+ 14.0)

(defun w-menu-item (ui item close-menu set-next reset-next &key (font-size +context-menu-font-size+))
  (etypecase item
    (null (gui:w-visual ((gui:v-rectangle :fill-style (api:colors-border ui)))
            (:ui ui :layout ((height 0.8)))))
    (string (gui:w-label :ui ui
                         :text item
                         :font-size font-size
                         :layout ((margin yogalayout:+edge-all+ 3.0))))
    (api:menu-item-category
     (gui:w-visual ((gui:v-rectangle :fill-style (gui:wsd widget
                                                   (:hover (api:colors-fill-hover ui))
                                                   (:active (if (api::menu-item-category-on-click item)
                                                                (api:colors-fill-active ui)
                                                                (api:colors-fill-hover ui)))
                                                   (t nil))))
       (:ui ui :let widget :layout ((padding yogalayout:+edge-all+ 3.0)))
       (gui:w-label :ui ui
                    :text (api::menu-item-category-name item)
                    :font-size font-size
                    :cursor nil)
       (setf (gui:widget-hitp widget) #'gui:default-widget-hitp)
       (when (api::menu-item-category-on-click item)
              (setf (gui:widget-on-mouse-click-left widget)
                (lambda (ui widget x y other)
                  (declare (ignore ui widget x y other))
                  (funcall (api::menu-item-category-on-click item))
                  (funcall close-menu)
                  t)))
       (setf (gui:widget-on-mouse-enter widget)
          (lambda (ui widget x y other)
            (declare (ignore ui widget x y other))
            (funcall set-next (api::menu-item-category-items item))
            (values)))
       (setf (gui:widget-on-mouse-leave widget)
          (lambda (ui widget x y other)
            (declare (ignore ui widget x y other))
            (funcall reset-next)
            (values)))))
    (api::menu-item-button
     (gui:w-visual ((gui:v-rectangle :fill-style (gui:wsd widget
                                                   (:active (api:colors-fill-active ui))
                                                   (:hover (api:colors-fill-hover ui))
                                                   (t nil))))
       (:ui ui :let widget :layout ((padding yogalayout:+edge-all+ 3.0)))
       (gui:w-label :ui ui
                    :text (api::menu-item-button-text item)
                    :font-size font-size
                    :cursor nil)
       (setf (gui:widget-hitp widget) #'gui:default-widget-hitp)
       (setf (gui:widget-on-mouse-click-left widget)
          (lambda (ui widget x y other)
            (declare (ignore ui widget x y other))
            (funcall (api::menu-item-button-on-click item))
            (funcall close-menu)
            t))))))

(defun w-menu-list (ui menu-items-computed close-menu
                       &key
                       (modify-hover (lambda (f)
                                       (declare (ignore f))
                                       (values)))
                       (font-size +context-menu-font-size+))
  (let ((hover-count 0)
        (sdet-ctx (gui:sdet-context ui))
        (hover-zero-time nil))
    (multiple-value-bind (get-next set-next)
        (sdet:make-state sdet-ctx nil :equal #'eq)
      (gui:w-block* ((flex-direction yogalayout:+flex-direction-row+)
                     (align-items yogalayout:+align-flex-start+)) (:ui ui :let container)
        (setf (gui:widget-hitp container) #'gui:default-widget-hitp)
        (setf (gui:widget-on-mouse-enter container)
           (lambda (ui widget x y other)
             (declare (ignore ui widget x y other))
             (funcall modify-hover #'1+)
             (values)))
        (setf (gui:widget-on-mouse-leave container)
           (lambda (ui widget x y other)
             (declare (ignore ui widget x y other))
             (funcall modify-hover #'1-)
             (values)))
        (setf (gui:widget-on-render-end container)
           (lambda (ui widget)
             (declare (ignore widget))
             (when (and (= 0 hover-count)
                        hover-zero-time
                        (> (gui:time ui) hover-zero-time))
                   (setf hover-zero-time nil)
                   (funcall set-next nil))))
        (gui:w-visual ((gui:v-rectangle :fill-style (api:colors-background-focus ui)
                                        :border-style (api:colors-border-active ui)
                                        :border-width 1.2))
          (:ui ui :layout ((flex-direction yogalayout:+flex-direction-column+)
                           (padding yogalayout:+edge-all+ 4.0)
                           (margin yogalayout:+edge-right+ 4.0)
                           (margin yogalayout:+edge-bottom+ 2.0)))
          (gui:w-map-fix-index* ui
                                menu-items-computed
                                (lambda (get-item index)
                                  (declare (ignore index))
                                  (w-menu-item ui (funcall get-item) close-menu
                                               (lambda (next)
                                                 (unless (eq next (sdet:unobserved sdet-ctx (funcall get-next)))
                                                   (funcall set-next next))
                                                 (incf hover-count)
                                                 (values))
                                               (lambda ()
                                                 (decf hover-count)
                                                 (when (= 0 hover-count)
                                                       (setf hover-zero-time (+ 0.1d0 (gui:time ui))))
                                                 (values))))))
        (gui:w-dynamic* ui
                        (lambda ()
                          (let ((next (funcall get-next)))
                            (when next
                                  (w-menu-list ui next close-menu
                                               :modify-hover (lambda (f)
                                                               (setf hover-count (funcall f hover-count))
                                                               (when (= hover-count 0)
                                                                     (setf hover-zero-time (+ 0.1d0 (gui:time ui))))
                                                               (values))
                                               :font-size font-size)))))))))