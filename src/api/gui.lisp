(in-package #:my-editor/api)

(defconstant +ui-value-colors+ :colors)

;;; TODO: make configurable (by app user) or export
(defconstant +error-color+ #xFFFF0000)
(defconstant +warning-color+ #xFFCCCC00)
(defconstant +info-color+ #xFF00AAAA)
(defconstant +debug-color+ #xFF999999)

(macrolet ((define-colors (struct-sym &rest colors)
                          (let* ((slot-interfix "-RAW-")
                                 (sub-get-sym (intern (concatenate 'string (symbol-name struct-sym) slot-interfix "SUBSCRIBE"))))
                            `(progn
                              (defstruct (,struct-sym
                                           (:copier nil)
                                           (:conc-name ,(concatenate 'string (symbol-name struct-sym) slot-interfix))
                                           (:constructor ,(intern (concatenate 'string "MAKE-" (symbol-name struct-sym)))
                                                         (sdet-ctx &aux
                                                                   (subscribe-notify (multiple-value-list (sdet:make-notifier sdet-ctx)))
                                                                   (subscribe (first subscribe-notify))
                                                                   (notify (second subscribe-notify)))))
                                (subscribe (gui::unreachable) :type (function () (values &optional)))
                                (notify (gui::unreachable) :type (function () (values &optional)))
                                .
                                ,(mapcar (lambda (color)
                                           `(,(car color) ,(cdr color) :type (unsigned-byte 32)))
                                     colors))
                              ,@(mapcan (lambda (color)
                                          (let* ((color-name (car color))
                                                 (getter (intern (concatenate 'string (symbol-name struct-sym) "-" (symbol-name color-name))))
                                                 (raw-getter (intern (concatenate 'string (symbol-name struct-sym) slot-interfix (symbol-name color-name)))))
                                            (list
                                             `(export ',getter)
                                             `(declaim
                                                (inline ,getter)
                                                (ftype (function (gui:ui)) ,getter))
                                             `(defun ,getter (ui)
                                                (let ((instance (gui:get-value! ui +ui-value-colors+)))
                                                  (funcall (,sub-get-sym instance))
                                                  (,raw-getter instance))))))
                                    colors)))))
  (define-colors colors
                 (background . #xFF241E27)
                 (background-focus . #xFF342E37)
                 (fill . #xFF74667A)
                 (fill-hover . #xFF887A90)
                 (fill-active . #xFF9C90A2)
                 (fill-disabled . #xFF705D7A)
                 (input . #xFFCCC0AA)
                 (input-content . #xFF000033)
                 (border . #xFF9C90A2)
                 (border-hover . #xFFB0A6B5)
                 (border-active . #xFFC4BCC7)
                 (border-disabled . #xFF847A8A)
                 (main . #xFFE4E4B0)
                 (main-hover . #xFFEDEDBF)
                 (main-active . #xFFF6F6DF)
                 (main-disabled . #xFFD2D280)
                 (title . #xFFFFFFFF)
                 (title-hover . #xFFFFFFFF)
                 (title-active . #xFFFFFFFF)
                 (title-disabled . #xFFFFFFFF)
                 (weak . #xFFD8D8B0)
                 (weak-hover . #xFFE2E2BF)
                 (weak-active . #xFFEAEADF)
                 (weak-disabled . #xFFCECE80)))

(macrolet ((define-color-dispatch (struct &rest colors)
                                  `(progn
                                    .
                                    ,(let* ((slot-interfix "-RAW-")
                                            (sub-get (intern (concatenate 'string (symbol-name struct) slot-interfix "SUBSCRIBE"))))
                                       (mapcan (lambda (color)
                                                 (let ((fn (intern (concatenate 'string (symbol-name struct) "-" (symbol-name color) "-DISPATCH")))
                                                       (col-norm (intern (concatenate 'string (symbol-name struct) slot-interfix (symbol-name color))))
                                                       (col-hover (intern (concatenate 'string (symbol-name struct) slot-interfix (symbol-name color) "-HOVER")))
                                                       (col-active (intern (concatenate 'string (symbol-name struct) slot-interfix (symbol-name color) "-ACTIVE")))
                                                       (col-disabled (intern (concatenate 'string (symbol-name struct) slot-interfix (symbol-name color) "-DISABLED"))))
                                                   (list
                                                    `(export ',fn)
                                                    `(declaim (ftype (function (gui:ui gui:widget) (values (unsigned-byte 32) &optional)) ,fn))
                                                    `(defun ,fn (ui widget)
                                                       (let ((instance (gui:get-value! ui +ui-value-colors+)))
                                                         (funcall (,sub-get instance))
                                                         (gui:wsd widget
                                                           (:disabled (,col-disabled instance))
                                                           (:active (,col-active instance))
                                                           (:hover (,col-hover instance))
                                                           (t (,col-norm instance))))))))
                                           colors)))))
  (define-color-dispatch colors fill border main title weak))

(export 'sdet-context)
(declaim
  (notinline sdet-context)
  (ftype (function () (values sdet:context &optional)) sdet-context))

(export 'activity-own-mouse)
(defun activity-own-mouse (ui activity)
  (let ((container (slot-value activity 'container-widget)))
    (assert container () "Activity is not embedded in a GUI tree.")
    (gui:own-mouse ui container))
  (values))

(export 'activity-disown-mouse)
(defun activity-disown-mouse (ui activity)
  (let ((container (slot-value activity 'container-widget)))
    (assert container () "Activity is not embedded in a GUI tree.")
    (gui:disown-mouse ui container))
  (values))

(export 'start-dragging)
(declaim
  (notinline start-dragging)
  (ftype (function (t
                    (or null (function () (values &optional)))
                    (or null (function () (values &optional)))
                    &key (:parent-window t))
                   (values &optional))
         start-dragging))

(export '(message-box-button make-message-box-button))
(defstruct (message-box-button (:copier nil)
                               (:constructor make-message-box-button (text on-click &key focus)))
  (text (gui::unreachable) :type string)
  (on-click (gui::unreachable) :type (function () (values &optional)))
  (focus nil :type boolean))

(export 'message-box)
(declaim
  (notinline message-box)
  (ftype (function (string string (vector message-box-button)) (values t &optional)) message-box))
(setf (documentation 'message-box 'function)
  "(TITLE TEXT BUTTONS)")

(export '(menu-item-button make-menu-item-button))
(defstruct (menu-item-button (:copier nil)
                             (:predicate nil)
                             (:constructor make-menu-item-button (text on-click)))
  (text (gui::unreachable) :type string)
  (on-click (gui::unreachable) :type (function () (values &optional))))

(export '(menu-item-category make-menu-item-category))
(defstruct (menu-item-category (:copier nil)
                               (:predicate nil)
                               (:constructor make-menu-item-category (name items &optional on-click)))
  (name (gui::unreachable) :type string)
  (items (gui::unreachable) :type vector)
  (on-click (gui::unreachable) :type (or null (function () (values &optional)))))

(export 'menu-item)
(deftype menu-item ()
  '(or null string menu-item-button menu-item-category))

(export 'context-menu)
(declaim
  (notinline context-menu)
  (ftype (function (t real real (sdet:computed (vector menu-item))) (values &optional)) context-menu))
(setf (documentation 'context-menu 'function)
  "(PARENT-WINDOW X Y MENU-ITEMS-COMPUTED)
  Opens context menu at X Y coordinates relative to the PARENT-WINDOW
  (NIL means main window, integers are interpreted as SDL window IDs).
  It will be automatically closed on any mouse click outside or on clickable items.")

(export 'create-tooltip)
(declaim
  (notinline create-tooltip)
  (ftype (function (t real real string) (values t &optional)) create-tooltip))

(export 'move-tooltip)
(declaim
  (notinline move-tooltip)
  (ftype (function (t real real) (values &optional)) move-tooltip))

(export 'destroy-tooltip)
(declaim
  (notinline destroy-tooltip)
  (ftype (function (t) (values &optional)) destroy-tooltip))

(export 'open-inspector) ; todo: rename
(declaim
  (notinline open-inspector)
  (ftype (function (t (or null (function (t) (values &optional))))
                   (values (or null (function () (values t &optional))) &optional))
         open-inspector))

(export 'compose-inspector)
(declaim (ftype (function (ui t) (values &optional)) compose-inspector))
(defgeneric compose-inspector (ui value)
  (:method (ui value)
           (gui:w-label :ui ui
                        :text (format nil "COMPOSE-INSPECTOR wasn't implemented for the target value class ~S." (class-name (class-of value)))
                        :font-size 14
                        :text-style (colors-main ui)))
  (:method (ui (value null))
           (gui:w-label :ui ui
                        :text "Nothing."
                        :font-size 14
                        :text-style (colors-main-disabled ui))))

(export 'message-category)
(deftype message-category ()
  '(member :debug :info :warning :error))

(export 'message)
(declaim
  (notinline message)
  (ftype (function (message-category string) (values &optional)) message))
