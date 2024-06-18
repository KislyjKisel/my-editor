(in-package #:my-editor)

(defstruct (gfx-sdl-renderer (:copier nil)
                             (:constructor make-gfx-sdl-renderer (renderer)))
  (renderer)
  (texture nil)
  (blend2d-image nil)
  (pitch)
  (pixels)
  (blend2d-pixel-converter)
  (blend2d-format-src)
  (blend2d-format-dst)
  (blend2d-image-data))

(defun gfx-sdl-renderer-blend-mode ()
  (%sdl3:compose-custom-blend-mode %sdl3:+blendfactor-src-alpha+
                                   %sdl3:+blendfactor-one-minus-src-alpha+
                                   %sdl3:+blendoperation-add+
                                   %sdl3:+blendfactor-one+
                                   %sdl3:+blendfactor-one-minus-src-alpha+
                                   %sdl3:+blendoperation-add+))

(defmethod api:gfx-initialize ((backend-key (eql :sdl-renderer)) with-ui
                                                                 &key
                                                                 window-title window-width window-height window-flags
                                                                 window-parent window-offset-x window-offset-y)
  ;; TODO: configure renderer backend somehow (usecase?)
  (multiple-value-bind (sdl-window px-width px-height)
      (create-raw-sdl-window window-title window-width window-height window-flags
                             window-parent window-offset-x window-offset-y)
    (setf px-width (if (= 0 px-width) 1 px-width))
    (setf px-height (if (= 0 px-height) 1 px-height))
    (let* ((renderer (%sdl3:create-renderer sdl-window (cffi:null-pointer)))
           (state (make-gfx-sdl-renderer renderer)))
      (when (/= 0 (%sdl3:set-render-v-sync renderer %sdl3:+renderer-vsync-adaptive+))
            (%sdl3:set-render-v-sync renderer 1))
      (let ((rp (%sdl3:get-renderer-properties renderer))
            (rpn (sdl3:alloc-prop-renderer-name-string)))
        (format t "GFX-SDL-RENDERER uses ~a~%" (%sdl3:get-string-property rp rpn (cffi:null-pointer)))
        (cffi:foreign-string-free rpn))
      (if (not with-ui)
          (values state sdl-window nil)
          (progn
           (setf (gfx-sdl-renderer-texture state)
             (%sdl3:create-texture renderer
                                   %sdl3:+pixelformat-argb32+
                                   %sdl3:+textureaccess-streaming+
                                   px-width
                                   px-height))
           ; dbg info
           ; (let ((format (cffi:foreign-alloc :int)))
           ;   (%sdl3:query-texture (gfx-sdl-renderer-texture state) format (cffi:null-pointer) (cffi:null-pointer) (cffi:null-pointer))
           ;   (format t "TX FMT: ~A~%" (cffi:mem-aref format :int)))
           (%sdl3:set-texture-blend-mode (gfx-sdl-renderer-texture state) (gfx-sdl-renderer-blend-mode))
           (setf (gfx-sdl-renderer-blend2d-image state)
             (autowrap:alloc '%blend2d:image-core))
           (%blend2d:image-init-as (gfx-sdl-renderer-blend2d-image state)
                                   px-width
                                   px-height
                                   %blend2d:+format-prgb32+)
           (setf (gfx-sdl-renderer-pixels state)
             (cffi:foreign-alloc :pointer))
           (setf (gfx-sdl-renderer-pitch state)
             (cffi:foreign-alloc :int))
           (setf (gfx-sdl-renderer-blend2d-format-src state)
             (autowrap:alloc '%blend2d:format-info))
           (setf (gfx-sdl-renderer-blend2d-format-dst state)
             (autowrap:alloc '%blend2d:format-info))
           (%blend2d:format-info-query (gfx-sdl-renderer-blend2d-format-src state)
                                       %blend2d:+format-prgb32+)
           (%blend2d:format-info-query (gfx-sdl-renderer-blend2d-format-dst state)
                                       %blend2d:+format-prgb32+)
           ; (setf (%blend2d:format-info.flags (gfx-sdl-renderer-blend2d-format-dst state))
           ;   (logxor
           ;     (%blend2d:format-info.flags (gfx-sdl-renderer-blend2d-format-dst state))
           ;     %blend2d:+format-flag-premultiplied+))
           (setf (gfx-sdl-renderer-blend2d-pixel-converter state)
             (autowrap:alloc '%blend2d:pixel-converter-core))
           (%blend2d:pixel-converter-init (gfx-sdl-renderer-blend2d-pixel-converter state))
           (%blend2d:pixel-converter-create (gfx-sdl-renderer-blend2d-pixel-converter state)
                                            (gfx-sdl-renderer-blend2d-format-dst state)
                                            (gfx-sdl-renderer-blend2d-format-src state)
                                            %blend2d:+pixel-converter-create-no-flags+)
           (setf (gfx-sdl-renderer-blend2d-image-data state)
             (autowrap:alloc '%blend2d:image-data))
           (values
             state
             sdl-window
             (gui:create-layer (gfx-sdl-renderer-blend2d-image state))))))))

(defmethod api:gfx-resize ((state gfx-sdl-renderer) window layer px-width px-height)
  (declare (ignore window))
  (when layer
        (assert (and (gfx-sdl-renderer-texture state)
                     (gfx-sdl-renderer-blend2d-image state)))
        (setf px-width (if (= 0 px-width) 1 px-width))
        (setf px-height (if (= 0 px-height) 1 px-height))
        (%sdl3:destroy-texture (gfx-sdl-renderer-texture state))
        (setf (gfx-sdl-renderer-texture state)
          (%sdl3:create-texture (gfx-sdl-renderer-renderer state)
                                %sdl3:+pixelformat-argb32+
                                %sdl3:+textureaccess-streaming+
                                px-width
                                px-height))
        (%sdl3:set-texture-blend-mode (gfx-sdl-renderer-texture state) (gfx-sdl-renderer-blend-mode))
        (setf (gfx-sdl-renderer-blend2d-image state)
          (gui:update-layer layer
                            (lambda ()
                              (%blend2d:image-reset (gfx-sdl-renderer-blend2d-image state))
                              (%blend2d:image-create (gfx-sdl-renderer-blend2d-image state)
                                                     px-width
                                                     px-height
                                                     %blend2d:+format-prgb32+)
                              (gfx-sdl-renderer-blend2d-image state)))))
  (values))

(defmethod api:gfx-render ((state gfx-sdl-renderer) window gui-layer render-content)
  (declare (ignore window))
  (%sdl3:set-render-draw-color-float (gfx-sdl-renderer-renderer state)
                                     0.0 0.0 0.0 0.0)
  (%sdl3:render-clear (gfx-sdl-renderer-renderer state))
  (funcall render-content (gfx-sdl-renderer-renderer state))
  (when gui-layer
        (%sdl3:lock-texture (gfx-sdl-renderer-texture state)
                            (cffi:null-pointer)
                            (gfx-sdl-renderer-pixels state)
                            (gfx-sdl-renderer-pitch state))
        (%blend2d:image-get-data (gfx-sdl-renderer-blend2d-image state)
                                 (gfx-sdl-renderer-blend2d-image-data state))
        (%blend2d:pixel-converter-convert (gfx-sdl-renderer-blend2d-pixel-converter state)
                                          (cffi:mem-aref (gfx-sdl-renderer-pixels state) :pointer)
                                          (cffi:mem-aref (gfx-sdl-renderer-pitch state) :int)
                                          (%blend2d:image-data.pixel-data (gfx-sdl-renderer-blend2d-image-data state))
                                          (%blend2d:image-data.stride (gfx-sdl-renderer-blend2d-image-data state))
                                          (%blend2d:image-data.size.w (gfx-sdl-renderer-blend2d-image-data state))
                                          (%blend2d:image-data.size.h (gfx-sdl-renderer-blend2d-image-data state))
                                          (cffi:null-pointer))
        (%sdl3:unlock-texture (gfx-sdl-renderer-texture state))
        (%sdl3:render-texture (gfx-sdl-renderer-renderer state)
                              (gfx-sdl-renderer-texture state)
                              (cffi:null-pointer)
                              (cffi:null-pointer)))
  (%sdl3:render-present (gfx-sdl-renderer-renderer state))
  (values))

(defmethod api:gfx-terminate ((state gfx-sdl-renderer) sdl-window)
  (when (or (gfx-sdl-renderer-blend2d-image state) (gfx-sdl-renderer-texture state))
        (assert (and (gfx-sdl-renderer-blend2d-image state)
                     (gfx-sdl-renderer-texture state)))
        (%blend2d:image-destroy (gfx-sdl-renderer-blend2d-image state))
        (autowrap:free (gfx-sdl-renderer-blend2d-image state))
        (%sdl3:destroy-texture (gfx-sdl-renderer-texture state))
        (cffi:foreign-free (gfx-sdl-renderer-pixels state))
        (cffi:foreign-free (gfx-sdl-renderer-pitch state))
        (%blend2d:pixel-converter-destroy (gfx-sdl-renderer-blend2d-pixel-converter state))
        (autowrap:free (gfx-sdl-renderer-blend2d-pixel-converter state))
        (autowrap:free (gfx-sdl-renderer-blend2d-format-src state))
        (autowrap:free (gfx-sdl-renderer-blend2d-format-dst state))
        (autowrap:free (gfx-sdl-renderer-blend2d-image-data state)))
  (%sdl3:destroy-renderer (gfx-sdl-renderer-renderer state))
  (%sdl3:destroy-window sdl-window)
  (values))
