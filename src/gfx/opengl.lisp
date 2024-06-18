(in-package #:my-editor)

(defstruct (gfx-opengl (:copier nil))
  (thread)
  (atomic-exit-flag)
  (atomic-window-size)
  (exit-lock)
  (render-content nil :type (or null function)))

(defstruct (opengl-sync (:copier nil))
  (rc 0 :type (integer 0))
  (frame 0 :type (integer 0))
  (render-called 0 :type (integer 0))
  (lock (bt2:make-lock :name "Editor OpenGL sync lock") :type bt2:lock)
  (condvar (bt2:make-condition-variable :name "Editor OpenGL sync condvar") :type bt2:condition-variable)
  (barrier (make-thread-barrier 1 :name "Editor OpenGL sync barrier") :type thread-barrier))

(global-vars:define-global-var **opengl-sync** nil)

(defconstant +opengl-window-size-dimension-bits+ 16)

(declaim (inline opengl-window-dims-to-size))
(defun opengl-window-dims-to-size (width height)
  (logior width (ash height +opengl-window-size-dimension-bits+)))

(defun opengl-thread (raw-sdl-window gfx-state layer-semaphore set-layer)
  (lambda ()
    (assert (or (and layer-semaphore set-layer) (and (not layer-semaphore) (not set-layer))))
    ; (setf %gl:*gl-get-proc-address* #'%sdl3:gl-get-proc-address)
    (flet ((make-gui-pixels-vbo (width height)
                                (when (or (= 0 width) (= 0 height))
                                      (return-from make-gui-pixels-vbo (values nil nil 0)))
                                (let ((vbo (gl:gen-buffer))
                                      (chunk-size (* 4 width height)))
                                  (gl:bind-buffer :shader-storage-buffer vbo)
                                  (gl:buffer-storage :shader-storage-buffer
                                                     (cffi:null-pointer)
                                                     '(:map-write-bit :map-persistent-bit)
                                                     :end (* 3 chunk-size))
                                  (values
                                    vbo
                                    (%gl:map-buffer-range :shader-storage-buffer
                                                          0
                                                          (* 3 chunk-size)
                                                          (cffi:foreign-bitfield-value '%gl::MapBufferAccessMask '(:map-write
                                                                                                                   :map-persistent
                                                                                                                   :map-invalidate-buffer
                                                                                                                   :map-flush-explicit
                                                                                                                   :map-unsynchronized)))
                                    chunk-size)))
           (win-size-to-dims (size)
                             (let ((mask (1- (ash 1 +opengl-window-size-dimension-bits+))))
                               (values (logand mask size) (logand mask (ash size (- +opengl-window-size-dimension-bits+)))))))
      (bt2:with-lock-held ((gfx-opengl-exit-lock gfx-state))
        (let* ((with-ui (not (null layer-semaphore)))
               (context (%sdl3:gl-create-context raw-sdl-window))
               (lock (opengl-sync-lock **opengl-sync**))
               (condvar (opengl-sync-condvar **opengl-sync**))
               (barrier (opengl-sync-barrier **opengl-sync**))
               (frame nil)
               (prev-win-size (bt2:atomic-integer-value (gfx-opengl-atomic-window-size gfx-state)))
               win-width win-height
               (blend2d-image nil)
               (blend2d-pixel-converter)
               (blend2d-format-src)
               (blend2d-format-dst)
               (blend2d-image-data)
               gui-vbo
               gui-vao
               (gui-pixels-vbo nil) ; may be NIL for zero sized windows
               gui-pixels-ptr
               gui-pixels-chunk-size
               (gui-pixels-chunk-index 0)
               gui-chunk-render-fences
               gui-shader-program
               gui-layer)
          (%sdl3:gl-make-current raw-sdl-window context)
          (when (> 0 (%sdl3:gl-set-swap-interval -1))
                (%sdl3:gl-set-swap-interval 1))
          (when with-ui
                (setf gui-chunk-render-fences (make-array 3 :initial-element nil))
                (setf gui-vbo (gl:create-buffer))
                (let* ((len 8)
                       (data (cffi:foreign-alloc :float
                                                 :count len
                                                 :initial-contents #(#+(or)0
                                                                     1.0 -1.0
                                                                     1.0 1.0
                                                                     -1.0 1.0
                                                                     -1.0 -1.0))))
                  (%gl:named-buffer-storage gui-vbo (* 4 len) data 0)
                  (cffi:foreign-free data))
                (flet ((make-shader (shader-type source)
                                    (let ((shader (gl:create-shader shader-type)))
                                      (gl:shader-source shader source)
                                      (gl:compile-shader shader)
                                      (unless (gl:get-shader shader :compile-status)
                                        (gl:get-shader shader :info-log-length)
                                        (error "Shader compilation failed (~a): ~s" shader-type (gl:get-shader-info-log shader)))
                                      shader)))
                  (let ((vertex-shader (make-shader :vertex-shader *opengl-gui-vertex-shader*))
                        (fragment-shader (make-shader :fragment-shader *opengl-gui-fragment-shader*)))
                    (setf gui-shader-program (gl:create-program))
                    (gl:attach-shader gui-shader-program vertex-shader)
                    (gl:attach-shader gui-shader-program fragment-shader)
                    (gl:link-program gui-shader-program)
                    (unless (gl:get-program gui-shader-program :link-status)
                      (error (gl:get-program-info-log gui-shader-program)))
                    (gl:delete-shader vertex-shader)
                    (gl:delete-shader fragment-shader)))
                (setf gui-vao (gl:gen-vertex-array))
                (gl:bind-vertex-array gui-vao)
                (gl:bind-buffer :array-buffer gui-vbo)
                (gl:vertex-attrib-pointer 0 2 :float nil (* 2 (cffi:foreign-type-size :float)) 0)
                (gl:enable-vertex-attrib-array 0)
                (gl:bind-vertex-array 0)
                (multiple-value-setq (win-width win-height) (win-size-to-dims prev-win-size))
                (multiple-value-setq (gui-pixels-vbo gui-pixels-ptr gui-pixels-chunk-size) (make-gui-pixels-vbo win-width win-height))
                ;; ^^^ OpenGL  init ^^^
                ;; VVV Blend2d init VVV
                (setf blend2d-image
                  (autowrap:alloc '%blend2d:image-core))
                (%blend2d:image-init-as blend2d-image
                                        win-width
                                        win-height
                                        %blend2d:+format-prgb32+)
                (let* ((format-dst (autowrap:alloc '%blend2d:format-info)))
                  (setf blend2d-format-dst format-dst)
                  ;; TODO: fill format-dst + sanitize check
                  (%blend2d:format-info-query format-dst %blend2d:+format-prgb32+))
                (setf blend2d-format-src (autowrap:alloc '%blend2d:format-info))
                (%blend2d:format-info-query blend2d-format-src %blend2d:+format-prgb32+)
                (setf blend2d-pixel-converter
                  (autowrap:alloc '%blend2d:pixel-converter-core))
                (%blend2d:pixel-converter-init blend2d-pixel-converter)
                (%blend2d:pixel-converter-create blend2d-pixel-converter
                                                 blend2d-format-dst
                                                 blend2d-format-src
                                                 %blend2d:+pixel-converter-create-no-flags+)
                (setf blend2d-image-data (autowrap:alloc '%blend2d:image-data))
                (funcall set-layer (setf gui-layer (gui:create-layer blend2d-image)))
                (setf set-layer nil)
                (bt2:signal-semaphore layer-semaphore)
                (setf layer-semaphore nil))
          ;; Rendering loop
          (loop #:named render-loop
                #:do
                (bt2:acquire-lock lock)
                (unless frame (setf frame (opengl-sync-frame **opengl-sync**)))
                ; (format t "OPENGL-THREAD WAIT FRAME~%")
                (loop #:while (<= (opengl-sync-frame **opengl-sync**) frame)
                      #:do
                      (bt2:condition-wait condvar lock)
                      (unless (= 0 (bt2:atomic-integer-value (gfx-opengl-atomic-exit-flag gfx-state)))
                        (bt2:release-lock lock)
                        (return-from render-loop)))
                (bt2:release-lock lock)
                ; (format t "OPENGL-THREAD START FRAME~%")
                ; (format t "RENDER NEW FRAME ~a >= ~a ~%" (opengl-sync-frame **opengl-sync**) frame)
                (incf frame)
                ; (gl:disable :scissor-test)
                ; (gl:clear-color 0.0 0.0 0.0 0.0)
                ; (gl:clear :color-buffer)
                (when (gfx-opengl-render-content gfx-state)
                      (funcall (gfx-opengl-render-content gfx-state) nil))
                (when with-ui
                      (let ((data-offset (* gui-pixels-chunk-index gui-pixels-chunk-size)))
                        (unless
                            (let ((last-fence (svref gui-chunk-render-fences gui-pixels-chunk-index)))
                              (when last-fence
                                    (if (eq :timeout-expired
                                            (%gl:client-wait-sync last-fence 0 (floor 1e6)))
                                        t
                                        (progn
                                         (%gl:delete-sync last-fence)
                                         (setf (svref gui-chunk-render-fences gui-pixels-chunk-index) nil)
                                         nil))))
                          (%blend2d:image-get-data blend2d-image blend2d-image-data)
                          (%blend2d:pixel-converter-convert blend2d-pixel-converter
                                                            (cffi:inc-pointer gui-pixels-ptr data-offset)
                                                            (* 4 win-width)
                                                            (%blend2d:image-data.pixel-data blend2d-image-data)
                                                            (%blend2d:image-data.stride blend2d-image-data)
                                                            win-width ; (%blend2d:image-data.size.w blend2d-image-data)
                                                            win-height ; (%blend2d:image-data.size.h blend2d-image-data)
                                                            (cffi:null-pointer))
                          (%gl:flush-mapped-named-buffer-range gui-pixels-vbo data-offset gui-pixels-chunk-size)
                          (gl:viewport 0 0 win-width win-height)
                          (gl:enable :blend)
                          (gl:disable :depth-test)
                          (gl:disable :scissor-test)
                          (gl:blend-func :src-alpha :one-minus-src-alpha)
                          (gl:use-program gui-shader-program)
                          (gl:uniformf 0 (* 0.5 (coerce win-width 'single-float)))
                          (gl:uniformf 1 (* 0.5 (coerce win-height 'single-float)))
                          (gl:uniformi 2 win-width)
                          (gl:bind-vertex-array gui-vao)
                          (%gl:bind-buffer-range :shader-storage-buffer 0 gui-pixels-vbo data-offset gui-pixels-chunk-size)
                          (gl:draw-arrays :triangle-fan 0 4)
                          (setf (svref gui-chunk-render-fences gui-pixels-chunk-index)
                            (%gl:fence-sync :sync-gpu-commands-complete 0))
                          (setf gui-pixels-chunk-index (mod (1+ gui-pixels-chunk-index) 3)))))
                (%sdl3:gl-swap-window raw-sdl-window)
                ;; Render before resizing to not break existing image
                (let ((new-win-size (bt2:atomic-integer-value (gfx-opengl-atomic-window-size gfx-state))))
                  (when (and with-ui (/= prev-win-size new-win-size))
                        (loop #:for index #:below 3
                              #:for fence = (svref gui-chunk-render-fences index)
                              #:do (when fence
                                    (%gl:client-wait-sync fence 0 (floor 8e6))
                                    (%gl:delete-sync fence)
                                    (setf (svref gui-chunk-render-fences index) nil)))
                        (setf gui-pixels-chunk-index 0)
                        (setf prev-win-size new-win-size)
                        (multiple-value-setq (win-width win-height) (win-size-to-dims new-win-size))
                        (%gl:unmap-named-buffer gui-pixels-vbo)
                        (when gui-pixels-vbo (gl:delete-buffers (list gui-pixels-vbo)))
                        (multiple-value-setq (gui-pixels-vbo gui-pixels-ptr gui-pixels-chunk-size) (make-gui-pixels-vbo win-width win-height))
                        (setf blend2d-image
                          (gui:update-layer gui-layer
                                            (lambda ()
                                              (%blend2d:image-reset blend2d-image)
                                              (%blend2d:image-create blend2d-image
                                                                     win-width
                                                                     win-height
                                                                     %blend2d:+format-prgb32+)
                                              blend2d-image)))))
                ;; Resize before waiting to avoid layer race with main
                ; (format t "OPENGL-THREAD BARRIER IN~%")
                (thread-barrier-wait barrier)
                ; (format t "OPENGL-THREAD BARRIER OUT~%")
                )
          (when with-ui
                (gl:delete-vertex-arrays (list gui-vao))
                (gl:delete-program gui-shader-program)
                (let ((vbos (list gui-vbo)))
                  (when gui-pixels-vbo
                        (push gui-pixels-vbo vbos)
                        (%gl:unmap-named-buffer gui-pixels-vbo))
                  (gl:delete-buffers vbos))
                (%blend2d:image-destroy blend2d-image)
                (autowrap:free blend2d-image)
                (%blend2d:pixel-converter-destroy blend2d-pixel-converter)
                (autowrap:free blend2d-pixel-converter)
                (autowrap:free blend2d-format-src)
                (autowrap:free blend2d-format-dst)
                (autowrap:free blend2d-image-data))
          (%sdl3:gl-delete-context context)))
      (values))))

(defmethod api:gfx-initialize ((backend-key (eql :opengl)) with-ui
                                                           &key
                                                           window-title window-width window-height window-flags
                                                           window-parent window-offset-x window-offset-y)
  ;; TODO: allow user to specify opengl attributes
  (%sdl3:gl-set-attribute %sdl3:+gl-context-major-version+ 4)
  (%sdl3:gl-set-attribute %sdl3:+gl-context-minor-version+ 6)
  (%sdl3:gl-set-attribute %sdl3:+gl-context-profile-mask+ %sdl3:+gl-context-profile-core+)
  (%sdl3:gl-set-attribute %sdl3:+gl-doublebuffer+ 1)
  (%sdl3:gl-set-attribute %sdl3:+gl-context-flags+ %sdl3:+gl-context-debug-flag+)
  (multiple-value-bind (raw-sdl-window px-width px-height)
      (create-raw-sdl-window window-title window-width window-height (logior window-flags %sdl3:+window-opengl+)
                             window-parent window-offset-x window-offset-y)
    (unless **opengl-sync**
      (setf **opengl-sync** (make-opengl-sync)))
    (incf (opengl-sync-rc **opengl-sync**))
    (let* ((atomic-exit-flag (bt2:make-atomic-integer :value 0))
           (atomic-window-size (bt2:make-atomic-integer :value (opengl-window-dims-to-size px-width px-height)))
           (exit-lock (bt2:make-lock))
           (layer-semaphore nil)
           (layer nil)
           (set-layer nil)
           (state (make-gfx-opengl :atomic-exit-flag atomic-exit-flag
                                   :atomic-window-size atomic-window-size
                                   :exit-lock exit-lock)))
      (when with-ui
            (setf set-layer (lambda (created-layer) (setf layer created-layer)))
            (setf layer-semaphore (bt2:make-semaphore)))
      (thread-barrier-change-count (opengl-sync-barrier **opengl-sync**))
      (setf (gfx-opengl-thread state)
        (bt2:make-thread (opengl-thread raw-sdl-window state layer-semaphore set-layer)))
      (when layer-semaphore
            (bt2:wait-on-semaphore layer-semaphore))
      (values state raw-sdl-window layer))))

(defmethod api:gfx-resize ((state gfx-opengl) window layer px-width px-height)
  (setf (bt2:atomic-integer-value (gfx-opengl-atomic-window-size state))
    (opengl-window-dims-to-size px-width px-height))
  (values))

(defmethod api:gfx-render ((state gfx-opengl) window gui-layer render-content)
  (setf (gfx-opengl-render-content state) render-content)
  ; **opengl-sync** is not locked: all render threads should be waiting on condvar 
  (incf (opengl-sync-render-called **opengl-sync**))
  (when (>= (opengl-sync-render-called **opengl-sync**)
            (opengl-sync-rc **opengl-sync**))
        (incf (opengl-sync-frame **opengl-sync**))
        (bt2:condition-broadcast (opengl-sync-condvar **opengl-sync**))
        (thread-barrier-wait (opengl-sync-barrier **opengl-sync**))
        (setf (opengl-sync-render-called **opengl-sync**) 0))
  (values))

(defmethod api:gfx-terminate ((state gfx-opengl) raw-sdl-window)
  ;; barrier's count can't become less than the number of waiting threads
  ;; because the main thread is also a participant, is not waiting and will release others later
  (thread-barrier-change-count (opengl-sync-barrier **opengl-sync**) :delta -1)
  (setf (bt2:atomic-integer-value (gfx-opengl-atomic-exit-flag state)) 1)
  (bt2:condition-broadcast (opengl-sync-condvar **opengl-sync**))
  (bt2:acquire-lock (gfx-opengl-exit-lock state))
  (%sdl3:destroy-window raw-sdl-window)
  ; (already exited) (bt2:destroy-thread (gfx-opengl-thread state))
  (bt2:release-lock (gfx-opengl-exit-lock state))
  (decf (opengl-sync-rc **opengl-sync**))
  (when (= 0 (opengl-sync-rc **opengl-sync**))
        (setf **opengl-sync** nil))
  (values))

(defvar *opengl-gui-vertex-shader*
        "
        #version 460 core
        layout(location = 0) uniform float uHalfWidthF;
        layout(location = 1) uniform float uHalfHeightF;

        layout(location = 0) in vec2 vOffset;

        out vec2 fTexCoords;

        void main() {
          fTexCoords = vec2((vOffset.x + 1.0) * uHalfWidthF, (-vOffset.y + 1.0) * uHalfHeightF);
          gl_Position = vec4(vOffset, 0.0, 1.0);
        }
        ")

(defvar *opengl-gui-fragment-shader*
        "
        #version 460 core

        layout(location = 2) uniform int uWidth;
        layout(std430, binding = 0) restrict readonly buffer bTexture {
          uint colors[];
        };

        in vec2 fTexCoords;

        out vec4 oColor;

        void main() {
          uint color = colors[int(fTexCoords.y) * uWidth + int(fTexCoords.x)];
          oColor = vec4(
            float((color >> 16u) & 0xFFu) / 255.0,
            float((color >> 8u) & 0xFFu) / 255.0,
            float((color >> 0u) & 0xFFu) / 255.0,
            float((color >> 24u) & 0xFFu) / 255.0
          );
        }
        ")
