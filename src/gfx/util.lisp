(in-package #:my-editor)

(defun create-raw-sdl-window (title width height flags parent offset-x offset-y)
  (setf flags (or flags 0))
  (let ((is-popup (/= 0 (logior
                          (logand %sdl3:+window-tooltip+ flags)
                          (logand %sdl3:+window-popup-menu+ flags)))))
    (when (and is-popup (or (not parent) (not offset-x) (not offset-y)))
          (error "Attempting to create window with popup flags while not specifying parent or offsets"))
    (when (and (not is-popup) (or parent offset-x offset-y))
          (error "Attempting to create window without popup flags while specifying parent or offsets"))
    (let ((window (if is-popup
                      (%sdl3:create-popup-window parent offset-x offset-y width height flags)
                      (%sdl3:create-window title width height flags))))
      (multiple-value-bind (real-pixel-width real-pixel-height) (sdl3:get-window-size-in-pixels window)
        (values (autowrap:ptr window) real-pixel-width real-pixel-height)))))

(declaim (ftype (function (%sdl3:pixel-format %blend2d:format-info &key (:debug (or null t stream))) boolean) format-sdl3->blend2d))
(defun format-sdl3->blend2d (sdl3-fmt blend2d-fmt &key debug)
  (let* ((rsize (- 8 (%sdl3:pixel-format.rloss sdl3-fmt)))
         (gsize (- 8 (%sdl3:pixel-format.gloss sdl3-fmt)))
         (bsize (- 8 (%sdl3:pixel-format.bloss sdl3-fmt)))
         (asize (- 8 (%sdl3:pixel-format.aloss sdl3-fmt)))
         (has-r (/= 0 (%sdl3:pixel-format.rmask sdl3-fmt)))
         (has-g (/= 0 (%sdl3:pixel-format.gmask sdl3-fmt)))
         (has-b (/= 0 (%sdl3:pixel-format.bmask sdl3-fmt)))
         (has-a (/= 0 (%sdl3:pixel-format.amask sdl3-fmt))))
    (setf (%blend2d:format-info.depth blend2d-fmt) (* 8 (%sdl3:pixel-format.bytes-per-pixel sdl3-fmt)))
    (setf (%blend2d:format-info.flags blend2d-fmt)
      (logior
        (if (= 8 rsize gsize bsize asize) %blend2d:+format-flag-byte-aligned+ 0)
        (if (and has-r has-g has-b)
            (if has-a %blend2d:+format-flag-rgba+ %blend2d:+format-flag-rgb+)
            (if (and has-a (not has-r) (not has-g) (not has-b)) %blend2d:+format-flag-alpha+ 0))))
    (setf (cffi:mem-ref (autowrap:ptr blend2d-fmt) :uint8 8) rsize) ; R-SIZE
    (setf (cffi:mem-ref (autowrap:ptr blend2d-fmt) :uint8 9) gsize)
    (setf (cffi:mem-ref (autowrap:ptr blend2d-fmt) :uint8 10) bsize)
    (setf (cffi:mem-ref (autowrap:ptr blend2d-fmt) :uint8 11) asize)
    (setf (cffi:mem-ref (autowrap:ptr blend2d-fmt) :uint8 12) (%sdl3:pixel-format.rshift sdl3-fmt)) ; R-SHIFT
    (setf (cffi:mem-ref (autowrap:ptr blend2d-fmt) :uint8 13) (%sdl3:pixel-format.gshift sdl3-fmt)) ; G-SHIFT
    (setf (cffi:mem-ref (autowrap:ptr blend2d-fmt) :uint8 14) (%sdl3:pixel-format.bshift sdl3-fmt)) ; B-SHIFT
    (setf (cffi:mem-ref (autowrap:ptr blend2d-fmt) :uint8 15) (%sdl3:pixel-format.ashift sdl3-fmt)) ; A-SHIFT
    (when debug
          (format debug "RSIZE: ~A~%" (cffi:mem-ref (autowrap:ptr blend2d-fmt) :uint8 8))
          (format debug "GSIZE: ~A~%" (cffi:mem-ref (autowrap:ptr blend2d-fmt) :uint8 9))
          (format debug "BSIZE: ~A~%" (cffi:mem-ref (autowrap:ptr blend2d-fmt) :uint8 10))
          (format debug "ASIZE: ~A~%" (cffi:mem-ref (autowrap:ptr blend2d-fmt) :uint8 11))
          (format debug "RSHIFT: ~A~%" (cffi:mem-ref (autowrap:ptr blend2d-fmt) :uint8 12))
          (format debug "GSHIFT: ~A~%" (cffi:mem-ref (autowrap:ptr blend2d-fmt) :uint8 13))
          (format debug "BSHIFT: ~A~%" (cffi:mem-ref (autowrap:ptr blend2d-fmt) :uint8 14))
          (format debug "ASHIFT: ~A~%" (cffi:mem-ref (autowrap:ptr blend2d-fmt) :uint8 15))
          (format debug "FLAGS: ~A~%" (%blend2d:format-info.flags blend2d-fmt))
          (format debug "SURF R MASK: ~A~%" (%sdl3:pixel-format.rmask sdl3-fmt))
          (format debug "SURF G MASK: ~A~%" (%sdl3:pixel-format.gmask sdl3-fmt))
          (format debug "SURF B MASK: ~A~%" (%sdl3:pixel-format.bmask sdl3-fmt))
          (format debug "SURF A MASK: ~A~%" (%sdl3:pixel-format.amask sdl3-fmt))))
  (= 0 (%blend2d:format-info-sanitize blend2d-fmt)))
