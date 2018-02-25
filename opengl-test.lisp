(defparameter *wick-left-spacing* 0.4)
(defparameter *wick-width* 0.15)
(defparameter *frame-count-interval* 50)

(defun debug-log (msg &rest args)
  "Output and flush MSG to STDOUT with arguments ARGS"
  (apply #'format t msg args)
  ;; Flush to standard out
  (finish-output))
 
(defun setup-gl (win gl-context vx vy)
  "Setup OpenGL with the window WIN and the gl context of GL-CONTEXT"
  (debug-log "Setting up window/gl.~%")
  (sdl2:gl-make-current win gl-context)
  (gl:viewport 0 0 vx vy)
  (gl:matrix-mode :projection)
  (gl:ortho -2 2 -2 2 -2 2)
  (gl:matrix-mode :modelview)
  (gl:load-identity)
  ;; Clear to white
  (gl:clear-color 1.0 1.0 1.0 1.0))

(defun count-frames (frame-tracker)
  (decf (car frame-tracker))
  (if (<= (car frame-tracker) 0)
      (let ((tdiff (- (sdl2:get-ticks) (cdr frame-tracker))))
	(setf (cdr frame-tracker) (sdl2:get-ticks))
	(setf (car frame-tracker) *frame-count-interval*)
	(format t "FPS: ~A~%" (/ *frame-count-interval* tdiff 0.001)))))

(defun blit-all (src dst)
  (let ((sw (sdl2:surface-width  src))
	(sh (sdl2:surface-height src))
	(dw (sdl2:surface-width  dst))
	(dh (sdl2:surface-height dst)))
    (sdl2:blit-surface src (sdl2:make-rect 0 0 sw sh) dst (sdl2:make-rect 0 0 dw dh))))

(defun render (frame-tracker pos)
  "Renders the current frame"
  (count-frames frame-tracker)
  (gl:clear-color 1.0 1.0 1.0 1.0)
  (gl:clear :color-buffer)
  ;; Draw a demo triangle
  (gl:begin :triangles)
  (gl:color 1.0 0.0 0.0)
  (gl:vertex pos 0)
  (gl:vertex (- pos 0.618) -1.618)
  (gl:vertex (+ pos 0.618) -1.618)
  (gl:end)
  (gl:flush))

;; Output goes to slime buffer *inferior-lisp*
;; Let over lambda
(let ((pos 1.0)
      (running t)
;      (shader-holder (make-instance 'shader-holders))
      (frame-tracker (cons 0 0)) ; Format is (frames_remaining, last_timestamp)
      (direction 0.01))

  (defun process-key (keysym)
    (let ((scancode (sdl2:scancode-value keysym))
	  (sym (sdl2:sym-value keysym))
	  (mod-value (sdl2:mod-value keysym)))
      (cond
	((sdl2:scancode= scancode :scancode-w) (format t "~a~%" "WALK"))
	((sdl2:scancode= scancode :scancode-a) (decf pos 0.1))
	((sdl2:scancode= scancode :scancode-d) (incf pos 0.1))
	; ESC scancode has to be set as 41
	((sdl2:scancode= scancode 41) (setf running nil)))
      (format t "Key sym: ~a, code: ~a, mod: ~a~%"
	      sym
	      scancode
	      mod-value)))
  
  (defun main-loop ()
    "Run the game loop that handles input, rendering through the
    render function RENDER-FN, amongst others."
    (setf running t)   
    (sdl2:with-init (:everything)
      (debug-log "Using SDL library version: ~D.~D.~D~%"
               sdl2-ffi:+sdl-major-version+
               sdl2-ffi:+sdl-minor-version+
               sdl2-ffi:+sdl-patchlevel+)
      (sdl2:with-window (win :flags '(:shown :opengl))
	(sdl2:with-gl-context (gl-context win)

	  (setup-gl win gl-context 800 600)
	  ; Setup the GL shader holders
;	  (setup-shaders shader-holder)
	  (sdl2:with-event-loop (:method :poll)
	    (:keydown (:keysym keysym)
		      (process-key keysym))
	    (:idle ()
		   (render frame-tracker pos)
		   (sdl2:gl-swap-window win)
		   (change-pos)
		   (if (not running) (sdl2:push-event :quit)))
	    (:quit () t))))))

  (defun change-pos ()
    (incf pos direction)
    (if (> (abs pos) 2) (setf direction (- 0 direction))))

  (defun quit-gui () (setf running nil))

  (defun threaded-loop ()
    (bt:make-thread (lambda () (main-loop))
		    :name "GUIRenderThread"))

  (defun set-pos (p) (setf pos p)))


;; Start the GUI
(defun gui ()
  (threaded-loop))
