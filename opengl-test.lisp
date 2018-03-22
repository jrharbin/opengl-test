(defparameter *frame-count-interval* 50)

(defvar *square-vbo*)
(defvar *offset-vbo*)
(defvar *index-buf*)
(defvar *geometry-vao*)
(defvar *ohlc-vbo*)

(defparameter *instance-count* 6)

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
  (gl:clear-color 0.0 0.0 0.0 0.0)
  (gl:clear-depth 1.0))

(defun setup-state ()
  "Sets up the state for rendering a single square"
  (setf *square-vbo* (make-instance 'vbo
				    :data '#(0.0 0.0 -1.0
					     0.0 1.0 -1.0
					     1.0 0.0 -1.0
					     1.0 1.0 -1.0)))
  (setf *index-buf* (make-instance 'vbo 
				   :element-type :unsigned-short
				   :data '#(0 2 1 1 2 3)))

  (setf *offset-vbo* (make-instance 'vbo
				    :data '#(1.0
					     2.0
					     3.0
					     4.0
					     5.0
					     6.0)))
  
  (setf *ohlc-vbo* (make-instance 'vbo
				  :data '#(0.2 0.7  0.2 0.4
					   0.3 0.7  0.2 0.2
					   0.05 0.7 0.2 0.6
					   0.4 0.7  0.2 0.7
					   0.3 0.7  0.2 0.8
					   0.1 0.7  0.2 0.9)))
  
  (setf *geometry-vao* (make-instance 'vao :vbo *square-vbo*
					   :indices *index-buf*)))

(defun count-frames (frame-tracker)
  (decf (car frame-tracker))
  (if (<= (car frame-tracker) 0)
      (let ((tdiff (- (sdl2:get-ticks) (cdr frame-tracker))))
	(setf (cdr frame-tracker) (sdl2:get-ticks))
	(setf (car frame-tracker) *frame-count-interval*)
	(format t "FPS: ~A~%" (/ *frame-count-interval* tdiff 0.001)))))

(defun render-pipeline-elt (shader-holder wick pos vpos)
  (gl:uniformf (gl:get-uniform-location (program shader-holder) "shift")
	       pos vpos)

  (gl:uniformf (gl:get-uniform-location (program shader-holder) "drawElement")
	       (if wick -1.0 1.0))

  (let ((shift-loc (gl:get-attrib-location (program shader-holder) "offset"))
	(ohlc-loc (gl:get-attrib-location (program shader-holder) "ohlc")))
    (gl:bind-buffer :array-buffer (gl-id *offset-vbo*))
    (gl:vertex-attrib-pointer shift-loc 1 :float 0 0 0)
    (gl:enable-vertex-attrib-array shift-loc)
    (cl-opengl-bindings:vertex-attrib-divisor shift-loc 1)

    (gl:bind-buffer :array-buffer (gl-id *ohlc-vbo*))
    (gl:vertex-attrib-pointer ohlc-loc 4 :float 0 0 0)
    (gl:enable-vertex-attrib-array ohlc-loc)
    (cl-opengl-bindings:vertex-attrib-divisor ohlc-loc 1))
  
  (gl:bind-vertex-array (gl-id *geometry-vao*))
  (gl:draw-elements-instanced :triangles (gl:make-null-gl-array :unsigned-short)
			      *instance-count* :count 6))

(defun render (shader-holder frame-tracker pos vpos)
  "Renders the current frame"
  (count-frames frame-tracker)
  (gl:clear-color 0.0 0.0 0.2 1.0)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (gl:disable :cull-face)
  (gl:use-program (program shader-holder)) 
  (render-pipeline-elt shader-holder t pos vpos)
  (render-pipeline-elt shader-holder nil pos vpos)
  (gl:flush))

;; Output goes to slime buffer *inferior-lisp*
;; Let over lambda
(let ((pos 0.0)
      (vpos 0.0)
      (running t)
      (shader-holder (make-instance 'shader-holders))
      (frame-tracker (cons 0 0)) ; Format is (frames_remaining, last_timestamp)
      (direction 0.005))

  (defun process-key (keysym)
    (let ((scancode (sdl2:scancode-value keysym))
	  (sym (sdl2:sym-value keysym))
	  (mod-value (sdl2:mod-value keysym)))
      (cond
	;; Key strokes for navigation
	((sdl2:scancode= scancode :scancode-left) (decf pos 0.1))
	((sdl2:scancode= scancode :scancode-right) (incf pos 0.1))
	((sdl2:scancode= scancode :scancode-up) (incf vpos 0.1))
	((sdl2:scancode= scancode :scancode-down) (decf vpos 0.1))
	;; 75 is PG up, 78 is PG down
	((sdl2:scancode= scancode 75) (incf vpos 0.5))
	((sdl2:scancode= scancode 78) (decf vpos 0.5))
	;; ESC scancode has to be set as 41
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
          ; Setup the GL shader holders - if new system
	  (setup-shaders shader-holder)
	  (setup-state)
	  (sdl2:with-event-loop (:method :poll)
	    (:keydown (:keysym keysym)
		      (process-key keysym))
	    (:idle ()
		   (render shader-holder frame-tracker pos vpos)
		   (sdl2:gl-swap-window win)
		   (change-pos)
		   (if (not running) (sdl2:push-event :quit)))
	    (:quit () t))))))

  (defun change-pos ()
    (incf pos direction)
    (if (> (abs pos) 0.5) (setf direction (- 0 direction))))

  (defun quit-gui () (setf running nil))

  (defun threaded-loop ()
    (bt:make-thread (lambda () (main-loop))
		    :name "GUIRenderThread"))

  (defun set-pos (p) (setf pos p)))

;; Start the GUI
(defun gui ()
  (threaded-loop))
