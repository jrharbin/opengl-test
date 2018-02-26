(defparameter *frame-count-interval* 50)

(defvar *triangle-vbo*)
(defvar *index-buf*)
(defvar *geometry-vao*)

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

(defun copy-lisp-array-to-buffer (lispdata a)
  (dotimes (i (length lispdata))
    (setf (gl:glaref a i) (aref lispdata i))))

(defun setup-state ()
  "Sets up the state for rendering the triangle"
  (setf *triangle-vbo* (gl:gen-buffer))
  (setf *index-buf* (gl:gen-buffer))
  (format t "Triangle VBO: ~A~%" *triangle-vbo*)
  (let ((arr (gl:alloc-gl-array :float 12))
	(verts #(-0.5 -0.5 0.0 
		 -0.5 0.5 0.0 
		 0.5 -0.5 0.0 
		 0.5 0.5 0.0)))
    (gl:bind-buffer :array-buffer *triangle-vbo*)
    (copy-lisp-array-to-buffer verts arr)
    (gl:buffer-data :array-buffer :static-draw arr)
    (gl:free-gl-array arr))
  (gl:bind-buffer :array-buffer 0)

  (let ((arr (gl:alloc-gl-array :unsigned-short 6))
	(indexes #(0 2 1 1 2 3)))
    (gl:bind-buffer :element-array-buffer *index-buf*)
    (copy-lisp-array-to-buffer indexes arr)
    (gl:buffer-data :element-array-buffer :static-draw arr)
    (gl:free-gl-array arr))
  (gl:bind-buffer :element-array-buffer 0)

  ;; Vertex array objects manage which vertex attributes are
  ;; associated with which data buffers. 
  (setf *geometry-vao* (gl:gen-vertex-array))
  (gl:bind-vertex-array *geometry-vao*)

  ;; To associate our VBO data with this VAO, we bind it, specify
  ;; which vertex attribute we want to associate it with, and specify
  ;; where the data comes from.
  (gl:bind-buffer :array-buffer *triangle-vbo*)
  ;; In this program, we use attribute 0 for position. If you had
  ;; per-vertex normals, you could use a different attribute for those
  ;; as well.
  (gl:enable-vertex-attrib-array 0)
  ;; Using a null pointer as the data source indicates that we want
  ;; the vertex data to come from the currently bound array-buffer.
  (gl:vertex-attrib-pointer 0 2 :float nil 0 (cffi:null-pointer))

  ;; To associate an element array with this VAO, all we need to do is
  ;; bind the element array buffer we want to use.
  (gl:bind-buffer :element-array-buffer *index-buf*)

  ;; Once we're done, we can unbind the VAO, and rebind it when we want to render it.
  (gl:bind-vertex-array 0))

(defun count-frames (frame-tracker)
  (decf (car frame-tracker))
  (if (<= (car frame-tracker) 0)
      (let ((tdiff (- (sdl2:get-ticks) (cdr frame-tracker))))
	(setf (cdr frame-tracker) (sdl2:get-ticks))
	(setf (car frame-tracker) *frame-count-interval*)
	(format t "FPS: ~A~%" (/ *frame-count-interval* tdiff 0.001)))))

(defun render-old (frame-tracker pos vpos)
  (declare (ignore vpos))
  "Renders the current frame the old way"
  (count-frames frame-tracker)
  (gl:clear-color 0.0 0.0 0.0 0.0)
  (gl:clear :color-buffer)
  ;; Draw a demo triangle
  (gl:begin :triangles)
  (gl:color 1.0 0.0 0.0)
  (gl:vertex pos 0)
  (gl:vertex (- pos 0.618) -1.618)
  (gl:vertex (+ pos 0.618) -1.618)
  (gl:end)
  (gl:flush))

(defun render (shader-holder frame-tracker pos vpos)
  (declare (ignore pos vpos))
  "Renders the current frame"
  (count-frames frame-tracker)

  (gl:disable :cull-face)
  (gl:use-program (program shader-holder))
  (gl:clear-color 0.0 0.0 0.2 1.0)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (gl:bind-vertex-array *geometry-vao*)
  (gl:draw-elements :triangles (gl:make-null-gl-array :unsigned-short) :count 6)
  (gl:flush))

;; Output goes to slime buffer *inferior-lisp*
;; Let over lambda
(let ((pos 1.0)
      (vpos 0.0)
      (running t)
      (shader-holder (make-instance 'shader-holders))
      (frame-tracker (cons 0 0)) ; Format is (frames_remaining, last_timestamp)
      (direction 0.02))

  (defun process-key (keysym)
    (let ((scancode (sdl2:scancode-value keysym))
	  (sym (sdl2:sym-value keysym))
	  (mod-value (sdl2:mod-value keysym)))
      (cond
	((sdl2:scancode= scancode :scancode-a) (decf pos 0.1))
	((sdl2:scancode= scancode :scancode-d) (incf pos 0.1))
	((sdl2:scancode= scancode :scancode-w) (incf vpos 0.1))
	((sdl2:scancode= scancode :scancode-s) (decf vpos 0.1))
	
	; ESC scancode has to be set as 41
	((sdl2:scancode= scancode 41) (setf running nil)))
      (format t "Key sym: ~a, code: ~a, mod: ~a~%"
	      sym
	      scancode
	      mod-value)))
  
  (defun main-loop (&key (use-old t))
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
	  (if (not use-old) (setup-shaders shader-holder))
	  (setup-state)
	  (sdl2:with-event-loop (:method :poll)
	    (:keydown (:keysym keysym)
		      (process-key keysym))
	    (:idle ()
		   (if use-old
		       (render-old frame-tracker pos vpos)
		       (render shader-holder frame-tracker pos vpos))
		   (sdl2:gl-swap-window win)
		   (change-pos)
		   (if (not running) (sdl2:push-event :quit)))
	    (:quit () t))))))

  (defun change-pos ()
    (incf pos direction)
    (if (> (abs pos) 2) (setf direction (- 0 direction))))

  (defun quit-gui () (setf running nil))

  (defun threaded-loop (&key (use-old t))
    (bt:make-thread (lambda () (main-loop :use-old use-old))
		    :name "GUIRenderThread"))

  (defun set-pos (p) (setf pos p)))


;; Start the GUI
(defun gui (&key (use-old t))
  (threaded-loop :use-old use-old))
