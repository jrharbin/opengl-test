(defparameter *chart-vertex-prog*
  "#version 330

// The location is 0 because that's the vertex attribute we associate with vertex positions.
layout (location = 0) in vec3 in_Position;
uniform vec2 shift;

layout (location = 1) in vec2 offset;
layout (location = 2) in vec4 ohlc;
// O = x
// H = y
// L = z
// C = w

void main()
{
  float barHeight = ohlc.w - ohlc.x; // close - open
  float barSpacing = 0.1;             // Supply this as a uniform later
  float barWidth = 0.5;                // bar width as a portion of spacing
  gl_Position = vec4(((in_Position.x * barWidth + offset.x)) * barSpacing + shift.x,
                     (in_Position.y * barHeight) + ohlc.x + shift.y, 
                     in_Position.z, 1.0);
}
")

(defparameter *chart-fragment-prog*
  "#version 330
out vec4 out_Color;
void main() 
{
    out_Color = vec4(1.0, 1.0, 1.0, 1.0); 
}
")

(defclass shader-holders ()
  ((vs :accessor vertex-shader)
   (fs :accessor fragment-shader)
   (program :accessor program)))

(defun print-if-not-empty (stream str msg)
  (if (equal str "") (format stream "~A completed OK" str)
      (error "~A ~A" msg str)))

(defmethod setup-shaders ((sh shader-holders)  &key (print-shader-status t))
  (let ((vs (gl:create-shader :vertex-shader))
	(fs (gl:create-shader :fragment-shader)))

    (if (equal vs 0) (error "Vertex shader creation failed"))
    (if (equal fs 0) (error "Fragment shader creation failed"))
    (setf (vertex-shader sh) vs)
    (setf (fragment-shader sh) fs)
    (gl:shader-source vs *chart-vertex-prog*)
    (gl:compile-shader vs)
    (gl:shader-source fs *chart-fragment-prog*)
    (gl:compile-shader fs)
    ;; If the shader doesn't compile, you can print errors with:

    (if print-shader-status
	(progn
	  (print-if-not-empty t (gl:get-shader-info-log vs)
			      "Compiling vertex shader")
	  (print-if-not-empty t (gl:get-shader-info-log fs)
			      "Compiling fragment shader")))

    (setf (program sh) (gl:create-program))
    ;; You can attach the same shader to multiple different programs.
    (gl:attach-shader (program sh) vs)
    (gl:attach-shader (program sh) fs)
    ;; Don't forget to link the program after attaching the
    ;; shaders. This step actually puts the attached shader together
    ;; to form the program.
    (gl:link-program (program sh))
    ;; If we want to render using this program object, or add
    ;; uniforms, we need to use the program. This is similar to
    ;; binding a buffer.
    (gl:use-program (program sh))))
