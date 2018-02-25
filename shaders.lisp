(defparameter *chart-fragment-prog*
  "#version 330

out vec4 out_Color;
smooth in vec2 pos;

uniform float angle;

void main() 
{
  mat2 rotationMatrix = mat2( cos(angle), sin(angle), -sin(angle), cos(angle) );
  vec2 rpos = mod(rotationMatrix * pos, 2.0 );
  
  if ((rpos.x > 1.0 && rpos.y > 1.0 ) || (rpos.x < 1.0 && rpos.y < 1.0))
    out_Color = vec4(0.1, 0.1, 0.1, 1.0); 
  else
    out_Color = vec4(0.5, 0.5, 0.7, 1.0);
}")

(defparameter *chart-vertex-prog*
  "#version 330

// The location is 0 because that's the vertex attribute we associate with vertex positions.
layout (location = 0) in vec3 in_Position;

uniform mat4 projectionMatrix;
uniform float angle;

// This is interpolated and used in the fragment shader.
smooth out vec2 pos;

void main()
{
  mat2 rotationMatrix = mat2(cos(angle), sin(angle), -sin(angle), cos(angle));
  float scaleFactor = 1.0 + 0.5 * sin(1.75 * angle);
  vec2 vertPos = scaleFactor * rotationMatrix * in_Position.xy;
  pos = vertPos * 5.0;

  gl_Position = projectionMatrix * vec4(vertPos, 0.0, 1.0); 
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
