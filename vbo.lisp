(defclass vbo ()
  ((size :reader size :initarg :size)
   (gl-id :reader gl-id)
   (element-type :initarg :element-type :initform :float)
   (vbo-type :initarg :vbo-type :initform :static-draw)))

(defun copy-lisp-array-to-buffer (lispdata a)
  "Does the low level copy of lispdata to gl array a"
  (dotimes (i (length lispdata))
    (setf (gl:glaref a i) (aref lispdata i))))

(defun get-size (vbo data)
  (max (length data) (slot-value vbo 'size)))

(defmethod initialize-instance :after ((vbo vbo) &key data)
  "Sets up the buffer, binds and transfers vdata into it"
  (setf (slot-value vbo 'gl-id) (gl:gen-buffer))
  (let ((arr (gl:alloc-gl-array (slot-value vbo 'element-type)
				(get-size vbo data)))
	(buf-id (slot-value vbo 'gl-id)))
    (gl:bind-buffer :array-buffer buf-id)
    (copy-lisp-array-to-buffer data arr)
    (gl:buffer-data :array-buffer (slot-value vbo 'vbo-type) arr)
    (gl:free-gl-array arr)))

(defmethod free ((vbo vbo))
  "Deallocate the buffer"
  (gl:delete-buffers (list (slot-value vbo 'gl-id))))

(defmethod vbo-get ((vbo vbo) index)
  "Doesn't work yet - fix this"
  (let ((arr (gl:alloc-gl-array :float (slot-value vbo 'size))))
    (gl:buffer-data :array-buffer :static-draw arr)
    (gl:glaref arr index)
    (gl:free-gl-array arr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass vao ()
  ((vbo :reader vbo :initarg vbo)
   (indices :reader indices :initarg indices)
   (gl-id :reader gl-id)))

(defmethod initialize-instance :after ((vao vao) &key vbo indices)
  (setf (slot-value vao 'gl-id) (gl:gen-vertex-array))
  (gl:bind-vertex-array (slot-value vao 'gl-id))
  (gl:bind-buffer :array-buffer (gl-id vbo))
  (gl:enable-vertex-attrib-array 0)
  ; Need to set the size correctly here from the VBO!!
  (gl:vertex-attrib-pointer 0 3 :float nil 0 (cffi:null-pointer))
  (gl:bind-buffer :element-array-buffer (gl-id indices))
  (gl:bind-vertex-array 0))
