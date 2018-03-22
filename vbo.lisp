(defclass vbo ()
  ((size :reader size :initarg :size :initform 0)
   (gl-id :reader gl-id)
   (element-type :initarg :element-type :initform :float)
   (vbo-type :initarg :vbo-type :initform :static-draw)))

(defun copy-lisp-array-to-buffer (lispdata a)
  "Does the low level copy of lispdata to gl array a"
  (dotimes (i (length lispdata))
    (setf (gl:glaref a i) (aref lispdata i))))

(defun ensure-slot-size (vbo data)
  "Ensures the size is the max of given size and the data"
  (with-slots (size) vbo
    (let ((m (max (length data) size)))
      (setf size m)
      m)))

(defmethod initialize-instance :after ((vbo vbo) &key data)
  "Sets up the buffer, binds and transfers vdata into it"
  (with-slots (gl-id vbo-type element-type) vbo
    (setf gl-id (gl:gen-buffer))
    (let ((arr (gl:alloc-gl-array element-type
				  (ensure-slot-size vbo data))))
      (gl:bind-buffer :array-buffer gl-id)
      (copy-lisp-array-to-buffer data arr)
      (gl:buffer-data :array-buffer vbo-type arr)
      (gl:free-gl-array arr))))

(defmethod free ((vbo vbo))
  "Deallocate the buffer"
  (gl:delete-buffers (list (slot-value vbo 'gl-id))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass vao ()
  ((vbo :reader vbo :initarg vbo)
   (indices :reader indices :initarg indices)
   (gl-id :reader gl-id)))

(defmethod initialize-instance :after ((vao vao) &key vbo indices)
  (with-slots (gl-id) vao
    (setf gl-id (gl:gen-vertex-array))
    (gl:bind-vertex-array gl-id)
    (gl:bind-buffer :array-buffer (gl-id vbo))
    (gl:enable-vertex-attrib-array 0)
    ;; Need to set the size correctly here from the VBO!!
    (gl:vertex-attrib-pointer 0 3 :float nil 0 (cffi:null-pointer))
    (when indices
     (gl:bind-buffer :element-array-buffer (gl-id indices)))
    (gl:bind-vertex-array 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
