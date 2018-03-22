(defclass sel-buffer ()
  ((buf :reader sel-buf)
   (lookup-coord :initform (make-hash-table))))

(defmethod initialize-instance :after ((sel-buffer sel-buffer) &key)
  "Set up the selection buffer"
  (let ((sb (gl:gen-framebuffer)))
    ;; Store the generated buffer
    (setf (slot-value sel-buffer 'buf) sb)
    ;; Bind the framebuffer to the target
    (gl:bind-framebuffer :framebuffer sb)))

(defmethod selection-render ((sel-buffer sel-buffer) render-f)
  "Performs the rendering for the selection buffer using the given function render-f"
  ;; Set up the buffer and clear it
  (gl:clear (slot-value sel-buffer 'buf))
  ;; Use a program with the same vertex shader
  ;; And the fragment shader that encodes the bar ID
  (funcall render-f))
;; Store all the objects in the lookup hash table - how regularly do we need this
;; in practice

(defmethod selection-getpixel ((sel-buffer sel-buffer) x y)
  "Set the pixel selection count"
  (let ((col (gl:read-pixels x y 1 1 format type))
	(lookupt (slot-value sel-buffer 'lookup-coord)))
    (gethash col lookupt)))
