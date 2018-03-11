(defpackage :artistic)

(in-package :artistic)

(asdf:defsystem opengl-test
  :name "opengl-test"
  :version "0.1"
  :maintainer "JRH"
  :author "JRH"
  :license "MIT"
  :description "OpenGL Test for instanced draws, keyboard/mouse input and FPS test"
  :serial t
  :depends-on ( :sdl2
		:sdl2-ttf
		:bordeaux-threads
		:cl-opengl
		:cl-glu
		:cl-glut)
  
  :components ((:file "vbo")
	       (:file "shaders")
	       (:file "opengl-test" :depends-on ("shaders" "vbo"))))
