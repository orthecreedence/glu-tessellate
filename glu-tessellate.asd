(asdf:defsystem :glu-tessellate
  :author "Andrew Lyon <orthecreedence@gmail.com>"
  :licence "MIT"
  :version "0.1.0"
  :description "A simple triangulation engine that wraps around GLU's tessellation system."
  :depends-on (#:cffi)
  :components ((:file "package")
               (:file "tessellate" :depends-on ("package"))))

