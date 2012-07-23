(asdf:defsystem :glu-tessellate
  :author "Andrew Lyon <orthecreedence@gmail.com>"
  :licence "MIT"
  :version "0.1.0"
  :depends-on (#:cffi)
  :components ((:file "package")
               (:file "tessellate" :depends-on ("package"))))

