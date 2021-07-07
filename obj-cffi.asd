(asdf:defsystem #:obj-cffi
  :description "An interface to Objective-C using CFFI"
  :author "Jeremy Phelps <jeremyphelps077@gmail.com>"
  :licence "MIT"
  :depends-on (#:cffi #:cl-ppcre #:closer-mop #:uiop #:trivial-garbage)
  :components
  ((:file "obj-cffi")))
