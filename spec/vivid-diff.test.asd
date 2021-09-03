; vim: ft=lisp et
(in-package :asdf)
(defsystem "vivid-diff.test"
  :version
  "0.1.2"
  :depends-on
  (:jingoh "vivid-diff")
  :components
  ((:file "vivid-diff"))
  :perform
  (test-op (o c) (declare (special args))
   (apply #'symbol-call :jingoh :examine :vivid-diff args)))
