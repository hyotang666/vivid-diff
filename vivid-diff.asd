; vim: ft=lisp et
(in-package :asdf)
(defsystem "vivid-diff"
  :version
  "2.0.2"
  :depends-on
  (
   "vivid-colors"       ; Colored object printer.
   "cl-colors2"         ; Color object, implicitly depends on via vivid-colors.
   "closer-mop"         ; Wrapper for Meta Object Protocols.
   )
  :pathname
  "src/"
  :components
  ((:file "vivid-diff"))
  :author "SATO Shinichi"
  :description "Colored object diff viewer."
  :license "MIT"
  :source-control (:git "git@github.com:hyotang666/vivid-diff")
  :bug-tracker "https://github.com/hyotang666/vivid-diff/issues")

;;; These forms below are added by JINGOH.GENERATOR.
;; Ensure in ASDF for pretty printings.
(in-package :asdf)
;; Enable testing via (asdf:test-system "vivid-diff").
(defmethod component-depends-on
           ((o test-op) (c (eql (find-system "vivid-diff"))))
  (append (call-next-method) '((test-op "vivid-diff.test"))))
;; Enable passing parameter for JINGOH:EXAMINER via ASDF:TEST-SYSTEM.
(defmethod operate :around
           ((o test-op) (c (eql (find-system "vivid-diff")))
            &rest keys
            &key ((:compile-print *compile-print*))
            ((:compile-verbose *compile-verbose*)) &allow-other-keys)
  (flet ((jingoh.args (keys)
           (loop :for (key value) :on keys :by #'cddr
                 :when (find key '(:on-fails :subject :vivid) :test #'eq)
                 :collect key
                 :and
                 :collect value :else
                 :when (eq :jingoh.verbose key)
                 :collect :verbose
                 :and
                 :collect value)))
    (let ((args (jingoh.args keys)))
      (declare (special args))
      (call-next-method))))
;; Enable importing spec documentations.
(let ((system (find-system "jingoh.documentizer" nil)))
  (when system
    (load-system system)
    (defmethod perform :after
               ((o load-op) (c (eql (find-system "vivid-diff"))))
      (with-muffled-conditions (*uninteresting-conditions*)
        (handler-case (symbol-call :jingoh.documentizer :import c)
                      (error (condition)
                             (warn "Fails to import documentation of ~S.~%~A"
                                   (coerce-name c)
                                   (princ-to-string condition))))))))
