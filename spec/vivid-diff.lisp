(defpackage :vivid-diff.spec
  (:use :cl :jingoh :vivid-diff))
(in-package :vivid-diff.spec)
(setup :vivid-diff)

(requirements-about DIFF :doc-type STRUCTURE)

;;;; Description:
; Abstract structure for every DIFF object.

;;;; Class Precedence List: (case in SBCL)
; diff structure-object slot-object t

;;;; Effective Slots:

; OBJECT [Type] T

;;;; Notes:

(requirements-about MARKUP :doc-type function)

;;;; Description:
; Markup OBJECT as DIFF.

#+syntax (MARKUP object) ; => result

;;;; Arguments and Values:

; object := T

; result := DIFF.

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:
; When *PRINT-READABLY* and *PRINT-ESCAPSE* is NIL and VIVID-COLORS:*PRINT-VIVID* is T,
; and VIVID-COLORS:*VPRINT-DISPATCH* bound by :vivid-diff dispatch-table,
; Colored notation is printed.
#?(let ((vivid-colors:*print-vivid* t)
	(vivid-colors:*vprint-dispatch* (vivid-colors:find-vprint-dispatch :vivid-diff)))
    (write (markup 0) :readably nil :escape nil))
:outputs #.(let ((cl-ansi-text:*color-mode* :8bit))
	     (cl-ansi-text:red "0"))

;;;; Exceptional-Situations:

(requirements-about DIFF-OBJECT :doc-type function)

;;;; Description:
; Accessor for DIFF.

#+syntax (DIFF-OBJECT sb-kernel:instance) ; => result

#+setf
(SETF (DIFF-OBJECT SB-KERNEL:INSTANCE) SB-KERNEL::VALUE) ; => new-value

;;;; Arguments and Values:

; instance := DIFF, otherwise implementation dependent condition.
#?(diff-object "not diff object") :signals condition

; result := T

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about DIFF-COMPARABLE :doc-type STRUCTURE)

;;;; Description:
; Abstract diff structure for comparables.

;;;; Class Precedence List: (case in SBCL)
; diff-comparable diff structure-object slot-object t

;;;; Effective Slots:

; ORIGIN [Type] T

; OBJECT [Type] T

;;;; Notes:

(requirements-about DIFF-COMPARABLE-ORIGIN :doc-type function)

;;;; Description:

#+syntax (DIFF-COMPARABLE-ORIGIN sb-kernel:instance) ; => result

#+setf
(SETF (DIFF-COMPARABLE-ORIGIN SB-KERNEL:INSTANCE) SB-KERNEL::VALUE) ; => new-value

;;;; Arguments and Values:

; instance := DIFF-COMPARABLE

; result := T

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about MISMATCH-SEXP :doc-type function)

;;;; Description:
; If S-Expression is mismatch, DIFF object is returned,
; otherwise actual is returned.

#+syntax (MISMATCH-SEXP actual expected) ; => result

;;;; Argument Precedence Order:
; actual expected

;;;; Method signature:
#+signature(MISMATCH-SEXP (ACTUAL HASH-TABLE) (EXPECTED HASH-TABLE))
#+signature(MISMATCH-SEXP (ACTUAL STANDARD-OBJECT) (EXPECTED STANDARD-OBJECT))
#+signature(MISMATCH-SEXP (ACTUAL STRUCTURE-OBJECT) (EXPECTED STRUCTURE-OBJECT))
#+signature(MISMATCH-SEXP (ACTUAL ARRAY) (EXPECTED ARRAY))
#+signature(MISMATCH-SEXP (ACTUAL VECTOR) (EXPECTED VECTOR))
#+signature(MISMATCH-SEXP (ACTUAL BIT-VECTOR) (EXPECTED BIT-VECTOR))
#+signature(MISMATCH-SEXP (ACTUAL NUMBER) (EXPECTED NUMBER))
#+signature(MISMATCH-SEXP (ACTUAL PATHNAME) (EXPECTED PATHNAME))
#+signature(MISMATCH-SEXP (ACTUAL STRING) (EXPECTED STRING))
#+signature(MISMATCH-SEXP (ACTUAL LIST) (EXPECTED LIST))
#+signature(MISMATCH-SEXP (ACTUAL SYMBOL) (EXPECTED SYMBOL))
#+signature(MISMATCH-SEXP :AROUND (ACTUAL T) (EXPECTED T))
#+signature(MISMATCH-SEXP (ACTUAL T) (EXPECTED T))

;;;; Arguments and Values:

; actual := T

; expected := T

; result := (or T DIFF)

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:
; As the name explains, MISMATCH-SEXP is for S-Expression.
; i.e. macro expansion form especially uninterned symbol.
#?(mismatch-sexp '#:a '#:b)
:satisfies (lambda (x)
	     (& (symbolp x)
		(null (symbol-package x))
		(equal "A" (symbol-name x))))

#?(let ((expanded-form1 (macroexpand-1 #0='(case 0)))
	(expanded-form2 (macroexpand-1 #0#)))
    (values (tree-equal expanded-form1 expanded-form2)
	    (tree-equal expanded-form1 (mismatch-sexp expanded-form1 expanded-form2))))
:values (nil t)

;;;; Exceptional-Situations:
;; Numbers
#?(mismatch-sexp 0 0) => 0
#?(mismatch-sexp 0 1) :be-the diff
#?(diff-print (mismatch-sexp 0 1))
:outputs #.(let ((cl-ansi-text:*color-mode* :8bit))
	     (cl-ansi-text:red "0"))

;; Characters
#?(mismatch-sexp #\a #\b) :be-the diff
#?(diff-print (mismatch-sexp #\a #\b))
:outputs #.(let ((cl-ansi-text:*color-mode* :8bit))
	     (cl-ansi-text:red (prin1-to-string #\a)))

;; Symbols
#?(mismatch-sexp t t) => T
#?(mismatch-sexp t nil) :be-the diff
#?(diff-print (mismatch-sexp t nil))
:outputs #.(let ((cl-ansi-text:*color-mode* :8bit))
	     (cl-ansi-text:red "T"))

#?(mismatch-sexp :key nil)
:satisfies (lambda (diff)
	     (& (typep diff 'diff)
		(equal (with-output-to-string (out) (diff-print diff out))
		       (let ((cl-ansi-text:*color-mode* :8bit))
			 (cl-ansi-text:red ":KEY")))))

#?(mismatch-sexp :key '#:uninterned)
:satisfies (lambda (diff)
	     (& (typep diff 'diff)
		(equal (with-output-to-string (out) (diff-print diff out))
		       (let ((cl-ansi-text:*color-mode* :8bit))
			 (cl-ansi-text:red ":KEY")))))

;; List
#?(mismatch-sexp '(:a) '(:a)) => (:A)
,:test equal

#?(mismatch-sexp '(:a) '(:b))
:satisfies (lambda (diff)
	     (& (typep diff '(cons diff null))
		(equal (with-output-to-string (out) (diff-print diff out))
		       (format nil "(~A)"
			       (let ((cl-ansi-text:*color-mode* :8bit))
				 (cl-ansi-text:red (prin1-to-string ':a)))))))

; Dot list.
#?(mismatch-sexp '(a) '(a . b))
:satisfies (lambda (diff)
	     (& (typep diff '(cons (eql a) diff))
		(equal (with-output-to-string (out) (diff-print diff out))
		       (format nil "(A . ~A)"
			       (let ((cl-ansi-text:*color-mode* :8bit))
				 (cl-ansi-text:red "NIL"))))))

; Lesser
#?(mismatch-sexp '(a) '(a b))
:satisfies (lambda (diff)
	     (& (typep diff '(cons (eql a) diff))
		(equal (with-output-to-string (out) (diff-print diff out))
		       (format nil "(A . ~A)"
			       (let ((cl-ansi-text:*color-mode* :8bit))
				 (cl-ansi-text:red "NIL"))))))

; Greater
#?(mismatch-sexp '(a b) '(a))
:satisfies (lambda (diff)
	     (& (typep diff '(cons (eql a) diff))
		(let ((output (with-output-to-string (out) (diff-print diff out))))
		  (& (equal "(A . (B))" (ppcre:regex-replace-all "\\x1B[^m]+m" output ""))
		     (let ((red-escape-sequence
			     (search (let ((cl-ansi-text:*color-mode* :8bit))
				       (cl-ansi-text:make-color-string cl-colors2:+red+))
				     output)))
		       (& (< red-escape-sequence
			     (search cl-ansi-text:+reset-color-string+ output))))))))

;; String
#?(mismatch-sexp "a" "a") => "a"
,:test equal

#?(mismatch-sexp "a" "b")
:satisfies (lambda (diff)
	     (& (typep diff 'diff)
		(equal (with-output-to-string (out) (diff-print diff out))
		       (format nil "\"~A\""
			       (cl-ansi-text:red "a" :style :background)))))

; Lesser
#?(mismatch-sexp "a" "ab")
:satisfies (lambda (diff)
	     (& (typep diff 'diff)
		(equal (with-output-to-string (out) (diff-print diff out))
		       (format nil "\"a~A\""
			       (cl-ansi-text:red ":NULL" :effect :blink)))))

; Greater
#?(mismatch-sexp "ab" "a")
:satisfies (lambda (diff)
	     (& (typep diff 'diff)
		(equal (with-output-to-string (out) (diff-print diff out))
		       (format nil "\"a~A\""
			       (cl-ansi-text:red "b" :style :background)))))

;; Pathname
#?(mismatch-sexp #P"hoge" #P"hoge") => #P"hoge"
,:test equal

#?(mismatch-sexp #P"hoge" #P"fuga")
:satisfies (lambda (diff)
	     (& (typep diff 'diff)
		(equal (with-output-to-string (out) (diff-print diff out))
		       (format nil "#P\"~A\""
			       (cl-ansi-text:red "hoge" :style :background)))))

;; Bit-vector
#?(mismatch-sexp #*1101 #*1101) => #*1101
,:test equal

#?(mismatch-sexp #*1101 #*1111)
:satisfies (lambda (diff)
	     (& (typep diff 'diff)
		(equal (with-output-to-string (out) (diff-print diff out))
		       (let ((cl-ansi-text:*color-mode* :8bit))
			 (cl-ansi-text:red "#*1101")))))

;; Vector
#?(mismatch-sexp #() #()) => #()
,:test equalp

; type mismatch.
#?(mismatch-sexp "a" #(#\a))
:satisfies (lambda (diff)
	     (& (typep diff 'diff)
		(equal (with-output-to-string (out) (diff-print diff out))
		       (let ((cl-ansi-text:*color-mode* :8bit))
			 (cl-ansi-text:red "\"a\"")))))

; element mismatch.
#?(mismatch-sexp #(hoge) #(#\a))
:satisfies (lambda (diff)
	     (& (typep diff '(vector t *))
		(equal (with-output-to-string (out) (diff-print diff out))
		       (format nil "#(~A)"
			       (let ((cl-ansi-text:*color-mode* :8bit))
				 (cl-ansi-text:red "HOGE"))))))

; Lesser
#?(mismatch-sexp #(a) #(a b))
:satisfies (lambda (diff)
	     (& (typep diff '(vector t *))
		(equal (with-output-to-string (out) (diff-print diff out))
		       (format nil "#(A ~A)"
			       (let ((cl-ansi-text:*color-mode* :8bit))
				 (cl-ansi-text:red ":NULL" :effect :blink))))))

; Greater
#?(mismatch-sexp #(a b) #(a))
:satisfies (lambda (diff)
	     (& (typep diff '(vector t *))
		(equal (with-output-to-string (out) (diff-print diff out))
		       (format nil "#(A ~A)"
			       (let ((cl-ansi-text:*color-mode* :8bit))
				 (cl-ansi-text:red "B"))))))

;; Array
#?(mismatch-sexp #0A() #0A()) => #0A()
,:test equalp

; dimension mismatch
#?(mismatch-sexp #0A() #1A())
:satisfies (lambda (diff)
	     (& (typep diff 'diff)
		(let ((output (with-output-to-string (out) (diff-print diff out))))
		  (& (uiop:string-prefix-p (let ((cl-ansi-text:*color-mode* :8bit))
					     (cl-ansi-text:make-color-string cl-colors2:+red+))
					   output)
		     (uiop:string-suffix-p output cl-ansi-text:+reset-color-string+)
		     (equal "(:DIFFERENT-DIMENSIONS :EXPECTED (0) :ACTUAL NIL #0ANIL)"
			    (ppcre:regex-replace-all "\\x1B[^m]+m" output ""))))))

; Lesser
#?(mismatch-sexp #2A((a b)) #2A((a b) (c d)))
:satisfies (lambda (diff)
	     (& (typep diff 'diff)
		(let ((output (with-output-to-string (out) (diff-print diff out))))
		  (& (uiop:string-prefix-p (let ((cl-ansi-text:*color-mode* :8bit))
					     (cl-ansi-text:make-color-string cl-colors2:+red+))
					   output)
		     (uiop:string-suffix-p output cl-ansi-text:+reset-color-string+)
		     (equal "(:DIFFERENT-DIMENSIONS :EXPECTED (2 2) :ACTUAL (1 2) #2A((A B)))"
			    (ppcre:regex-replace-all "\\x1B[^m]+m" output ""))))))

#?(mismatch-sexp #2A((a b)) #2A((a c)))
:satisfies (lambda (diff)
	     (& (typep diff 'array)
		(equal (with-output-to-string (out) (diff-print diff out))
		       (format nil "#2A((A ~A))"
			       (let ((cl-ansi-text:*color-mode* :8bit))
				 (cl-ansi-text:red "B"))))))

;; Strucuture

#?(mismatch-sexp cl-colors2:+red+ cl-colors2:+red+)
=> #.cl-colors2:+red+
,:test equalp

#?(mismatch-sexp cl-colors2:+red+ cl-colors2:+blue+)
:satisfies (lambda (diff)
	     (& (typep diff 'diff)
		(equal (with-output-to-string (out) (diff-print diff out))
		       (format nil "#S(CL-COLORS2:RGB :RED ~A :GREEN 0 :BLUE ~A)"
			       (let ((cl-ansi-text:*color-mode* :8bit))
				 (cl-ansi-text:red "1"))
			       (let ((cl-ansi-text:*color-mode* :8bit))
				 (cl-ansi-text:red "0"))))))

;; Standard-object

#?(defclass test () ((slot :initarg :slot))) :be-the standard-class

#?(mismatch-sexp (make-instance 'test :slot :a) (make-instance 'test :slot :a))
:satisfies (lambda (result)
	     (& (typep result 'test)
		(equal :a (slot-value result 'slot))))

; Not bound.
#?(mismatch-sexp (make-instance 'test) (make-instance 'test :slot :a))
:satisfies (lambda (diff)
	     (& (typep diff 'diff)
		(equal (with-output-to-string (out) (diff-print diff out))
		       (format nil "#<TEST :SLOT ~A>"
			       (let ((cl-ansi-text:*color-mode* :8bit))
				 (cl-ansi-text:red ":UNBOUND" :effect :blink))))))

; Should be unbound.
#?(mismatch-sexp (make-instance 'test :slot :a) (make-instance 'test))
:satisfies (lambda (diff)
	     (& (typep diff 'diff)
		(equal (with-output-to-string (out) (diff-print diff out))
		       (format nil "#<TEST :SLOT ~A>"
			       (let ((cl-ansi-text:*color-mode* :8bit))
				 (cl-ansi-text:red ":A"))))))

; Mismatch value.
#?(mismatch-sexp (make-instance 'test :slot :a) (make-instance 'test :slot :b))
:satisfies (lambda (diff)
	     (& (typep diff 'diff)
		(equal (with-output-to-string (out) (diff-print diff out))
		       (format nil "#<TEST :SLOT ~A>"
			       (let ((cl-ansi-text:*color-mode* :8bit))
				 (cl-ansi-text:red ":A"))))))

;; Hash-table
#?(mismatch-sexp (make-hash-table) (make-hash-table)) => #.(make-hash-table)
,:test equalp

; Lesser
#?(mismatch-sexp (make-hash-table) (alexandria:plist-hash-table '(:a :b)))
:satisfies (lambda (diff)
	     (& (typep diff 'diff)
		(equal (with-output-to-string (out) (diff-print diff out))
		       (format nil "#<HASH-TABLE :A ~A>"
			       (let ((cl-ansi-text:*color-mode* :8bit))
				 (cl-ansi-text:red ":MISSING" :effect :blink))))))

; Greater
#?(mismatch-sexp (alexandria:plist-hash-table '(:a :b)) (make-hash-table))
:satisfies (lambda (diff)
	     (& (typep diff 'diff)
		(equal (with-output-to-string (out) (diff-print diff out))
		       (format nil "#<HASH-TABLE :A ~A>"
			       (let ((cl-ansi-text:*color-mode* :8bit))
				 (cl-ansi-text:red ":B"))))))

; Mismatch value.
#?(mismatch-sexp (alexandria:plist-hash-table '(:a :b)) (alexandria:plist-hash-table '(:a "b")))
:satisfies (lambda (diff)
	     (& (typep diff 'diff)
		(equal (with-output-to-string (out) (diff-print diff out))
		       (format nil "#<HASH-TABLE :A ~A>"
			       (let ((cl-ansi-text:*color-mode* :8bit))
				 (cl-ansi-text:red ":B"))))))

