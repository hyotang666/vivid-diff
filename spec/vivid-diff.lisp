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
; Colored notation is printed.
#?(let ((vivid-colors:*print-vivid* t))
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

;;;; Exceptional-Situations:
#?(mismatch-sexp 0 0) => 0
#?(mismatch-sexp 0 1) :be-the diff
#?(mismatch-sexp #\a #\b) :be-the diff
#?(princ (mismatch-sexp #\a #\b))
:outputs #.(let ((cl-ansi-text:*color-mode* :8bit))
	     (cl-ansi-text:red (prin1-to-string #\a)))
