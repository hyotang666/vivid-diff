(in-package :cl-user)

(defpackage :vivid-diff
  (:use :cl)
  (:export ;;;; MAIN API
           #:mismatch-sexp
           #:diff-print)
  (:export ;;;; For hackers to extend.
           #:diff ; structure name
           #:markup ; constructor
           #:diff-object ; accessor
           #:diff-comparable ; structure name
           #:diff-comparable-origin ; accessor
           #:nothing ; structure name.
           #:markup-nothing ; constructor.
           ))

(in-package :vivid-diff)

(declaim (optimize speed))

;;;; DIFF OBJECTS

(defstruct (diff (:constructor markup (object))) object)

(defstruct (diff-comparable (:include diff)) origin)

(defstruct (string-diff (:include diff-comparable)
                        (:constructor markup-string (object origin))))

(defstruct (pathname-diff (:include diff-comparable)
                          (:constructor markup-pathname (object origin))))

(defstruct (object-diff (:include diff-comparable)
                        (:constructor markup-object (object origin))))

(defstruct (hash-table-diff (:include diff)
                            (:constructor markup-hash-table (object))))

(defstruct (nothing (:constructor markup-nothing (object)) (:include diff)))

;;;; VPRINTERS

(defun vprint-nothing (output diff)
  (vivid-colors:put (diff-object diff) output
                    :color (list cl-colors2:+red+ :effect :blink))
  (values))

(defun vprint-diff (output diff)
  (let ((vivid-colors:*color* (list cl-colors2:+red+)))
    (vivid-colors:vprint (diff-object diff) output t))
  (values))

(defmethod print-object ((object diff) output)
  (cond ((or *print-readably* *print-escape*) (call-next-method))
        (t (diff-print object output))))

(defun vprint-string-diff (output diff)
  (let* ((string (string-diff-object diff))
         (pos (mismatch (string-diff-origin diff) string))
         (expected-in-bounds-p
          (array-in-bounds-p (string-diff-origin diff) pos))
         (actual-in-bounds-p (array-in-bounds-p string pos)))
    (declare (type string string))
    (if expected-in-bounds-p
        (if actual-in-bounds-p
            ;; simply different. e.g. "foobar" "foohoge"
            #0=(vivid-colors:put-strings
                 (list (subseq string 0 pos)
                       (list (subseq string pos) cl-colors2:+red+
                             :style :background))
                 output)
            ;; too much short. e.g. "foobar" "foo"
            (vivid-colors:put-strings
              (list string (list ":NULL" cl-colors2:+red+ :effect :blink))
              output))
        (if actual-in-bounds-p
            ;; too much long. e.g. "foo" "foobar"
            #0#
            ;; simply different e.g. "foo" "bar"
            (vivid-colors:put string output :color cl-colors2:+red+)))))

(defun vprint-pathname-diff (output diff)
  (let ((diff (pathname-diff-object diff)) (origin (pathname-diff-origin diff)))
    (labels ((diff? (a b &key (test #'eql) &aux (test (coerce test 'function)))
               (if (funcall test a b)
                   a
                   (etypecase a
                     (symbol (markup a))
                     (string (markup-string a b))))))
      (if (or (not (eq (pathname-host diff) (pathname-host origin)))
              (not (eq (pathname-version diff) (pathname-version origin))))
          (vivid-colors:vprint-logical-block (output nil :prefix "#P")
            (vivid-colors:vprint
              (list :host (diff? (pathname-host diff) (pathname-host origin))
                    :device (diff? (pathname-device diff)
                                   (pathname-device origin))
                    :directory (mismatch-sexp (pathname-directory diff)
                                              (pathname-directory origin))
                    :name (diff? (pathname-name diff) (pathname-name origin)
                                 :test #'equal)
                    :type (diff? (pathname-type diff) (pathname-type origin)
                                 :test #'equal)
                    :version (diff? (pathname-version diff)
                                    (pathname-version origin)))
              output))
          (vivid-colors:vprint-logical-block (output nil :prefix "#P")
            (vivid-colors:vprint
              (mismatch-sexp (namestring diff) (namestring origin)) output))))))

(defun vprint-object-diff (output diff)
  (vivid-colors:vprint-logical-block (output nil :prefix
                                      (if (typep (object-diff-origin diff)
                                                 'structure-object)
                                          "#S("
                                          "#<")
                                      :suffix
                                      (if (typep (object-diff-origin diff)
                                                 'structure-object)
                                          ")"
                                          ">"))
    (vivid-colors:vprint (type-of (object-diff-origin diff)) output)
    (write-char #\Space output)
    (vivid-colors:vprint-newline :linear output)
    (loop :for (slot . rest) :on (slots<=obj (object-diff-origin diff))
          :for actual :in (object-diff-object diff)
          :do (vivid-colors:put slot output
                                :key (lambda (n) (format nil ":~A" n)))
              (write-char #\Space output)
              (vivid-colors:vprint-newline :miser output)
              (vivid-colors:vprint actual output t)
              (when rest
                (write-char #\Space output)
                (vivid-colors:vprint-newline :linear output)))))

(defun vprint-hash-table-diff (output diff)
  (vivid-colors:vprint-logical-block (output nil :prefix "#<" :suffix ">")
    (vivid-colors:put 'hash-table output)
    (write-char #\Space output)
    (vivid-colors:vprint-indent :current 0 output)
    (vivid-colors:vprint-newline :miser output)
    (loop :for (k v . rest) :on (diff-object diff) :by #'cddr
          :do (vivid-colors:put k output)
              (write-char #\Space output)
              (vivid-colors:vprint-newline :miser output)
              (vivid-colors:vprint v output)
              (when rest
                (write-char #\Space output)
                (vivid-colors:vprint-newline :linear output)))))

(vivid-colors:define-vprint-dispatch :vivid-diff
  (:merge :pretty)
  (:set 'hash-table-diff 'vprint-hash-table-diff)
  (:set 'object-diff 'vprint-object-diff)
  (:set 'pathname-diff 'vprint-pathname-diff)
  (:set 'string-diff 'vprint-string-diff)
  (:set 'diff 'vprint-diff)
  (:set 'nothing 'vprint-nothing))

(defun diff-print (mismatched &optional (output *standard-output*))
  (let ((vivid-colors:*vprint-dispatch*
         (vivid-colors:find-vprint-dispatch :vivid-diff)))
    (vivid-colors:vprint mismatched output)))

;;;; MISMATCH-SEXP

(defvar *env* nil)

(defgeneric mismatch-sexp (actual expected)
  (:method :around (actual expected)
    (declare (ignore actual expected))
    (let ((*env* (or *env* nil)))
      (call-next-method)))
  (:method (actual expected)
    (if (equal actual expected)
        actual
        (markup actual))))

(defmethod mismatch-sexp ((actual symbol) (expected symbol))
  (cond ;; Symbol vs (or keyword boolean).
        ((typep expected '(or keyword boolean))
         (if (eq expected actual)
             actual
             (markup actual)))
        ;; Symbol vs interned-symbol.
        ((symbol-package expected)
         (if (eq actual expected)
             actual
             (markup actual)))
        ;; interned-symbol vs uninterned-symbol.
        ((symbol-package actual) (markup actual))
        ;; uninterned-symbol vs uninterned-symbol.
        (t
         (let ((pair (assoc actual *env* :test #'eq)))
           (if pair ; seen.
               (if (eq expected (cdr pair))
                   actual
                   (markup actual)) ; unseen.
               (let ((pair (rassoc expected *env* :test #'eq)))
                 (if pair ; seen.
                     (markup actual)
                     (progn (push (cons actual expected) *env*) actual))))))))

(defmethod mismatch-sexp ((actual list) (expected list))
  (cond ((null expected) (cond ((null actual) actual) (t (markup actual))))
        ((null actual) (markup actual))
        (t
         (cons (mismatch-sexp (car actual) (car expected))
               (mismatch-sexp (cdr actual) (cdr expected))))))

(defmethod mismatch-sexp ((actual string) (expected string))
  #+sbcl ; due to not simple-string.
  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (if (string= actual expected)
      actual
      (markup-string actual expected)))

(defmethod mismatch-sexp ((actual pathname) (expected pathname))
  (if (equal actual expected)
      actual
      (markup-pathname actual expected)))

(defmethod mismatch-sexp ((actual number) (expected number))
  #+sbcl ; due to number.
  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (if (= actual expected)
      actual
      (markup actual)))

(defmethod mismatch-sexp ((actual bit-vector) (expected bit-vector))
  #+sbcl ; due to not simple-bit-vector.
  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (if (equal actual expected)
      actual
      (markup actual)))

(defun similar-vector-type-p (vec1 vec2) ; to support acl.
  (flet ((vector-type (vector)
           (let ((type-specifier (type-of vector)))
             (and (atom type-specifier) (second type-specifier)))))
    (matrix-case:matrix-typecase (vec1 vec2)
      ((simple-vector simple-vector) t)
      ((simple-vector vector) nil) ; ccl needs.
      ((vector simple-vector) nil) ; ccl needs.
      ((vector vector) (equal (vector-type vec1) (vector-type vec2)))
      (otherwise nil))))

(defmethod mismatch-sexp ((actual vector) (expected vector))
  #+sbcl ; due to not simple-vector.
  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (if (not (similar-vector-type-p actual expected))
      (markup actual)
      (do* ((i 0 (1+ i))
            (a-p (array-in-bounds-p expected i) (array-in-bounds-p expected i))
            (b-p (array-in-bounds-p actual i) (array-in-bounds-p actual i))
            (acc))
           ((or (and (not a-p) (not b-p)) ; same length
                (and (not a-p) b-p) ; actual is longer
                (and a-p (not b-p))) ; actual is shorter
            (cond
              ((and (not a-p) (not b-p)) ; same length
               (coerce (nreverse acc) 'vector))
              ((and (not a-p) b-p) ; actual is longer
               (concatenate 'vector (nreverse acc)
                            (map 'vector #'markup (subseq actual i))))
              ((and a-p (not b-p)) ; actual is shorter
               (coerce (nreverse (cons (markup-nothing :null) acc)) 'vector))))
        (push (mismatch-sexp (aref actual i) (aref expected i)) acc))))

(defmethod mismatch-sexp ((actual array) (expected array))
  #+sbcl ; Due to not known array rank in compile time.
  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (if (not (equal (array-dimensions expected) (array-dimensions actual)))
      (markup
        (list :different-dimensions :expected (array-dimensions expected)
              :actual (array-dimensions actual) actual))
      (let ((a (make-array (array-dimensions actual))))
        (dotimes (i (array-total-size expected) a)
          (setf (row-major-aref a i)
                  (mismatch-sexp (row-major-aref actual i)
                                 (row-major-aref expected i)))))))

(defun slots<=obj (obj)
  #.(or #+abcl
        `(if (typep obj 'structure-object)
             (loop :for slot :in (c2mop:class-slots (class-of obj))
                   :collect (aref slot 1))
             (mapcar #'c2mop:slot-definition-name
                     (c2mop:class-slots (class-of obj))))
        `(mapcar #'c2mop:slot-definition-name
                 (c2mop:class-slots (class-of obj)))))

(defmethod mismatch-sexp
           ((actual structure-object) (expected structure-object))
  (cond ((equalp actual expected) actual)
        ((not (eq (type-of expected) (type-of actual))) (markup actual))
        (t
         (loop :for slot1 :in (slots<=obj expected)
               :for slot2 :in (slots<=obj actual)
               :collect (mismatch-sexp (slot-value actual slot2)
                                       (slot-value expected slot1))
                 :into diffs
               :finally (return (markup-object diffs expected))))))

(defmethod mismatch-sexp ((actual standard-object) (expected standard-object))
  (if (not (eq (type-of expected) (type-of actual)))
      (markup actual)
      (loop :for slot :in (slots<=obj expected)
            :if (slot-boundp expected slot)
              :if (slot-boundp actual slot)
                :collect (mismatch-sexp (slot-value actual slot)
                                        (slot-value expected slot))
                  :into diffs
              :else
                :collect (markup-nothing :unbound) :into diffs
              :end
            :else :if (slot-boundp actual slot)
              :collect (markup (slot-value actual slot)) :into diffs
            :finally (return
                      (if (find-if (lambda (x) (typep x 'diff)) diffs)
                          (markup-object diffs expected)
                          actual)))))

(defmethod mismatch-sexp ((actual hash-table) (expected hash-table))
  (if (equalp actual expected)
      actual
      (markup-hash-table
        (append
          (loop :for ek :being :each :hash-key :of expected :using
                     (:hash-value ev)
                :for (av exists?) := (multiple-value-list (gethash ek actual))
                :for diff? = (mismatch-sexp av ev)
                :if (not exists?)
                  :collect ek
                  :and :collect (markup-nothing :missing)
                :else :if (typep diff? 'diff)
                  :collect ek
                  :and :collect diff?)
          (loop :for ak :being :each :hash-key :of actual
                :for exist? = (nth-value 1 (gethash ak expected))
                :if (not exist?)
                  :collect ak
                  :and :collect (markup (gethash ak actual)))))))