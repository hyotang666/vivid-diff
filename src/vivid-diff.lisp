(in-package :cl-user)

(defpackage :vivid-diff
  (:use :cl)
  (:export ;;;; MAIN API
           "MISMATCH-SEXP")
  (:export ;;;; For hackers to extend.
           "DIFF" ; structure name
           "MARKUP" ; constructor
           "DIFF-OBJECT" ; accessor
           "DIFF-COMPARABLE" ; structure name
           "DIFF-COMPARABLE-ORIGIN" ; accessor
           ))

(in-package :vivid-diff)

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

;;;; VPRINTERS

(defun vprint-diff (output diff)
  (vivid-colors:put (diff-object diff) output :color cl-colors2:+red+)
  (values))

(vivid-colors:set-vprint-dispatch 'diff 'vprint-diff)

(defmethod print-object ((object diff) output)
  (cond ((or *print-readably* *print-escape*) (call-next-method))
        (t (vivid-colors:vprint object output))))

(defun vprint-string-diff (output diff)
  (let* ((pos (mismatch (string-diff-origin diff) (string-diff-object diff)))
         (expected-in-bounds-p
          (array-in-bounds-p (string-diff-origin diff) pos))
         (actual-in-bounds-p (array-in-bounds-p (string-diff-object diff) pos)))
    (if expected-in-bounds-p
        (if actual-in-bounds-p
            ;; simply different. e.g. "foobar" "foohoge"
            #0=(vivid-colors:put-strings
                 (list (subseq (string-diff-object diff) 0 pos)
                       (list (subseq (string-diff-object diff) pos)
                             cl-colors2:+red+
                             :style :background))
                 output)
            ;; too much short. e.g. "foobar" "foo"
            (vivid-colors:put-strings
              (list (string-diff-object diff)
                    (list ":NULL" cl-colors2:+red+ :effect :blink))
              output))
        (if actual-in-bounds-p
            ;; too much long. e.g. "foo" "foobar"
            #0#
            ;; simply different e.g. "foo" "bar"
            (vivid-colors:put (string-diff-object diff) output
                              :color cl-colors2:+red+)))))

(vivid-colors:set-vprint-dispatch 'string-diff 'vprint-string-diff)

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
          (vivid-colors:vprint-logical-block (out output :prefix "#P")
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
              out))
          (vivid-colors:vprint-logical-block (out output :prefix "#P")
            (vivid-colors:vprint
              (mismatch-sexp (namestring diff) (namestring origin)) out))))))

(vivid-colors:set-vprint-dispatch 'pathname-diff 'vprint-pathname-diff)

(defun vprint-object-diff (output diff)
  (vivid-colors:vprint-logical-block (out output :prefix
                                      (if (typep (object-diff-origin diff)
                                                 'structure-object)
                                          "#S("
                                          "#<")
                                      :suffix
                                      (if (typep (object-diff-origin diff)
                                                 'structure-object)
                                          ")"
                                          ">"))
    (vivid-colors:put (type-of (object-diff-origin diff)) out :color nil)
    (vivid-colors:put-char #\Space out)
    (vivid-colors:vprint-indent :current 0 out)
    (vivid-colors:vprint-newline :miser out)
    (loop :for (slot . rest) :on (slots<=obj (object-diff-origin diff))
          :for actual :in (object-diff-object diff)
          :do (vivid-colors:put slot out
                                :color cl-colors2:+yellow+
                                :key (lambda (n) (format nil ":~S" n)))
              (vivid-colors:put-char #\Space out)
              (vivid-colors:vprint-newline :miser out)
              (if (typep actual 'diff)
                  (vivid-colors:vprint actual out t)
                  (vivid-colors:put actual out))
              (when rest
                (vivid-colors:put-char #\Space out)
                (vivid-colors:vprint-newline :linear out)))))

(vivid-colors:set-vprint-dispatch 'object-diff 'vprint-object-diff)

(defun vprint-hash-table-diff (output diff)
  (vivid-colors:vprint-logical-block (out output :prefix "#<" :suffix ">")
    (vivid-colors:put 'hash-table out)
    (vivid-colors:put-char #\Space out)
    (vivid-colors:vprint-indent :current 0 out)
    (vivid-colors:vprint-newline :miser out)
    (loop :for (k v . rest) :on (diff-object diff) :by #'cddr
          :do (vivid-colors:put k out
                                :color (if (find v '(missing over))
                                           cl-colors2:+red+
                                           cl-colors2:+yellow+))
              (vivid-colors:put-char #\Space out)
              (vivid-colors:vprint-newline :miser out)
              (if (find v '(missing over))
                  (vivid-colors:put v out)
                  (vivid-colors:vprint v out))
              (when rest
                (vivid-colors:put-char #\Space out)
                (vivid-colors:vprint-newline :linear out)))))

(vivid-colors:set-vprint-dispatch 'hash-table-diff 'vprint-hash-table-diff)

;;;; MISMATCH-SEXP

(defvar *env* nil)

(defgeneric mismatch-sexp (actual expected)
  (:method :around (actual expected)
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
  (if (string= actual expected)
      actual
      (markup-string actual expected)))

(defmethod mismatch-sexp ((actual pathname) (expected pathname))
  (if (equal actual expected)
      actual
      (markup-pathname actual expected)))

(defmethod mismatch-sexp ((actual number) (expected number))
  (if (= actual expected)
      actual
      (markup actual)))

(defmethod mismatch-sexp ((actual bit-vector) (expected bit-vector))
  (if (equal actual expected)
      actual
      (markup actual)))

(defmethod mismatch-sexp ((actual vector) (expected vector))
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
           (coerce (nreverse (cons :null acc)) 'vector))))
    (push (mismatch-sexp (aref actual i) (aref expected i)) acc)))

(defmethod mismatch-sexp ((actual array) (expected array))
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
  (mapcar #'c2mop:slot-definition-name (c2mop:class-slots (class-of obj))))

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
      (loop :for slot1 :in (slots<=obj expected)
            :for slot2 :in (slots<=obj actual)
            :collect (mismatch-sexp (slot-value actual slot2)
                                    (slot-value expected slot1))
              :into diffs
            :finally (return (markup-object diffs expected)))))

(defmethod mismatch-sexp ((actual hash-table) (expected hash-table))
  (if (equalp actual expected)
      actual
      (markup-hash-table
        (append
          (loop :for ek :being :each :hash-key :of expected :using
                     (:hash-value ev)
                :for (av exists?) := (multiple-value-list (gethash ek actual))
                :for diff? = (mismatch-sexp ev av)
                :if (not exists?)
                  :collect ek
                  :and :collect 'missing
                :else :if (typep diff? 'diff)
                  :collect ek
                  :and :collect diff?)
          (loop :for ak :being :each :hash-key :of actual
                :for exist? = (gethash ak expected)
                :if (not exist?)
                  :collect ak
                  :and :collect 'over)))))