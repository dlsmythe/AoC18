
;;; Helper function to tell us if a given sequence has just one element.
(defun single (sequence)
  (if (consp sequence)
      (not (cdr sequence))
      (= (length sequence) 1)))

;;; Sequence can be a vector or a list. Note that this means that this
;;; code isn't optimized for any of those.
(defun merge-sort (sequence)
  (if (or (null sequence) (single sequence))
      sequence
      (let ((half (truncate (/ (length sequence) 2))))
        ;; MERGE is a standard common-lisp function, which does just
        ;; what we want.
        (merge (type-of sequence)
               (merge-sort (subseq sequence 0 half))
               (merge-sort (subseq sequence half))
               #'string<))))
