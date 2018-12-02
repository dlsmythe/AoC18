;;(proclaim '(optimize (speed 3) (safety 0)))
(proclaim '(optimize (debug 3) (speed 0) (safety 3)))

(defun read-freqs ()
  (with-open-file (in "adv18-01.input")
    (let ((freqtab (make-array '(1) :fill-pointer 0 :adjustable t)))
      (do ((line (read-line in nil)
		 (read-line in nil)))
	  ((null line) freqtab)
	(vector-push-extend (parse-integer line) freqtab)))))

(defun do-day (daynum)
  (format t "Day: ~a~%" daynum)
  (do ((freqs (read-freqs))
       (freq 0)
       (freqhist (make-hash-table :test 'equal)))
      (nil)
    (setf (gethash 0 freqhist) 0)
    (loop for i across freqs
       do (progn
	    (incf freq i)
	    (when (gethash freq freqhist)
	      (format t "First repeat is ~a~%" freq)
	      (return-from do-day))
	    (setf (gethash freq freqhist) freq)))
    (when (= 1 daynum)
      (format t "final freq is ~a~%" freq)
      (return-from do-day))))

(do-day 1)
(do-day 2)

(sb-ext:exit :code 0)
