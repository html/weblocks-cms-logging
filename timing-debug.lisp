(in-package :weblocks-cms-logging)

; Contains all timings per page
(defvar *timings* nil)
; Contains started but not yet ended timings
(defvar *running-timings* nil)

(defmethod weblocks::on-timing-start :around (level name)
  (declare (special *timings* *running-timings*))
  ;(firephp:fb "Start" level name)

  (when (= 1 level)
    (setf *timings* nil)
    (setf *running-timings* nil))

  (let* ((start/real (get-internal-real-time))
         (start/cpu (get-internal-run-time)))

    (push (list level name start/real start/cpu) *running-timings*)
    (push (list :level level :name name) *timings*)))

(defmethod weblocks::on-timing-end :around (level name)
  (declare (special *timings*))
  ;(firephp:fb "End" level name)

  (let ((removed-timing-data (remove-running-timing level name)))
    (setf *timings* 
          (loop for i in *timings* 
                collect
                (if (and 
                      (= (getf i :level) (first removed-timing-data))
                      (string= (getf i :name) (second removed-timing-data)))
                  (progn 
                    (setf (getf i :spent/real) (/ (- (get-internal-real-time) (third removed-timing-data)) internal-time-units-per-second))
                    (setf (getf i :spent/cpu) (/ (- (get-internal-run-time) (fourth removed-timing-data)) internal-time-units-per-second))
                    i)
                  i))))

  (when (= level 1)
    (display-timings (reverse *timings*))))

(defun display-timings (timings)
  (format t "~%~%~%")
  (dolist (timing timings)
    (dotimes (i (getf timing :level))
      (format t " "))
    (format t "~A time (real/cpu): ~F/~F~%" (getf timing :name) (getf timing :spent/real) (getf timing :spent/cpu)))
  (format t "~%~%~%"))

(defun display-timings-as-html (timings)
  (dolist (timing timings)
    (dotimes (i (getf timing :level))
      (<:as-is "&nbsp;&nbsp;&nbsp;&nbsp;"))
    (<:format "~A time (real/cpu): ~F/~F" (getf timing :name) (getf timing :spent/real) (getf timing :spent/cpu))
    (<:br)))

(defun remove-running-timing (level name)
  (declare (special *running-timings*))
  (let ((removed-timing))
    (setf *running-timings* 
          (remove-if 
            (lambda (item)
              (when 
                (and 
                  (not removed-timing)
                  (= (first item) level)
                  (string= (second item) name))
                (progn 
                  (setf removed-timing item)
                  t)))
            *running-timings*))
    removed-timing))

(defun complete-running-timings ()
  (declare (special *timings* *running-timings*))
  (loop for (level name dummy-1 dummy-2) in *running-timings* do 
        (weblocks::on-timing-end level name))
  (reverse *timings*))
