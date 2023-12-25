(ql:quickload "bordeaux-threads")

(defparameter *sez-profiles* (make-hash-table :test 'equal))
(defvar *sez-queue* nil)  ; Queue for speech commands
(defvar *sez-queue-lock* (bt:make-lock "sez-queue-lock"))  ; Lock for thread safety
(defvar *sez-queue-process-thread* nil)  ; Thread for processing the queue

(defun sez/add-profile (name lang rate pitch)
  "Adds or updates a profile."
  (setf (gethash name *sez-profiles*)
        (list :lang lang :rate rate :pitch pitch)))

(defun sez-process-queue ()
  "Processes the speech queue."
  (loop while (bt:with-lock-held (*sez-queue-lock*)
               (not (null *sez-queue*)))
        do (let ((cmd (bt:with-lock-held (*sez-queue-lock*)
                        (pop *sez-queue*))))
             (uiop:run-program cmd :wait t)
             (sleep 1))  ; Adjust the delay as needed
        finally (setf *sez-queue-process-thread* nil)))

(defun sez (text &key as rate pitch lang)
  "Enqueues the speech command."
  (let ((cmd (if as
                 (let ((profile (gethash as *sez-profiles*)))
                   (format nil "spd-say -y ~A -r ~A -p ~A ~S"
                           (getf profile :lang)
                           (getf profile :rate)
                           (getf profile :pitch)
                           text))
                 (format nil "spd-say ~@[ -y ~A~] ~@[ -r ~A~] ~@[ -p ~A~] ~S"
                         (or lang "default-voice") (or rate 50) (or pitch 50) text))))
    (bt:with-lock-held (*sez-queue-lock*)
      (push cmd *sez-queue*)
      (when (and (null *sez-queue-process-thread*)
                 (not (null *sez-queue*)))
        (setf *sez-queue-process-thread*
              (bt:make-thread (lambda () (sez-process-queue))))))))

;; Example usage
(sez/add-profile 'bob "english-male" 10 60)
(sez "Hello" :as 'bob)
(sez "Custom voice" :lang "english-female" :rate 30 :pitch 100)
(sez "Default settings")
