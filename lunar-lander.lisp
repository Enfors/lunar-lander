;;;; Lunar lander - a learning example by Christer Enfors

(defclass lander ()
  ((altitude :initarg :altitude)
   (speed    :initarg :speed)
   (fuel     :initarg :fuel)))

(defgeneric apply-fuel (lander fuel)
  (:documentation "Use fuel to alter the lander's speed."))

(defgeneric apply-friction (lander)
  (:documentation "Reduce the lander's speed by a set percentage."))

(defgeneric apply-gravity (lander)
  (:documentation "Apply the effect of gravity on the lander's speed."))

(defgeneric apply-speed (lander)
  (:documentation "Change the lander's altitude by applying speed."))

(defgeneric update (lander fuel)
  (:documentation "Update the lander's speed and altitude."))

(defgeneric check-touchdown (lander)
  (:documentation "Check if the lander has landed, crashed, or is in the air."))

(defmethod apply-fuel ((lander lander) fuel)
  (and (> fuel 20)
       (setq fuel 20))
  (and (< fuel 0)
       (setq fuel 0))
  (and (> fuel (slot-value lander 'fuel))
       (setq fuel (slot-value lander 'fuel)))
  (decf (slot-value lander 'fuel) fuel)
  (decf (slot-value lander 'speed) fuel)
  (format t "Using ~a fuel.~%" fuel))

(defmethod apply-friction (lander)
  (setf (slot-value lander 'speed) (* (slot-value lander 'speed) 0.9)))

(defmethod apply-gravity (lander)
  (incf (slot-value lander 'speed) 10))

(defmethod apply-speed (lander)
  (decf (slot-value lander 'altitude) (slot-value lander 'speed)))

(defmethod update ((lander lander) fuel)
  (apply-fuel      lander fuel)
  (apply-friction  lander)
  (apply-gravity   lander)
  (apply-speed     lander)
  (check-touchdown lander))

(defmethod check-touchdown (lander)
  (if (< (slot-value lander 'altitude) 0)
      (progn (if (> (slot-value lander 'speed) 10)
		 (format t "You crashed!~%")
		 (format t "You landed safely!~%"))
	     t)
      nil))

(defmethod print-object ((lander lander) stream)
  (format stream "A lander!~%Altitude: ~a~%Speed: ~a~%Fuel: ~a~%"
	  (slot-value lander 'altitude)
	  (slot-value lander 'speed)
	  (slot-value lander 'fuel)))

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun main ()
  (let ((lander (make-instance 'lander :altitude 100 :speed 0 :fuel 100)))
    (loop
       (print lander)
       (update lander (parse-integer (prompt-read "Use how much fuel?")))
       (if (check-touchdown lander)
	   (return)))))
     
    