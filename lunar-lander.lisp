;;;; Lunar lander - a learning example by Christer Enfors

(defclass lander ()
  ((altitude :initarg :altitude)
   (speed    :initarg :speed)
   (fuel     :initarg :fuel)))

(defgeneric update (lander fuel)
  (:documentation "Update the lander's speed and altitude."))

(defmethod update ((lander lander) fuel)
  (print "Updating lander."))

(defmethod print-object ((lander lander) stream)
  (format stream "A lander!~%Altitude: ~a~%Speed: ~a~%Fuel: ~a~%"
	  (slot-value lander 'altitude)
	  (slot-value lander 'speed)
	  (slot-value lander 'fuel)))