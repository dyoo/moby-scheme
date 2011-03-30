 #lang planet dyoo/moby:3:9
  (require (planet dyoo/moby:3:9/phone/tilt))
  (require (planet dyoo/moby:3:9/phone/location))
  
  (define-struct gps (lat lng))
  (define-struct tilt (a p r))
  (define-struct accel (x y z))
  
  (define-struct sensors (gps tilt accel))
  
  (define (update-gps w lat lng)
    (make-sensors (make-gps lat lng)
                  (sensors-tilt w)
                  (sensors-accel w)))
  
  (define (update-tilt w a p r)
    (make-sensors (sensors-gps w)
                  (make-tilt a p r)
                  (sensors-accel w)))
  
  (define (update-accel w x y z)
    (make-sensors (sensors-gps w)
                  (sensors-tilt w)
                  (make-accel x y z)))
  
  (big-bang (make-sensors (make-gps "loading" "loading")
                          (make-tilt "loading" "loading" "loading")
                          (make-accel "loading" "loading" "loading"))
            (on-location-change update-gps)
            (on-tilt update-tilt)
            (on-acceleration update-accel))