#lang scheme/base
(require scheme/math
         scheme/contract
         (planet dyoo/infix/infix))


(provide/contract [compute-distance (number? number? number? number? . -> . number?)])



(define (to-rad deg)
  (/ (* deg pi) 180.0))

;; transliterated from Android source code on Location.

(define (compute-distance lat1 lon1 lat2 lon2)
  (define MAXITERS 20)
  ;; Convert lat/long to radians
  (set! lat1 (to-rad lat1))
  (set! lat2 (to-rad lat2))
  (set! lon1 (to-rad lon1))
  (set! lon2 (to-rad lon2))

  (let* ([a 6378137.0]         ; WGS84 major axis
         [b 6356752.3142]      ; WGS84 semi-major axis
         [f (/ (- a b) a)]
         [aSqMinusBSqOverBSq (/ (- (sqr a) (sqr b)) 
                                (sqr b))]
         [L (- lon2 lon1)]
         [A 0.0]
         [U1 (atan (* (- 1.0 f) 
                      (tan lat1)))]
         [U2 (atan (* (- 1.0 f) 
                      (tan lat2)))]

         [cosU1 (cos U1)]
         [cosU2 (cos U2)]
         [sinU1 (sin U1)]
         [sinU2 (sin U2)]
         [cosU1cosU2 (* cosU1 cosU2)]
         [sinU1sinU2 (* sinU1 sinU2)]

         [sigma 0.0]
         [deltaSigma 0.0]
         [cosSqAlpha 0.0]
         [cos2SM 0.0]
         [cosSigma 0.0]
         [sinSigma 0.0]
         [cosLambda 0.0]
         [sinLambda 0.0]

         [lambda L])             ; initial guess
    (let/ec break
      (for ([iter (in-range 0 MAXITERS)])
        (let ([lambdaOrig lambda])
          (set! cosLambda (cos lambda))
          (set! sinLambda (sin lambda))
          (let* ([t1 (* cosU2 sinLambda)]
                 [t2 (- (* cosU1  sinU2) (* sinU1 cosU2 cosLambda))]
                 [sinSqSigma (+  (* t1  t1)
                                 (* t2  t2))])  ; (14)
            (set! sinSigma (sqrt sinSqSigma))
            (set! cosSigma (+ sinU1sinU2 (* cosU1cosU2 cosLambda))); (15)
            (set! sigma (atan sinSigma cosSigma))                  ; (16)
            (let ([sinAlpha 
                   (if (= sinSigma 0)
                       0.0
                       (/ (* cosU1cosU2 sinLambda)
                          sinSigma))])                             ; (17)
              (set! cosSqAlpha (- 1.0 (* sinAlpha sinAlpha)))
              (set! cos2SM  (if (= cosSqAlpha 0)
                                0.0 
                                (- cosSigma (/ (* 2.0 sinU1sinU2) cosSqAlpha))))  ; (18)
              (let ([uSquared (* cosSqAlpha aSqMinusBSqOverBSq)])  ;  defn
                (set! A (infix 1 + (uSquared / 16384.0) * ; (3)
                               (4096.0 + uSquared *
                                       (-768 + uSquared * (320.0 - 175.0 * uSquared)))))
                (let ([B (infix (uSquared / 1024.0) * ; (4)
                                (256.0 + uSquared *
                                       (-128.0 + uSquared * (74.0 - 47.0 * uSquared))))]
                      [C (infix 
                          (f / 16.0) *
                          cosSqAlpha *
                          (4.0 + f * (4.0 - 3.0 * cosSqAlpha)))]
                      [cos2SMSq (* cos2SM cos2SM)])                ; (10)

                  (set! deltaSigma (infix B * sinSigma *      ; (6)
                                          (cos2SM + (B / 4.0) *
                                                  (cosSigma * (-1.0 + 2.0 * cos2SMSq) -
                                                            (B / 6.0) * cos2SM *
                                                            (-3.0 + 4.0 * sinSigma * sinSigma) *
                                                            (-3.0 + 4.0 * cos2SMSq)))))
                  (set! lambda
                        (infix L +
                                               (1.0 - C) * f * sinAlpha *
                                               (sigma + C * sinSigma *
                                                (cos2SM + C * cosSigma *
                                                 (-1.0 + 2.0 * cos2SM * cos2SM))))); // (11)
                  (let ([delta (infix (lambda - lambdaOrig) / lambda)])

                    (when (infix (abs(delta) < 1.0e-12)) 
                      (break))))))))))
    
    (let ([distance 
           (infix (b * A * (sigma - deltaSigma)))])
      distance)))





;        float distance = (float) (b * A * (sigma - deltaSigma));
;        results[0] = distance;
;        if (results.length > 1) {
;            float initialBearing = (float) Math.atan2(cosU2 * sinLambda,
;                cosU1 * sinU2 - sinU1 * cosU2 * cosLambda);
;            initialBearing *= 180.0 / Math.PI;
;            results[1] = initialBearing;
;            if (results.length > 2) {
;                float finalBearing = (float) Math.atan2(cosU1 * sinLambda,
;                    -sinU1 * cosU2 + cosU1 * sinU2 * cosLambda);
;                finalBearing *= 180.0 / Math.PI;
;                results[2] = finalBearing;
;            }
;        }
;    }