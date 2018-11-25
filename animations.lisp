(import (file ini))

(define (animation:get-duration animation)
   (let*((duration (getf animation 'duration))
         (duration (substring duration 0 (- (string-length duration) 2)))
         (duration (string->number duration 10))
         (frames (get animation 'frames "4"))
         (frames (string->number frames 10))
         (animation-type (get animation 'type ""))
         (duration (if (string-eq? animation "back_forth")
            (floor (* (+ frames frames -1)))
            duration)))
         
      (if (string-eq? animation "back_forth")
            (* 100 (+ frames frames -1))
            (* 100 frames))
   ))