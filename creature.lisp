; /home/uri/Workspace/ira/flare-game/art_src/animation_defs/characters

(import (file ini))
(import (scheme misc))

; servers with numbers - creatures!
;; (fork-server 'creatures (lambda ()
;; (let this ((id 1) (itself #empty))
;; (let*((envelope (wait-mail))
;;       (sender msg envelope))
;;    (tuple-case msg
;;       ; low level interaction interface
;;       ((new)
;;          (mail sender id)
;;          (this (+ id 1) (put itself id #true)))
;;       ((kill id)
;;          (this itself id (del itself id)))
;;       ((debug)
;;          (mail sender itself)
;;          (this id itself)))))))



(define orientations (list->ff `(
   (0 . ,(* 3 32)) ; top
   (1 . ,(* 4 32)) ; top-right
   (2 . ,(* 5 32)) ; right
   (3 . ,(* 6 32)) ; right-bottom
   (4 . ,(* 7 32)) ; bottom
   (5 . ,(* 0 32)) ; left-bottom
   (6 . ,(* 1 32)) ; left
   (7 . ,(* 2 32))))) ;left-top
(define speed 64) ; 1 tile per second

; todo: make automatic id generation
(define (make-creature key)
   (fork-server key (lambda ()
   (let this ((itself #empty))
   (let*((envelope (wait-mail))
         (sender msg envelope))
      (tuple-case msg
         ; low level interaction interface
         ((set key value)
            (this (put itself key value)))
         ((get key)
            (mail sender (get itself key #false))
            (this itself))
         ((debug)
            (mail sender itself)
            (this itself))

         ; set animation
         ((set-animations ini first-gid)
            (let*((itself (put itself 'fg first-gid))
                  (itself (put itself 'animations ini)))
               (this itself)))
         ((set-current-animation animation)
            ; calculate current animation
            (let*((itself (put itself 'animation animation))
                  (ssms (let* ((ss ms (clock))) (+ (* ss 1000) ms)))
                  (itself (put itself 'ssms ssms)))
               (this itself)))
         ((set-location xy)
            (let*((itself (put itself 'location xy)))
               (this itself)))
         ((set-orientation orientation)
            (let*((itself (put itself 'orientation orientation)))
               (this itself)))

         ((move-relative xy)
            (let*((location (get itself 'location '(0 . 0)))
                  (itself (put itself 'location (cons (+ (car location) (car xy)) (+ (cdr location) (cdr xy)))))
                  (orientation (cond
                     ((equal? xy '(-1 . 0)) 6)
                     ((equal? xy '(0 . -1)) 0)
                     ((equal? xy '(+1 . 0)) 2)
                     ((equal? xy '(0 . +1)) 4)
                     (else (get itself 'orientation 0))))
                  (itself (put itself 'orientation orientation)))
               (this itself)))

         ((get-location)
            (mail sender (get itself 'location '(0 . 0)))
            (this itself))
         ((get-animation-frame)
            (let*((animation (get itself 'animation 'stance)) ; соответствующая состояния анимация
                  (ssms (- (let* ((ss ms (clock))) (+ (* ss 1000) ms)) ; количество ms с момента перехода в анимацию
                           (get itself 'ssms 0)))
                  (animations (get itself 'animations #empty))
                  (animation (get animations animation #empty))
                  (duration (get animation 'duration "250ms"))
                  (duration (substring duration 0 (- (string-length duration) 2)))
                  (duration (string->number duration 10))
                  (frames (get animation 'frames "4"))
                  (frames (string->number frames 10))
                  (position (get animation 'position "4"))
                  (position (string->number position 10))
                  (orientation (get itself 'orientation 0))
                  (frame (floor (/ (* ssms frames) duration))))
               (mail sender (+ (get itself 'fg 0) position (mod frame frames) (get orientations orientation 0))))
            (this itself))
         ; 

         (else
            ; игровое событие?
            (define event (getf itself (ref msg 1)))
            (unless event (print-to stderr "Unknown command " msg))
            (if event
               (this (event itself))
               (this itself)))))))))

