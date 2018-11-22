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


; имеет отношение к анимации:
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
(define (make-creature name initial)
   (fork-server name (lambda ()
   (let this ((itself initial))
   (let*((envelope (wait-mail))
         (sender msg envelope))
      (tuple-case msg
         ; low level interaction interface
         ((set key value)
            (print-to stdout name ": setting " key " to " value)
            (this (put itself key value)))
         ((get key)
            (mail sender (get itself key #false))
            (this itself))
         ((debug)
            (mail sender itself)
            (this itself))

         ; ---------------------------------------------
         ; блок обработки анимации
         ; set animation
         ((set-animations ini first-gid)
            (let*((itself (put itself 'fg first-gid))
                  (itself (put itself 'animations ini)))
               (this itself)))
         ;; ((set-default-animation animation)
         ;;    ; set default animation
         ;;    (let*((itself (put itself 'default-animation animation))
         ;;          (itself (put itself 'ssms (time-ms))))
         ;;       (this itself)))
         ((set-current-animation animation)
            ; set current animation (that will be changed, or not to default)
            ; return animation cycle time (for feature use by caller)
            (let*((itself (put itself 'animation animation))
                  (itself (put itself 'ssms (time-ms))))
               ; сообщим вызывающему сколько будет длиться полный цикл анимации
               ; (так как у нас пошаговая игра, то вызывающему надо подождать пока проиграется цикл анимации)
               (let ((animation-info (getf (get itself 'animations #empty) (getf itself 'animation))))
                  (mail sender (if animation-info (getf animation-info 'duration))))
               (this itself)))
         ((get-animation-frame)
            ; todo: change frames count according to animation type (and fix according math)
            (let*((animation (get itself 'animation 'stance)) ; соответствующая состояния анимация
                  (ssms (- (time-ms) (get itself 'ssms 0))) ; количество ms с момента перехода в анимацию
                  (animations (get itself 'animations #empty))
                  (animation (get animations animation #empty))
                  (animation-type (get animation 'type #false))
                  (duration (get animation 'duration "250ms"))
                  (duration (substring duration 0 (- (string-length duration) 2)))
                  (duration (string->number duration 10))
                  (frames (get animation 'frames "4"))
                  (frames (string->number frames 10))
                  (position (get animation 'position "4"))
                  (position (string->number position 10))
                  (orientation (get itself 'orientation 0))
                  (frame (floor (/ (* ssms frames) duration))))
               ;(print "animation-type: " animation-type)
               (mail sender (+ (get itself 'fg 0) position
                  (cond
                     ((string-eq? animation-type "play_once")
                        (min (- frames 1) frame))
                     ((string-eq? animation-type "looped")
                        (mod frame frames))
                     ((string-eq? animation-type "back_forth")
                        (lref (append (iota frames) (reverse (iota (- frames 2) 1))) (mod frame (+ frames frames -2)))))
                  (get orientations orientation 0))))
            (this itself))

         ; ---------------------------------------------
         ((set-location xy)
            (let*((itself (put itself 'location xy)))
               (this itself)))
         ((set-orientation orientation)
            (let*((itself (put itself 'orientation orientation)))
               (this itself)))

         ;; ((move-relative xy)
         ;;    (let*((location (get itself 'location '(0 . 0)))
         ;;          (itself (put itself 'location (cons (+ (car location) (car xy)) (+ (cdr location) (cdr xy)))))
         ;;          (orientation (cond
         ;;             ((equal? xy '(-1 . 0)) 6)
         ;;             ((equal? xy '(0 . -1)) 0)
         ;;             ((equal? xy '(+1 . 0)) 2)
         ;;             ((equal? xy '(0 . +1)) 4)
         ;;             (else (get itself 'orientation 0))))
         ;;          (itself (put itself 'orientation orientation)))
         ;;       (this itself)))

         ((get-location)
            (mail sender (get itself 'location '(0 . 0)))
            (this itself))

         ;; ; ---------------------------------------------
         ;; ; стейт-машина
         ;; ; сложновата получается стейт-машина (, но что поделать..
         ;; ((process-event event args)
         ;;    ;(print "itself: " itself)
         ;;    (let ((state (getf itself 'state))
         ;;          (state-machine (or (getf itself 'state-machine) #empty)))
         ;;       ;(print name ": state name: " state)
         ;;       ;(print name ": state-machine: " state-machine)
         ;;       (let*((state (get state-machine state #empty))
         ;;             (handler (getf state event)))
         ;;          ;(print name ": state: " state)
         ;;          ;(print name ": handler: " handler " " args)
         ;;          (print-to stderr name ": before " event " " args)
         ;;          (if handler
         ;;             (let*((itself state (apply handler (cons itself (cons name args)))))
         ;;                ; ok, у нас новый стейт, да? надо запустить цикл смены стейта?
         ;;                (print-to stderr name ": handled " event " " args)
         ;;                (this (put itself 'state state)))
         ;;             ; else
         ;;             (begin
         ;;                ; это сообщение пока оставляем, но вообще оно не надо,
         ;;                ;  так как не все события надо обрабатывать
         ;;                (print-to stderr name ": no handler found for " event " event")
         ;;                (mail sender #false) ; просто индикатор, что не обрабатывали (на всякий случай)
         ;;                (this itself))))))

         ;; ((process-event-transition-tick)
         ;;    (let ((action (getf itself 'action)))
         ;;       (if action
         ;;          (this (action itself))
         ;;          (this itself))))

         (else
            (print "unhandled event: " msg)
            (this itself))))))))

; набор функций - работы с НИП
;; (define (creature:send-event creature event . args)
;;    (mail creature (tuple 'process-event event args)))

;; (define (creature:move itself xy)
;;    (let*((location (get itself 'location '(0 . 0)))
;;          (itself (put itself 'location (cons (+ (car location) (car xy)) (+ (cdr location) (cdr xy)))))
;;          (orientation (cond
;;             ((equal? xy '(-1 . 0)) 6)
;;             ((equal? xy '(0 . -1)) 0)
;;             ((equal? xy '(+1 . 0)) 2)
;;             ((equal? xy '(0 . +1)) 4)
;;             (else (get itself 'orientation 0))))
;;          (itself (put itself 'orientation orientation)))
;;       itself))

