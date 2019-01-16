(import (file ini))
(import (scheme misc))

; поместить создание на карту
(define (creature:set-location creature location)
   (mail creature (tuple 'set-location location)))
; получить положение создания
(define (creature:get-location creature)
   (interact creature (tuple 'get 'location)))

; задать поворот в пространстве
(define (creature:set-orientation creature orientation)
   (mail creature (tuple 'set-orientation orientation)))

; задать созданию набор анимаций (тайлсет, конфигурационный файл)
(define (creature:set-animations creature name inifile)
   (mail creature (tuple 'set-animations name inifile)))

; выбрать созданию текущую анимацию по ее имени
(define (creature:set-current-animation creature animation)
   (interact creature (tuple 'set-current-animation animation)))

(define (creature:set-next-location creature location)
   (mail creature (tuple 'set-next-location location)))

; отыграть цикл анимации (с ожиданием)
(define (creature:play-animation creature animation next-animation)
   (let ((started (time-ms))
         (saved-animation (or next-animation (interact creature (tuple 'get 'animation))))
         (duration (creature:set-current-animation creature animation)))
      (let loop ((unused #f))
         ;(print creature ": waiting for " (- (time-ms) started))
         (if (< (- (time-ms) started) duration)
            (loop (sleep 7))))
      (unless (eq? saved-animation animation)
         ; set next animation or restore saved
         (creature:set-current-animation creature saved-animation))))

; двигаться (с анимацией)
(define (creature:move-with-animation creature move animation next-animation)
   (let ((started (time-ms))
         (saved-animation (or next-animation (interact creature (tuple 'get 'animation))))
         (location (creature:get-location creature))
         (duration (creature:set-current-animation creature animation)))
      (creature:set-next-location creature move)
      (let loop ((unused #f))
         ;(print creature ": waiting for " (- (time-ms) started))
         (if (< (- (time-ms) started) duration)
            (loop (sleep 7))))
      ;(creature:set-next-location creature #f)
      (creature:set-location creature ; setting location automatically clears next-location
         (cons (+ (car location) (car move))
               (+ (cdr location) (cdr move))))

      (unless (eq? saved-animation animation)
         ; set next animation or restore saved
         (creature:set-current-animation creature saved-animation))))

; содержит список крич, где 0..N - npc, ну или по имени (например, 'hero - герой)
(fork-server 'creatures (lambda ()
   (let this ((itself #empty))
      (let*((envelope (wait-mail))
            (sender msg envelope))
         (tuple-case msg
            ; low level interaction interface
            ((set key data)
               (let ((itself (put itself key data)))
                  (this itself)))
            ((get key)
               (mail sender (get itself key #false))
               (this itself))
            ((debug)
               (mail sender itself)
               (this itself))

            ((ready?)
               ; вот тут надо посетить каждого из npc и просто сделать ему interact,
               ; это позволит убедиться, что все npc закончили обдумывать свои дела
               ; и их наконец то можно рисовать
               (mail sender
                  (ff-fold (lambda (* key value)
                              (cond
                                 ((list? value)
                                    (for-each (lambda (id) (interact id (tuple 'debug))) value))
                                 ((symbol? value)
                                    (interact value (tuple 'debug)))
                                 (else
                                    (print "unknown creature: " value)))
                              #true)
                     #t itself))
               (this itself))
            ;
            (else
               (print-to stderr "Unknown creatures command: " msg)
               (this itself)))))))


; --------------------------
; имеет отношение к анимации:
(define orientations (list->ff `(
   (0 . 3) ; top
   (1 . 4) ; top-right
   (2 . 5) ; right
   (3 . 6) ; right-bottom
   (4 . 7) ; bottom
   (5 . 0) ; left-bottom
   (6 . 1) ; left
   (7 . 2)))) ;left-top
(define speed 64) ; 1 tile per second

; ----------------------------------
; todo: make automatic id generation
; создать новое "создание"
(define (make-creature name initial)
   (fork-server name (lambda ()
   (let this ((itself initial))
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

         ; ---------------------------------------------
         ; блок обработки анимации
         ; set animation
         ;  задать имя тайловой карты и конфигурационный файл анимаций персонажа
         ((set-animations name ini)
            (let*((itself (put itself 'fg (level:get-gid name)))
                  (itself (put itself 'animations (list->ff (ini-parse-file ini))))
                  (itself (put itself 'columns (level:get-columns name))))
               (this itself)))

         ((set-current-animation animation)
            ; set current animation (that will be changed, or not to default)
            ; return animation cycle time (for feature use by caller)
            (let*((itself (put itself 'animation animation))
                  (itself (put itself 'ssms (time-ms))))
               ; сообщим вызывающему сколько будет длиться полный цикл анимации
               ; (так как у нас пошаговая игра, то вызывающему надо подождать пока проиграется цикл анимации)
               (let*((animation-info (getf (get itself 'animations #empty) (getf itself 'animation)))
                     (duration (getf animation-info 'duration))
                     (duration (substring duration 0 (- (string-length duration) 2)))
                     (duration (string->number duration 10))
                     (frames (get animation-info 'frames "4"))
                     (frames (string->number frames 10))
                     (animation-type (get animation-info 'type ""))
                     (duration (if (string-eq? animation-type "back_forth")
                                 (floor (* (+ frames frames -1)))
                                 duration)))
                     ; decoded duration in ms
                  (mail sender #|duration|#
                     (if (string-eq? animation-type "back_forth")
                           (* 100 (+ frames frames -1))
                           (* 100 frames))
                  ))
               (this itself)))
         ((get-animation-frame)
            ; todo: change frames count according to animation type (and fix according math)
            (let*((animation (get itself 'animation 'stance)) ; соответствующая состояния анимация
                  (ssms (- (time-ms) (get itself 'ssms 0))) ; количество ms с момента перехода в анимацию
                  (columns (get itself 'columns 32))
                  (delta (getf itself 'next-location))
                  (animations (get itself 'animations #empty))
                  (animation (get animations animation #empty))
                  (animation-type (get animation 'type #false))
                  (duration (get animation 'duration "250ms"))
                  (duration (substring duration 0 (- (string-length duration) 2)))
                  (duration (string->number duration 10))
                  (frames (get animation 'frames "4"))
                  (frames (string->number frames 10))
                  (duration (if (string-eq? animation-type "back_forth")
                              (floor (* (+ frames frames -1)))
                              duration))

                  (duration
                     (if (string-eq? animation-type "back_forth")
                           (* 100 (+ frames frames -1))
                           (* 100 frames)))


                  (position (get animation 'position "4"))
                  (position (string->number position 10))
                  (orientation (get itself 'orientation 0))
                  (frame (floor (/ (* ssms frames) duration)))

                  (delta (if delta (let ((n (if (string-eq? animation-type "back_forth")
                                                (+ frames frames -1)
                                                frames)))
                     (cons (* (min frame n) (/ (car delta) n))
                           (* (min frame n) (/ (cdr delta) n))))))

                  (frame (cond
                     ((string-eq? animation-type "play_once")
                        (min (- frames 1) frame))
                     ((string-eq? animation-type "looped")
                        (mod frame frames))
                     ((string-eq? animation-type "back_forth")
                        (lref (append (iota frames) (reverse (iota (- frames 2) 1))) (mod frame (+ frames frames -2)))))))
               (mail sender (cons (+ (get itself 'fg 0) position
                  frame
                  (* columns (get orientations orientation 0)))
                  delta)))
            (this itself))

         ; ---------------------------------------------
         ((set-location xy)
            (let*((itself (put itself 'location xy))
                  (itself (del itself 'next-location)))
               (this itself)))
         ((set-next-location xy)
            (let*((itself (put itself 'next-location xy)))
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
            (this itself)))))))
   (define creature (list->ff (list
      (cons 'set-location (lambda (location)
         (creature:set-location name location)))
      (cons 'get-location (lambda ()
         (creature:get-location name)))
      (cons 'set-orientation (lambda (orientation)
         (creature:set-orientation name orientation)))
      )))
   (mail 'creatures (tuple 'set name creature))
   creature)

