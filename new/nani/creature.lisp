(import (file ini))
(import (scheme misc))

;; temp
; получить слой уровня по имени
(define (level:get-layer name)
   (getf (interact 'level ['get 'layers]) name))

; получить первый номер тайла, ассоциированный с тайлсетом name
(define (level:get-gid name)
   (getf (interact 'level ['get 'gids]) name))

; возвращает количество тайлов в одной строке тайлсета name
(define (level:get-columns name)
   (getf (interact 'level ['get 'columns]) name))



;; ; поместить создание на карту
;; (define (creature:set-location creature location)
;;    (mail creature ['set-location location]))
;; ; получить положение создания
;; (define (creature:get-location creature)
;;    (interact creature ['get 'location]))

; задать поворот в пространстве
;; (define (creature:set-orientation creature orientation)
;;    (mail creature ['set-orientation orientation]))

; задать созданию набор анимаций (тайлсет, конфигурационный файл)
;; (define (creature:set-animation-profile creature tileset inifile)
;;    (mail creature ['set-animation-profile tileset inifile]))

; выбрать созданию текущую анимацию по ее имени
;; (define (creature:set-current-animation creature animation)
;;    (interact creature ['set-current-animation animation]))

;; (define (creature:set-next-location creature location)
;;    (mail creature ['set-next-location location]))

; отыграть цикл анимации (с ожиданием)
(define (creature:play-animation creature animation next-animation)
   (let ((started (time-ms))
         (saved-animation (or next-animation ((creature 'get) 'animation)))
         (duration ((creature 'set-current-animation) animation)))
      (let loop ((unused #f))
         ;(print creature ": waiting for " (- (time-ms) started))
         (if (< (- (time-ms) started) duration)
            (loop (sleep 7))))
      (unless (eq? saved-animation animation)
         ; set next animation or restore saved
         ((creature 'set-current-animation) saved-animation))))

; двигаться (с анимацией)
(define (creature:move-with-animation creature move animation next-animation)
   (let ((started (time-ms))
         (saved-animation (or next-animation ((creature 'get) 'animation)))
         (location ((creature 'get-location)))
         (duration ((creature 'set-current-animation) animation)))

      (cond
         ((equal? move '(0 . -1))
            ((creature 'set-orientation) 0))
         ((equal? move '(+1 . 0))
            ((creature 'set-orientation) 2))
         ((equal? move '(0 . +1))
            ((creature 'set-orientation) 4))
         ((equal? move '(-1 . 0))
            ((creature 'set-orientation) 6)))
      ((creature 'set-next-location) move)
      (let loop ((unused #f))
         ;(print creature ": waiting for " (- (time-ms) started))
         (if (< (- (time-ms) started) duration)
            (loop (sleep 7))))
      ;(creature:set-next-location creature #f)
      ((creature 'set-location) ; setting location automatically clears next-location
         (cons (+ (car location) (car move))
               (+ (cdr location) (cdr move))))

      (unless (eq? saved-animation animation)
         ; set next animation or restore saved
         ((creature 'set-current-animation) saved-animation))))

;; ; содержит список крич, где 0..N - npc, ну или по имени (например, 'hero - герой)
;; (fork-server 'creatures (lambda ()
;;    (let this ((itself #empty))
;;       (let*((envelope (wait-mail))
;;             (sender msg envelope))
;;          (case msg
;;             ; low level interaction interface
;;             (['set key data]
;;                (let ((itself (put itself key data)))
;;                   (this itself)))
;;             (['get key]
;;                (mail sender (get itself key #false))
;;                (this itself))
;;             (['debug]
;;                (mail sender itself)
;;                (this itself))

;; ;;             (['ready?]
;; ;;                ; вот тут надо посетить каждого из npc и просто сделать ему interact,
;; ;;                ; это позволит убедиться, что все npc закончили обдумывать свои дела
;; ;;                ; и их наконец то можно рисовать
;; ;;                (mail sender
;; ;;                   (ff-fold (lambda (* key value)
;; ;;                               (cond
;; ;;                                  ((list? value)
;; ;;                                     (for-each (lambda (id) (interact id ['debug])) value))
;; ;;                                  ((symbol? value)
;; ;;                                     (interact value ['debug]))
;; ;;                                  (else
;; ;;                                     (print "unknown creature: " value)))
;; ;;                               #true)
;; ;;                      #t itself))
;; ;;                (this itself))
;; ;;             ;
;;             (else
;;                (print-to stderr "Unknown creatures command: " msg)
;;                (this itself)))))))


;; ; --------------------------
;; ; имеет отношение к анимации:
;; (define orientations (pairs->ff `(
;;    (0 . 3) ; top
;;    (1 . 4) ; top-right
;;    (2 . 5) ; right
;;    (3 . 6) ; right-bottom
;;    (4 . 7) ; bottom
;;    (5 . 0) ; left-bottom
;;    (6 . 1) ; left
;;    (7 . 2)))) ;left-top
;; (define speed 64) ; 1 tile per second
(define orientations {
   0 0 ; top
   1 1 ; right
   2 2 ; bottom
   3 3 ; left
}) ;left-top



(define-syntax make-setter
   (syntax-rules (name)
      ((make-setter (function this sender . args) . body)
         (list (quote function)
            (lambda (this sender . args)
               . body)
            (lambda all
               (mail name (cons (quote function) all)))
            ))))
(define-syntax make-getter
   (syntax-rules (name)
      ((make-getter (function this sender . args) . body)
         (list (quote function)
            (lambda (this sender . args)
               . body)
            (lambda all
               (interact name (cons (quote function) all)))
            ))))


;; (define-syntax make-creature-macro
;;    (syntax-rules (SET)
;;       ((make-creature-macro (SET (function this sender . args) . body) . rest)
;;          (list (quote function)
;;             (lambda (this sender . args)
;;                . body)
;;             (lambda all
;;                (interact name (list->tuple (cons (quote function) all))))
;;             ))))

; ----------------------------------
; todo: make automatic id generation
; создать новое "создание"
(define (make-creature name initial)
(let*((I (fold (lambda (ff function)
                        (cons
                           (put (car ff) (car function) (cadr function))
                           (put (cdr ff) (car function) (caddr function))))
                  (cons initial #empty)
                  (list
         ; debug staff
         (make-setter (set this sender key value)
            (put this key value))
         (make-getter (get this sender key)
            (mail sender (get this key #f))
            this)
         (make-getter (debug this sender)
            (mail sender this)
            this)


         ; задать новое положение npc
         (make-setter (set-location this sender xy)
            (put this 'location xy))

         (make-setter (set-orientation this sender orientation)
            (put this 'orientation orientation))

         (make-getter (get-location this sender)
            (mail sender (get this 'location '(0 . 0)))
            this)


         ; конфигурирование анимации
         ;  задать имя тайловой карты и конфигурационный файл анимаций персонажа
         (make-setter (set-animation-profile this sender name ini)
            (let*((this (put this 'fg (level:get-gid name)))
                  (this (put this 'animations (pairs->ff (ini-parse-file ini))))
                  (this (put this 'columns (level:get-columns name))))
               this))

         (make-getter (set-current-animation this sender animation)
            ; set current animation (that will be changed, or not to default)
            ; return animation cycle time (for feature use by caller)
            (let*((this (put this 'animation animation))
                  (this (put this 'ssms (time-ms))))
               ; сообщим вызывающему сколько будет длиться полный цикл анимации
               ; (так как у нас пошаговая игра, то вызывающему надо подождать пока проиграется цикл анимации)
               (let*((animation-info (getf (get this 'animations #empty) (getf this 'animation)))
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
               this))
         ; ...
         (make-getter (get-animation-frame this sender)
            ; todo: change frames count according to animation type (and fix according math)
            (let*((animation (get this 'animation 'stance)) ; соответствующая состояния анимация
                  (ssms (- (time-ms) (get this 'ssms 0))) ; количество ms с момента перехода в анимацию
                  (columns (get this 'columns 32))
                  (delta (getf this 'next-location))
                  (animations (get this 'animations #empty))
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
                  (orientation (get this 'orientation 0))
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
                        (list-ref (append (iota frames) (reverse (iota (- frames 2) 1))) (mod frame (+ frames frames -2)))))))
               (mail sender (cons (+ (get this 'fg 0) position
                  frame
                  (* columns (get orientations orientation 0)))
                  delta)))
            this)
      ))))

   (fork-server name (lambda ()
   (let this ((itself (car I)))
   (let*((envelope (wait-mail))
         (sender msg envelope))
      (let ((handler (get itself (car msg) #false)))
         (unless handler
            (print "Unhandled message " msg " from " sender))
         (this (if handler
            (apply handler (cons itself (cons sender (cdr msg))))
            itself)))))))

   (define creature (ff-union (cdr I)
      {'name name}
      (lambda (a b) a)))

   ;; ; добавим npc к общему списку npc
   ;; (mail 'creatures ['set name creature])
   creature))

