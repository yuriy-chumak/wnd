#!/usr/bin/ol

; -=( main )=------------------------------------
; подключаем графические библиотеки, создаем окно
(import (lib gl))
(import (otus ffi))
(import (lib soil))
(import (OpenGL version-2-1))
(print "---------------")
(gl:set-window-title "Drawing the tiled map")
(gl:set-window-size 854 480) (glViewport 0 0 854 480)

;(gl:set-window-size 1920 960)
;(glViewport 0 0 1920 960)

(import (lib math))
(import (otus random!))


; ключевые объекты игры:
; -=( level )=-----------------
;     заведует игровой картой
,load "game/level.lisp"
;     (level:load filename) - загрузить карту из файла,
;                             tbd.

; -=( creatures )=-----------------
;  'creatures - заведует всеми живыми(или оживленными) созданиями


; load the model
(import (lang sexp))
(import (scheme misc))
(import (file xml))
(import (lib rlutil))


;(world:load-new-level "river_encampment")


(define split-by-comma (string->regex "c/,/"))
(define split-by-newline (string->regex "c/\n/"))


; load level
(level:load "river_encampment")

,load "ai.lisp"
,load "creature.lisp"

; временная функция работы с level-collision
(define collision-data (interact 'level (tuple 'get 'collision)))
(define H (length collision-data))
(define W (length (car collision-data)))
(define (at x y)
   (if (and (< -1 x W) (< -1 y H))
      (lref (lref collision-data y) x)))



; временная сопрограма creatures,
; TODO: поместить ее в отдельный файл и добавить
; функции "создать кричу, убить кричу и т.д. и т.п."

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


; loading animations
(define skeleton-animation (list->ff (ini-parse-file "skeleton.ini")))
;(define antlion-animation (list->ff (ini-parse-file "antlion.ini")))


;; ; find first skeleton id
;; (define hero_pos (xml-get-subtag level 'properties))
;; (define hero_pos (car (filter (lambda (tag) (string-eq? (xml-get-attribute tag 'name "") "hero_pos")) (xml-get-value hero_pos))))
;; (define hero_pos (xml-get-attribute hero_pos 'value #f))
;; (define hero_pos (split-by-comma hero_pos))
;; (define hero_pos (cons
;;    (string->number (car hero_pos) 10)
;;    (string->number (cadr hero_pos) 10)))
;; (define hero_pos (cons 51 61)) ; TODO: move to level


; -=( hero )=---------
(define hero-destination '(51 . 61)) ; точка, куда надо идти герою

(make-creature 'hero) (mail 'creatures (tuple 'set 'hero 'hero))
(mail 'hero (tuple 'set-location '(51 . 61)))

(define zombie-animation (list->ff (ini-parse-file "zombie.ini")))
(mail 'hero (tuple 'set-animations zombie-animation 860)) ; zombie 'firstgid'
(mail 'hero (tuple 'set-current-animation 'run))

(mail 'hero (tuple 'set 'idle (lambda (itself)
   (define location (getf itself 'location))
   ;(print "location: " location)
   ;(print "hero-destination: " hero-destination)
   (let ((move (A* collision-data
                  (car location) (cdr location)
                  (car hero-destination) (cdr hero-destination))))
      ;(print "move: " move)
      (if move
         (creature:move itself (cons (ref move 1) (ref move 2)))
         itself)))))


; TEMP
;; ; spawn skeleton
;; (make-creature 'skeleton)
;; (let*((tilesets (filter (lambda (tag) (string-eq? (xml-get-attribute tag 'name "") "skeleton")) tilesets))
;;       (firstgid (string->number (xml-get-attribute (car tilesets) 'firstgid 0) 10)))
;;    (mail 'skeleton (tuple 'set-animations skeleton-animation firstgid)))
;; (mail 'skeleton (tuple 'set-location hero_pos))
;; (mail 'skeleton (tuple 'set-current-animation 'run))

;; (define skeleton (filter (lambda (tag) (string-eq? (xml-get-attribute tag 'name "") "skeleton")) tilesets))
;; (define firstgid (string->number (xml-get-attribute (car skeleton) 'firstgid 0) 10))

(define firstgid 296) ; TODO: move to level
(define skeletons (map (lambda (id)
      (make-creature id)
      (mail id (tuple 'set-animations skeleton-animation firstgid))
      id)
   (iota 6 1000)))

; let skeletons random on map
(for-each (lambda (id)
      (let loop ()
         (let ((x (rand! 64))
               (y (rand! 64)))
            (if (eq? 0 (at x y))
               (mail id (tuple 'set-location (cons x y)))
               (loop))))
      (mail id (tuple 'set-current-animation 'run)))
   skeletons)

(mail 'creatures (tuple 'set 'skeletons skeletons))
(for-each (lambda (id)
      ; научим скелетов ходить и искать сундук
      (mail id (tuple 'set 'think (lambda (itself)
         ; let's find a chest and do a step to it
         (define location (getf itself 'location))
         (define chest (interact 'chest (tuple 'get-location)))
         (define _move
            (A* collision-data
               (car location) (cdr location)
               (car chest) (cdr chest)))
         (define move (cons (ref _move 1) (ref _move 2)))
         (print "move: " move)

         ; move relative:
         ; todo: set as internal function(event or command)
         (let*((itself (put itself 'location (cons (+ (car location) (car move)) (+ (cdr location) (cdr move)))))
               (orientation (cond
                  ((equal? move '(-1 . 0)) 6)
                  ((equal? move '(0 . -1)) 0)
                  ((equal? move '(+1 . 0)) 2)
                  ((equal? move '(0 . +1)) 4)
                  (else (get itself 'orientation 0))))
               (itself (put itself 'orientation orientation)))
            itself)))))
   skeletons)

; а давайте-ка пометим на карту сундук? вместо героя
; и пускай этот сундук сразу телепортируется в новое место,
; как только его кто-то нашел?
(make-creature 'chest)

(let loop ()
   (let ((x (rand! 64))
         (y (rand! 64)))
      (if (eq? 0 (at x y))
         (mail 'chest (tuple 'set-location (cons x y)))
         (loop))))
; и добавим к общему списку нипов:
(mail 'creatures (tuple 'set 'chest 'chest))



; --------------------------------------------------------------------
; x11 mouse:
;; (define X11 (load-dynamic-library "libX11.so.6"))
;; (define XQueryPointer (X11 fft-int "XQueryPointer" fft-void* fft-void* fft-void* fft-void*
;;    (fft& fft-int) (fft& fft-int)
;;    (fft& fft-int) (fft& fft-int)
;;    (fft& fft-unsigned-int)))

; --------------------------------------------------------------------
; view window

(define window (vector (+ -32 -800) -32 (+ 3645 32 -800) (+ 2048 32)))
(define (resize scale)
   (let*((x (floor (/ (+ (ref window 3) (ref window 1)) 2)))
         (w (floor (* (- (ref window 3) (ref window 1)) (/ scale 2))))
         (y (floor (/ (+ (ref window 4) (ref window 2)) 2)))
         (h (floor (* (- (ref window 4) (ref window 2)) (/ scale 2)))))
      (set-ref! window 1 (- x w))
      (set-ref! window 2 (- y h))
      (set-ref! window 3 (+ x w))
      (set-ref! window 4 (+ y h))))
(define (move dx dy)
   (let*((x (floor (* (- (ref window 3) (ref window 1)) 0.01)))
         (y (floor (* (- (ref window 4) (ref window 2)) 0.01))))
      (set-ref! window 1 (+ (ref window 1) (* dx x)))
      (set-ref! window 2 (- (ref window 2) (* dy y)))
      (set-ref! window 3 (+ (ref window 3) (* dx x)))
      (set-ref! window 4 (- (ref window 4) (* dy y)))))


; init
(glShadeModel GL_SMOOTH)
(glClearColor 0.0 0.0 0.0 1)

(glEnable GL_BLEND)
(glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)

(define timestamp (box 0))
; draw
(gl:set-renderer (lambda ()
   ; thinking!
   (let*((ss ms (clock))
         (i (mod (floor (/ (+ (* ss 1000) ms) (/ 1000 4))) 4)))

      (unless (eq? i (unbox timestamp))
         (begin
            (set-car! timestamp i)

            ; события нипов пускай остаются асинхронными,
            ; просто перед рисованием убедимся что они все закончили свою работу
            (for-each (lambda (id)
                  (mail id (tuple 'think)))
               skeletons)
            ; и герою тоже пошлем сообщение
            (mail 'hero (tuple 'idle))

            ;; ; maybe move chest
            ;; (for-each (lambda (id)
            ;;       (define pos (interact id (tuple 'get-location)))

            ;;       (if (equal? pos destination)
            ;;          (let loop ()
            ;;             (let ((x (rand! 64))
            ;;                   (y (rand! 64)))
            ;;                (if (eq? 0 (at x y))
            ;;                   (begin
            ;;                      (set-car! destination x)
            ;;                      (set-cdr! destination y))
            ;;                   (loop))))))
            ;;    skeletons)

         )))

   ; убедимся, что все npc готоры к отображению
   (interact 'creatures (tuple 'ready?))

   ; drawing
   (glClear GL_COLOR_BUFFER_BIT)
   (glLoadIdentity)
   (glOrtho (ref window 1) (ref window 3) (ref window 4) (ref window 2) -1 1) ; invert axis Y on screen!

   ; попросим уровень отрисовать себя
   (interact 'level (tuple 'draw)) ; (level:draw)

   ; -------------
   ; обработчик состояния клавиатуры
   ;  внимание, это "состояние", а не "события"!
   ;  посему можно обрабатывать сразу несколько нажатий клавиатуры одновременно
   (if (key-pressed #x3d) (resize 0.9)) ;=
   (if (key-pressed #x2d) (resize 1.1)) ;-
   (if (key-pressed #xff53) (move +1 0)); right
   (if (key-pressed #xff51) (move -1 0)); left
   (if (key-pressed #xff52) (move 0 +1)); up
   (if (key-pressed #xff54) (move 0 -1)); down

   #null))


; --------------------------------------------
;; (define (unX x y tw th)
;;    (+ (- (* x (/ w 2))
;;          (* y (/ w 2)))
;;       (- (/ (* width w) 4) (/ w 2))))

;; (define (unY x y tw th)
;;    (+ (+ (* x (/ h 2))
;;          (* y (/ h 2)))
;;       (- h th)))
; --------------------------------------------


; keyboard
; обработчик событий клавиатуры
;  внимание, это "события", а не "состояние"!!!
(gl:set-keyboard-handler (lambda (key)
   (print "key: " key)
   (case key
      (#x18 (shutdown 1))))) ; q - quit

(gl:set-mouse-handler (lambda (button x y)
   (print "mouse: " button " (" x ", " y ")")

   (if (eq? button 1) (begin
      ; let's calculate clicked tile: TBD.

      #true))
   #true))
