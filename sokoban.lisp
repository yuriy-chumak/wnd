#!/usr/bin/ol

; ----------------------------------
; зададим размеры графического окна
(define-library (lib gl config)
(export config) (import (otus lisp))
(begin
   (define config (list->ff `(
      ; напомню, что мы используем фиксированный шрифт размера 9*16
      (width  . ,(* 2  9 80))      ; 80 знакомест в ширину
      (height . ,(* 2 16 25))))))) ; 25 знакомест в высоту
(import (lib gl config))

; игра пошаговая! посему все ходы только после клика "я готов" (пока это ПКМ) и все НПС
; должны ходить по-очереди. при этом демонстрировать что они делают.
(define screen-width (config 'width))
(define screen-height (config 'height))

; -=( main )=------------------------------------
; подключаем графические библиотеки, создаем окно
(import (lib gl2))
(import (otus ffi))
(import (lib soil))
(print "---------------")
(gl:set-window-title "Sokoban")

; -------------------------------------------------------
; теперь текстовая консолька
(import (lib gl console))

; окно дебага (покажем fps):
(define fps (create-window 70 24 10 1))
(define started (time-ms)) (define time '(0))
(define frames '(0 . 0))

(set-window-writer fps (lambda (print)
   (set-car! frames (+ (car frames) 1))
   (let ((now (time-ms)))
      (if (> now (+ started (car time) 1000))
         (begin
            (set-cdr! frames (car frames))
            (set-car! frames 0)
            (set-car! time (- now started)))))
   (print GRAY (cdr frames) " fps")
))

; остальные игровые библиотеки
(import (lib math))
(import (otus random!))
(import (lang sexp))
(import (scheme misc))
(import (file xml))
(import (scheme dynamic-bindings))
(import (lib rlutil))

; -=( level )=-----------------
;     заведует игровой картой
,load "game/level.lisp"
;,load "animations.lisp"

,load "creature.lisp"
;,load "ai.lisp"

; =============================
; 1. Загрузим игровой уровень
(level:load "007")

; временная функция работы с level-collision
(define collision-data (level:get-layer 'collision))

(define H (length collision-data))       ; высота уровня
(define W (length (car collision-data))) ; ширина уровня

(print "H: " H)
(print "W: " W)

; временная функция: возвращает collision data
;  по координатам x,y на карте
(define (at x y)
   (if (and (< -1 x W) (< -1 y H))
      (lref (lref collision-data y) x)))

(define object-data (level:get-layer 'object))
(print object-data)
(define (get-object x y)
   (if (and (< -1 x W) (< -1 y H))
      (lref (lref object-data y) x)))
(define (set-object x y id)
   (if (and (< -1 x W) (< -1 y H))
      (let loop ((p (lref object-data y)) (x x))
         (if (eq? x 0)
            (set-car! p id)
            (loop (cdr p) (- x 1))))))

(define setup-data (level:get-layer 'setup))
; номера жаровни, горящей жаровни, решетки
(define grid-id (lref (lref setup-data 0) 0))
(define gem-id  (lref (lref setup-data 0) 1))
(define gemH-id (lref (lref setup-data 0) 2))

(print "grid-id: " grid-id)
(print "gem-id: " gem-id)
; =================================================================
; -=( hero )=---------
(define hero (make-creature 'hero #empty))
; зададим позицию героя в мире (найдем его в сетапе)
(creature:set-location 'hero
   (call/cc (lambda (return)
      (map (lambda (y row)
               (map (lambda (x id)
                        (unless (eq? id 0)
                           (return (cons x y))))
                  (iota W) row))
         (iota (- H 1) 1) ; skip first line (with row id's data)
         (cdr setup-data)))))

; зададим анимации герою, в нашем случае он будет выглядеть как скелет
(creature:set-animations 'hero 'zombie "animations/zombie.ini")
(creature:set-current-animation 'hero 'stance) ; пусть он просто стоит

; --------------------------------------------------------------------
; окно, через которое мы смотрим на мир
; todo: отцентрируем его на героя

(define tile (creature:get-location 'hero))
(define w (level:get 'tilewidth))
(define h (level:get 'tileheight))
(define cx (- (* (car tile) (/ w 2)) (* (cdr tile) (/ w 2))))
(define cy (+ (* (car tile) (/ h 2)) (* (cdr tile) (/ h 2))))

(define width (* w 8))
(define height (* h 8))

(print "tile: " tile)
(print "cx: " cx ", cy: " cy)
(print "width: " width ", height: " height)

;              x-left             x-right y-left         y-right
;(define window (vector (+ -32 -800) -32 (+ 3645 32 -800) (+ 2048 32)))
(define window (vector  (- cx width) (- cy height)
                        (+ cx width) (+ cy height)))

;(define window (vector -1920 -64 1920 (- 2160 64)))


(define (resize scale) ; изменение масштаба
   (let*((x (floor (/ (+ (ref window 3) (ref window 1)) 2)))
         (w (floor (* (- (ref window 3) (ref window 1)) (/ scale 2))))
         (y (floor (/ (+ (ref window 4) (ref window 2)) 2)))
         (h (floor (* (- (ref window 4) (ref window 2)) (/ scale 2)))))
      (set-ref! window 1 (- x w))
      (set-ref! window 2 (- y h))
      (set-ref! window 3 (+ x w))
      (set-ref! window 4 (+ y h))))
(define (move dx dy) ; сдвинуть окно
   (let*((x (floor (* (- (ref window 3) (ref window 1)) 0.01)))
         (y (floor (* (- (ref window 4) (ref window 2)) 0.01))))
      (set-ref! window 1 (+ (ref window 1) (* dx x)))
      (set-ref! window 2 (- (ref window 2) (* dy y)))
      (set-ref! window 3 (+ (ref window 3) (* dx x)))
      (set-ref! window 4 (- (ref window 4) (* dy y)))))

; функция перевода экранных координат в номер тайла, на который они попадают
(define (xy:screen->tile xy)
   (let ((x1 (ref window 1)) (x2 (ref window 3))
         (y1 (ref window 2)) (y2 (ref window 4)))
   (let ((x2-x1 (- x2 x1)) (y2-y1 (- y2 y1))
         (w screen-width) (h screen-height))
   (let ((X (floor (+ x1 (/ (* (car xy) x2-x1) w))))
         (Y (floor (+ y1 (/ (* (cdr xy) y2-y1) h)))))
   (let ((w (interact 'level (tuple 'get 'tilewidth)))
         (h (interact 'level (tuple 'get 'tileheight))))
   (let ((x (+ (/ X w) (/ Y h)))
         (y (- (/ Y h) (/ X w))))
      (cons (floor x) (floor y))))))))

; служебная функция: вычисляет, может ли герой ходить в указываемую клетку
(define (move-available? to)
   (let*((herotile (creature:get-location 'hero))
         (dx (- (car to) (car herotile)))
         (dy (- (cdr to) (cdr herotile)))
         (move (cons dx dy))
         (move-available (and
            (or
               (and (eq? dx 0) (eq? dy +1))
               (and (eq? dx 0) (eq? dy -1))
               (and (eq? dx +1) (eq? dy 0))
               (and (eq? dx -1) (eq? dy 0)))
            (and
               (eq? (at (car to) (cdr to)) 0)
               (let ((object (get-object (car to) (cdr to))))
                  (if (or (eq? object gem-id) (eq? object gemH-id))
                     (and
                        (eq? (at (+ (car to) dx) (+ (cdr to) dy)) 0)
                        (let ((o (get-object (+ (car to) dx) (+ (cdr to) dy))))
                           (and (not (eq? o gem-id)) (not (eq? o gemH-id)))))
                     #true))))))
      move-available))

; init
(glShadeModel GL_SMOOTH)
(glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
(gl:hide-cursor)

; служебные переменные
(define timestamp (box 0))

; draw
(gl:set-renderer (lambda (mouse)
   ; тут мы поворачиваем нашего шероя в сторону мышки
   (unless (unbox *calculating*)
      (let*((mousetile (xy:screen->tile mouse))
            (herotile (creature:get-location 'hero))
            (dx (- (car mousetile) (car herotile)))
            (dy (- (cdr mousetile) (cdr herotile))))
         (cond
            ((and (= dx 0) (< dy 0))
               (creature:set-orientation 'hero 0))
            ((and (= dx 0) (> dy 0))
               (creature:set-orientation 'hero 4))
            ((and (< dx 0) (= dy 0))
               (creature:set-orientation 'hero 6))
            ((and (> dx 0) (= dy 0))
               (creature:set-orientation 'hero 2))

            ((and (= dx +1) (= dy +1))
               (creature:set-orientation 'hero 3))
            ((and (= dx -1) (= dy +1))
               (creature:set-orientation 'hero 5))
            ((and (= dx -1) (= dy -1))
               (creature:set-orientation 'hero 7))
            ((and (= dx +1) (= dy -1))
               (creature:set-orientation 'hero 1))
         )))

   ; просто регулярные действия
   (let*((ss ms (clock))
         (i (mod (floor (/ (+ (* ss 1000) ms) (/ 1000 4))) 4)))

      (unless (eq? i (unbox timestamp))
         (begin
            (set-car! timestamp i)

            ; надо послать нипам 'tick, а вдруг они захотят с ноги на ногу попереминаться...

            ;; ; события нипов пускай остаются асинхронными,
            ;; ; просто перед рисованием убедимся что они все закончили свою работу
            ;; (for-each (lambda (id)
            ;;       (mail id (tuple 'process-event-transition-tick)))
            ;;    (interact 'creatures (tuple 'get 'skeletons)))
         )))

   ; теперь можем и порисовать: очистим окно и подготовим оконную математику
   (glClearColor 0.0 0.0 0.0 1)
   (glClear GL_COLOR_BUFFER_BIT)
   (glLoadIdentity)
   (glOrtho (ref window 1) (ref window 3) (ref window 4) (ref window 2) -1 1) ; invert axis Y on screen!
   (glEnable GL_TEXTURE_2D)
   (glEnable GL_BLEND)

   ; теперь попросим уровень отрисовать себя
   (level:draw #false #null)

   ; окошки, консолька, etc.
   (render-windows)

   ; let's draw mouse pointer
   (if (and mouse
            (not (unbox *calculating*)))
      (let*((mousetile (xy:screen->tile mouse))
            (move-available (move-available? mousetile))
            (id (+ (level:get-gid 'pointer) (if move-available 0 1)))
            (tile (getf (interact 'level (tuple 'get 'tileset)) id))
            (w (/ (- (ref window 3) (ref window 1)) 48)) ;  размер курсора
            (st (ref tile 5))
            ; window mouse to opengl mouse:
            (x (+ (ref window 1) (* (car mouse) (- (ref window 3) (ref window 1)) (/ 1 screen-width))))
            (y (+ (ref window 2) (* (cdr mouse) (- (ref window 4) (ref window 2)) (/ 1 screen-height)))))
         (glEnable GL_TEXTURE_2D)
         (glEnable GL_BLEND)
         (glBindTexture GL_TEXTURE_2D (ref tile 1))
         (glBegin GL_QUADS)
            (glTexCoord2f (ref st 1) (ref st 2))
            (glVertex2f x y)

            (glTexCoord2f (ref st 3) (ref st 2))
            (glVertex2f (+ x w) y)

            (glTexCoord2f (ref st 3) (ref st 4))
            (glVertex2f (+ x w) (+ y w))

            (glTexCoord2f (ref st 1) (ref st 4))
            (glVertex2f x (+ y w))
         (glEnd)))
   ;|#
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

(gl:set-keyboard-handler (lambda (key)
   (print "key: " key)
   (case key
      (#x18
         ;(mail 'music (tuple 'shutdown))
         (halt 1))))) ; q - quit

(gl:set-mouse-handler (lambda (button x y)
   (print "mouse: " button " (" x ", " y ")")
   (unless (unbox *calculating*) ; если мир сейчас не просчитывается (todo: оформить отдельной функцией)
      (cond
         ((eq? button 1)
            (let ((tile (xy:screen->tile (cons x y))))
               (set-world-busy #true)
               (mail 'game (tuple 'move tile))))
         (else
            ; nothing
            #false)))))

(fork-server 'game (lambda ()
   (let this ((itself #empty))
   (let*((envelope (wait-mail))
         (sender msg envelope))
      (tuple-case msg
         ((move to)
            (if (move-available? to)
               (let*((hero (creature:get-location 'hero))
                     (rel (cons
                        (- (car to) (car hero))
                        (- (cdr to) (cdr hero)))))
                  (print "object-to: " (get-object (car to) (cdr to)))
                  (if (or
                        (eq? (get-object (car to) (cdr to)) gem-id)
                        (eq? (get-object (car to) (cdr to)) gemH-id))
                     (begin
                        (set-object (+ (car to) (car rel)) (+ (cdr to) (cdr rel))
                           (let ((background (level:get-layer 'background)))
                              (if (eq? (lref (lref background (+ (cdr to) (cdr rel))) (+ (car to) (car rel))) grid-id)
                                 gemH-id gem-id)))
                        (set-object (car to) (cdr to) 0)))

                  ; повернем героя в нужную сторону
                  (cond
                     ((equal? rel '(0 . -1))
                        (creature:set-orientation 'hero 0))
                     ((equal? rel '(+1 . 0))
                        (creature:set-orientation 'hero 2))
                     ((equal? rel '(0 . +1))
                        (creature:set-orientation 'hero 4))
                     ((equal? rel '(-1 . 0))
                        (creature:set-orientation 'hero 6)))

                  ; и пошлем его в дорогу
                  (creature:move-with-animation 'hero rel 'run #f)))
            ; todo: хорошо бы добавить проверку на проигрыш...
            ; а теперь проверка на выигрыш:
            (let ((win? (fold (lambda (f b o)
                                 (fold (lambda (f b o)
                                          (and f (if (eq? b grid-id)
                                                   (eq? o gemH-id) #true)))
                                    f b o))
                           #true (level:get-layer 'background) (level:get-layer 'object))))
               (if win?
                  (creature:play-animation 'hero 'cast 'cast)
                  (set-world-busy #false)))
            (this itself))
         (else
            (print "logic: unhandled event: " msg)
            (this itself)))))))

(print "ok")