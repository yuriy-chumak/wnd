#!/usr/bin/ol

; читаємо нашого героя
(define характер (fasl-load "hero.fasl" #false))
(if (eq? характер #false)
   (begin
      (print "будь ласка, спершу створіть героя.") (halt 1)))


; ----------------------------------
; зададим размеры графического окна
(define-library (lib gl config)
(export config) (import (otus lisp))
(begin
   (define config (pairs->ff `(
      ; напомню, что мы используем фиксированный шрифт размера 9*16
      (width  . ,(* 1  9 80))      ; 80 знакомест в ширину
      (height . ,(* 1 16 25))))))) ; 25 знакомест в высоту
(import (lib gl config))

; игра пошаговая! посему все ходы только после клика "я готов" (пока это ПКМ) и все НПС
; должны ходить по-очереди. при этом демонстрировать что они делают.

; -=( main )=------------------------------------
; подключаем графические библиотеки, создаем окно
(import (lib gl2))
(gl:set-window-title "Wizards and Dragons")
(import (otus ffi))
(import (lib soil))

; -=( сразу нарисуем сплеш )=---------------------------
(glOrtho 0 1 1 0 0 1)
(glEnable GL_TEXTURE_2D)
(define id ; OpenGL texture splash ID
   (SOIL_load_OGL_texture (c-string "common/splash.jpg") SOIL_LOAD_RGBA SOIL_CREATE_NEW_ID 0))
(glBindTexture GL_TEXTURE_2D id)
(glBegin GL_QUADS)
   ; рисуем на весь экран квадратик с текстурой
   (for-each (lambda (xy)
         (glTexCoord2f (car xy) (cdr xy))
         (glVertex2f (car xy) (cdr xy)))
      '((0 . 0) (1 . 0) (1 . 1) (0 . 1)))
(glEnd)
(glDisable GL_TEXTURE_2D)
(gl:SwapBuffers (interact 'opengl (vector 'get 'context))) ; todo: make a function
(glDeleteTextures 1 (list id)) ; и спокойно удалим сплеш текстуру

; -------------------------------------------------------
; теперь запустим текстовую консольку
(import (lib gl console))

; временное окно дебага (покажем fps):
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

; информационное окно состояния героя
(define info (create-window 0 1 11 24))
(set-window-background info BLACK)
(set-window-border info GRAY)
(set-window-writer info (lambda (echo)
   (move-to 0 0)
   (echo LIGHTBLUE (case (list-ref характер 1)
      (0 "Человек")
      (1 "Полуэльф")
      (2 "Эльф")
      (3 "Хоббит")
      (4 "Карлик")
      (5 "Гном")
      (6 "Полуорк")
      (7 "Полутролль")
      (8 "Дунадан")
      (9 "Высший Эльф")
      (else "unknown")))

   (move-to 0 1)
   (echo LIGHTBLUE (case (list-ref характер 2)
      (0 "Воин")
      (1 "Маг")
      (2 "Священник")
      (3 "Вор")
      (4 "Следопыт")
      (5 "Паладин")
      (else "unknown")))

   (move-to 0 2)
   (echo LIGHTBLUE "Новобранец")

   (move-to 0 3)
   (echo WHITE "УРОВЕНЬ   1")

   (move-to 0 4)
   (echo WHITE "ОПЫТ      0")
))

; ----------------
; музычка...
;,load "music.lisp" ; временно отключена

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
,load "ai.lisp"

;;; -=( creatures )=-----------------
;;;  'creatures - заведует всеми живыми(или оживленными) созданиями

; =============================
; 1. Загрузим игровой уровень
(level:load "river_encampment.tmx")

; временная функция работы с level-collision
(define collision-data (level:get-layer 'collision))

(define H (length collision-data))       ; высота уровня
(define W (length (car collision-data))) ; ширина уровня

; временная функция: возвращает collision data
;  по координатам x,y на карте
(define (at x y)
   (if (and (< -1 x W) (< -1 y H))
      (lref (lref collision-data y) x)))

; =================================================================
; -=( hero )=---------
(define hero (make-creature 'hero #empty))
; зададим позицию героя в мире
(creature:set-location 'hero (cons 28 33)) ; старый способ перемещения героя
((hero 'set-location) (cons 28 33))        ; новый способ перемещения героя - выбрать какой лучше

; зададим анимации герою, в нашем случае он будет выглядеть как скелет
(creature:set-animations 'hero 'skeleton "animations/skeleton.ini")
(creature:set-current-animation 'hero 'stance) ; пусть он просто стоит


; -=( mobs )=-----------------
;,load "mob.lisp" ; стейт-машина скелета, в этом файле записано его поведение

(define monsters (iota 10 1000)) ; 10 монстров, начиная с номера 1000 (пускай мобы будут номерные)
(for-each (lambda (id)
      (make-creature id #empty)) ; создаем их
   monsters)
(mail 'creatures (vector 'set 'monsters monsters)) ; и сохраним в списке всех npc

; теперь поместим монстров в случайные места на карте
(for-each (lambda (id)
      ; поиск свободного места
      (let loop ()
         (let ((x (rand! W))
               (y (rand! H)))
            (if (eq? 0 (at x y))
               (begin
                  (creature:set-location id (cons x y))
                  (creature:set-orientation id (rand! 8))

                  (creature:set-animations id 'goblin "animations/goblin.ini")
                  (creature:set-current-animation id 'stance))
               (loop)))))
   monsters)

;; ; зададим скелетонам машину состояний и пусть все пока спят
;; (for-each (lambda (id)
;;       (mail id (vector 'set 'state-machine default-mob-state-machine))) ; state machine
;;    monsters)
;; ; set initial state
;; (for-each (lambda (id)
;;       (mail id (vector 'set 'state 'sleeping))) ; initial state
;;    monsters)

; --------------------------------------------------------------------
; окно, через которое мы смотрим на мир

;              x-left         y-left    x-right          y-right
;(define window (vector (+ -32 -800) -32 (+ 3645 32 -800) (+ 2048 32)))
(define window (vector -1920 -64 1920 (- 2160 64)))
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
         (w (ref gl:window-dimensions 3)) (h (ref gl:window-dimensions 4)))
   (let ((X (floor (+ x1 (/ (* (car xy) x2-x1) w))))
         (Y (floor (+ y1 (/ (* (cdr xy) y2-y1) h)))))
   (let ((w (level:get 'tilewidth))
         (h (level:get 'tileheight)))
   (let ((x (+ (/ X w) (/ Y h)))
         (y (- (/ Y h) (/ X w))))
      (cons (floor x) (floor y))))))))

;(resize 1/3) ; временно: увеличим карту в 3 раза

; init
(glShadeModel GL_SMOOTH)
(glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
(gl:hide-cursor)

; служебные переменные
(define timestamp (box 0))
(define calculating-world (box #false))

; draw
(gl:set-renderer (lambda (mouse)
   ; тут мы поворачиваем нашего шероя в сторону мышки
   (unless (unbox calculating-world)
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
            ;;       (mail id (vector 'process-event-transition-tick)))
            ;;    (interact 'creatures (vector 'get 'skeletons)))
         )))

   ; теперь можем и порисовать: очистим окно и подготовим оконную математику
   (glClearColor 0.0 0.0 0.0 1)
   (glClear GL_COLOR_BUFFER_BIT)
   (glLoadIdentity)
   (glOrtho (ref window 1) (ref window 3) (ref window 4) (ref window 2) -1 1) ; invert axis Y on screen!
   (glEnable GL_TEXTURE_2D)
   (glEnable GL_BLEND)

   ; теперь попросим уровень отрисовать себя
   (define creatures (map (lambda (id)
         (vector (interact id (vector 'get-location)) (interact id (vector 'get-animation-frame))))
      (interact 'creatures (vector 'get 'monsters))))

   (level:draw (if mouse (xy:screen->tile mouse)) creatures)

   ; окошки, консолька, etc.
   (render-windows)

   ; let's draw mouse pointer
   (if mouse
      (let*((ms (mod (floor (/ (time-ms) 100)) 40))
            (tile (getf (level:get 'tileset)
                        (+ (level:get-gid 'pointer)
                           (if (world-busy?) 1 0))))
                           ;; (cond
                           ;;    ((world-busy?) 1)
                           ;;    ((let ((xy (xy:screen->tile mouse)))
                           ;;       (and (< (car xy) (level:get 'width))
                           ;;            (< (cdr xy) (level:get 'height))
                           ;;            (>= (car xy) 0)
                           ;;            (>= (cdr xy) 0)
                           ;;            (A* collision-data xy (creature:get-location 'hero))))
                           ;;       0)
                           ;;    (else 3)))))
                        ;; (unless (unbox calculating-world)
                        ;;    (+ 1212 ms)
                        ;;    (+ 1292 ms))))
            (w (/ (- (ref window 3) (ref window 1)) 48)) ;  размер курсора
            (st (ref tile 5))
            ; window mouse to opengl mouse:
            (x (+ (ref window 1) (* (car mouse) (- (ref window 3) (ref window 1)) (/ 1 (ref gl:window-dimensions 3)))))
            (y (+ (ref window 2) (* (cdr mouse) (- (ref window 4) (ref window 2)) (/ 1 (ref gl:window-dimensions 4))))))
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


   ; coordinates
   #|
   (glDisable GL_TEXTURE_2D)
   (glEnable GL_LINE_STIPPLE)
   (glLineWidth 2.0)
   (glLineStipple 2 #xAAAA)
   (glBegin GL_LINES)
      (glColor3f 1 0 0)
      (glVertex2f -4096 0)
      (glVertex2f +4096 0)
      ;; (glColor3f 1 0 1)
      ;; (glVertex2f -4096 1024)
      ;; (glVertex2f +4096 1024)
      (glColor3f 0 1 0)
      (glVertex2f 0 -4096)
      (glVertex2f 0 +4096)
      ;; (glColor3f 0 1 1)
      ;; (glVertex2f 1024 -4096)
      ;; (glVertex2f 1024 +4096)
   (glEnd)
   (glDisable GL_LINE_STIPPLE)
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
      (#x18
         ;(mail 'music (vector 'shutdown))
         (halt 1))))) ; q - quit

(gl:set-mouse-handler (lambda (button x y)
   (print "mouse: " button " (" x ", " y ")")
   (unless (unbox calculating-world) ; если мир сейчас не просчитывается (todo: оформить отдельной функцией)
      (cond
         ((eq? button 1)
            (let ((tile (xy:screen->tile (cons x y))))
               (set-car! calculating-world 42)
               (mail 'game (vector 'run tile))))
         ((eq? button 3) ; ПКМ
            (unless (unbox calculating-world) ; если мир сейчас не просчитывается (todo: оформить отдельной функцией)
               (begin
                  (set-car! calculating-world 42)
                  (mail 'game (vector 'turn)))))
         (else
            ; nothing
            #true))
   )))

(fork-server 'game (lambda ()
   (let this ((itself #empty))
   (let*((envelope (wait-mail))
         (sender msg envelope))
      (vector-case msg
         ((turn)
            ; 1. Каждому надо выдать некотрое количество action-points (сколько действий он может выполнить за ход)
            ;  это, конечно же, зависит от npc - у каждого может быть разное
            ; TBD.

            ; 2. Отсортировать всех по уровню инициативности, то есть кто имеет право ударить первым
            ;  это тоже зависит от npc

            ; 3. И только теперь подергать каждого - пусть походит
            ;  причем следующего можно дергать только после того, как отработают все запланированные анимации хода
            (for-each (lambda (creature)
                  ; для тестов - пусть каждый скелет получает урон "-50"
                  (ai:make-action creature 'damage 50))
               (interact 'creatures (vector 'get 'skeletons)))
            (print "turn done.")

            ; вроде все обработали, можно переходить в состояние "готов к следующему ходу"
            (set-car! calculating-world #false)
            (this itself))
         ((fire-in-the-tile xy)
            (for-each (lambda (creature)
                  ; для тестов - пусть каждый скелет получает урон "-50"
                  (if (equal? (interact creature (vector 'get 'location)) xy)
                     (ai:make-action creature 'damage 50)))
               (interact 'creatures (vector 'get 'skeletons)))
            (set-car! calculating-world #false)
            (this itself))

         ((run to)
            (let loop ()
               (let*((hero (creature:get-location 'hero))
                     (move (A* collision-data hero to)))
                  (if move (begin
                     ; повернем героя в ту сторону, куда он собрался идти
                     (cond
                        ((equal? move '(0 . -1))
                           (creature:set-orientation 'hero 0))
                        ((equal? move '(+1 . 0))
                           (creature:set-orientation 'hero 2))
                        ((equal? move '(0 . +1))
                           (creature:set-orientation 'hero 4))
                        ((equal? move '(-1 . 0))
                           (creature:set-orientation 'hero 6)))
                     ; и пошлем его в дорогу
                     (creature:move-with-animation 'hero move 'run #f)
                     (unless (equal? (creature:get-location 'hero) to)
                        (loop))))))
            (set-car! calculating-world #false)
            (this itself))
         (else
            (print "logic: unhandled event: " msg)
            (this itself)))))))
