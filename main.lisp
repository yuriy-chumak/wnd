#!/usr/bin/ol

; читаємо нашого героя
(define характер (fasl-load "hero.fasl" #false))
(if (eq? характер #false)
   (begin
      (print "будь ласка, спершу створіть героя.") (halt 1)))

; (if a b c)


; зададим конфигурацию графического окна
(define-library (lib gl config)
(export config) (import (otus lisp))
(begin
   (define config (list->ff `(
      ; размеры окна в знакоместах
      ; напомню, что мы используем фиксированный шрифт размера 9*16
      (width . ,(* 2 80 9))
      (height . ,(* 2 16 25)))))))
(import (lib gl config))

; игра пошаговая! посему все ходы только после клика "я готов" (пока это ПКМ) и все НПС
; должны ходить по-очереди. при этом демонстрировать что они делают.
(define screen-width (getf config 'width))
(define screen-height (getf config 'height))

; -=( main )=------------------------------------
; подключаем графические библиотеки, создаем окно
(import (lib gl2))
(import (otus ffi))
(import (lib soil))
(print "---------------")
(gl:set-window-title "Drawing the tiled map")

; сразу нарисуем сплеш
(glOrtho 0 1 1 0 0 1)
(glEnable GL_TEXTURE_2D)
(define id ; OpenGL texture splah id
   (SOIL_load_OGL_texture (c-string "tmp/splash.png") SOIL_LOAD_RGBA SOIL_CREATE_NEW_ID 0))
(glBindTexture GL_TEXTURE_2D id)
(glBegin GL_QUADS)
   (glTexCoord2f 0 0)
   (glVertex2f 0 0)

   (glTexCoord2f 1 0)
   (glVertex2f 1 0)

   (glTexCoord2f 1 1)
   (glVertex2f 1 1)

   (glTexCoord2f 0 1)
   (glVertex2f 0 1)
(glEnd)
(glDisable GL_TEXTURE_2D)
(gl:SwapBuffers (interact 'opengl (tuple 'get 'context)))
(glDeleteTextures 1 (list id))

; -------------------------------------------------------
; теперь текстовая консолька
(import (lib gl console))

; временное окно дебага (покажем fps):
(define fps (create-window 70 24 10 1))
(define started (time-ms)) (define time (list 0))
(define frames '(0 . 0))

(set-window-writer fps (lambda (type)
   (set-car! frames (+ (car frames) 1))
   (let ((now (time-ms)))
      (if (> now (+ started (car time) 1000))
         (begin
            (set-cdr! frames (car frames))
            (set-car! frames 0)
            (set-car! time (- now started)))))
   (type GRAY (cdr frames) " fps")
))


;; (define hero (fasl-load "hero.bin" #empty))
;; (print hero)
;; (print (car hero))
;; (print (type (caar hero)))
;; (print (eq? (caar hero) 'sex))
;; (halt 1)

(define info (create-window 0 1 12 24))
(set-window-background info BLACK)
(set-window-border info GRAY)
(set-window-writer info (lambda (echo)
   (echo LIGHTBLUE (case 0 ;(getf hero 'race)
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
   (echo LIGHTBLUE "Воин\n")
   (echo LIGHTBLUE "Новобранец\n")
   (echo WHITE "УРОВЕНЬ   1\n")
   (echo WHITE "ОПЫТ      0\n")
))

; ----------------
; музычка...
;,load "music.lisp"


(import (lib math))
(import (otus random!))

; ключевые объекты игры:
; -=( level )=-----------------
;     заведует игровой картой
,load "game/level.lisp"
,load "animations.lisp"

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
(define (time-ms) (let*((ss ms (clock))) (+ (* ss 1000) ms)))


; load level
(level:load "river_encampment")

,load "creature.lisp"
,load "ai.lisp"
,load "animations.lisp"

; временная функция работы с level-collision
(define collision-data (interact 'level (tuple 'get 'collision)))
(define H (length collision-data))
(define W (length (car collision-data)))

; internal function (temp debug purposes)
(define (at x y)
   (if (and (< -1 x W) (< -1 y H))
      (lref (lref collision-data y) x)))



; временная сопрограма creatures,
; TODO: поместить ее в отдельный файл и добавить
; функции "создать кричу, убить кричу и т.д. и т.п."



; loading animations
;(define antlion-animation (list->ff (ini-parse-file "animations/antlion.ini")))


;; ; find first skeleton id
;; (define hero_pos (xml-get-subtag level 'properties))
;; (define hero_pos (car (filter (lambda (tag) (string-eq? (xml-get-attribute tag 'name "") "hero_pos")) (xml-get-value hero_pos))))
;; (define hero_pos (xml-get-attribute hero_pos 'value #f))
;; (define hero_pos (split-by-comma hero_pos))
;; (define hero_pos (cons
;;    (string->number (car hero_pos) 10)
;;    (string->number (cadr hero_pos) 10)))
;; (define hero_pos (cons 51 61)) ; TODO: move to level

(define calculating-world (box 0))


; -=( hero )=---------
(define hero 'hero)
(define hero-destination '(51 . 61)) ; точка, куда надо идти герою

(make-creature 'hero #empty) (mail 'creatures (tuple 'set 'hero hero))
(mail hero (tuple 'set-location '(51 . 61)))

(creature:set-animations hero 'zombie "animations/zombie.ini")
;не, не будем задавать никакую текущую анимацию персонажу. ;(interact 'hero (tuple 'set-current-animation 'run))

;; (mail 'hero (tuple 'set 'idle (lambda (itself)
;;    (define location (getf itself 'location))
;;    ;(print "location: " location)
;;    ;(print "hero-destination: " hero-destination)
;;    (let ((move (A* collision-data
;;                   (car location) (cdr location)
;;                   (car hero-destination) (cdr hero-destination))))
;;       ;(print "move: " move)
;;       (if move
;;          (creature:move itself (cons (ref move 1) (ref move 2)))
;;          itself)))))


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

; -=( skeletons )=-----------------
;,load "mob.lisp" ; стейт-машина скелета, в этом файле записано его поведение

(define skeletons (iota 10 1000)) ; 6 скелетонов, начиная с номера 1000 (пускай скелетоны будут номерные)
(for-each (lambda (id)
      (make-creature id #empty)) ; создаем их
   skeletons)
(mail 'creatures (tuple 'set 'skeletons skeletons)) ; и сохраним в списке всех npc

; теперь поместим скелетонов в случайные места на карте
(for-each (lambda (id)
      ; поиск свободного места
      (let loop ()
         (let ((x (rand! 64))
               (y (rand! 64)))
            (if (eq? 0 (at x y))
               (begin
                  (mail id (tuple 'set-location (cons x y)))
                  (mail id (tuple 'set-orientation (rand! 8))))
               (loop)))))
   skeletons)

; зададим скелетонам машину состояний и пусть все пока спят
(for-each (lambda (id)
      (mail id (tuple 'set 'state-machine default-mob-state-machine))) ; state machine
   skeletons)
; set initial state
(for-each (lambda (id)
      (mail id (tuple 'set 'state 'sleeping))) ; initial state
   skeletons)


(for-each (lambda (id)
      (creature:set-animations id 'goblin "animations/goblin.ini")
      (creature:set-current-animation id 'stance))
   skeletons)

; а давайте-ка пометим на карту сундук? вместо героя
; и пускай этот сундук сразу телепортируется в новое место,
; как только его кто-то нашел?
(make-creature 'chest #empty)

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

;              x-left             x-right y-left         y-right
;(define window (vector (+ -32 -800) -32 (+ 3645 32 -800) (+ 2048 32)))
(define window (vector -1920 -64 1920 (- 2160 64)))
(define (resize scale)
   (let*((x (floor (/ (+ (ref window 3) (ref window 1)) 2)))
         (w (floor (* (- (ref window 3) (ref window 1)) (/ scale 2))))
         (y (floor (/ (+ (ref window 4) (ref window 2)) 2)))
         (h (floor (* (- (ref window 4) (ref window 2)) (/ scale 2)))))
      (set-ref! window 1 (- x w))
      (set-ref! window 2 (- y h))
      (set-ref! window 3 (+ x w))
      (set-ref! window 4 (+ y h))))
(define (move dx dy)(let*((x (floor (* (- (ref window 3) (ref window 1)) 0.01)))
         (y (floor (* (- (ref window 4) (ref window 2)) 0.01))))
      (set-ref! window 1 (+ (ref window 1) (* dx x)))
      (set-ref! window 2 (- (ref window 2) (* dy y)))
      (set-ref! window 3 (+ (ref window 3) (* dx x)))
      (set-ref! window 4 (- (ref window 4) (* dy y)))))


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
         ;; (print "x: " (inexact x))
         ;; (print "y: " (inexact y))
      (cons (floor x) (floor y))))))))


; init
(glShadeModel GL_SMOOTH)
(glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)

(gl:hide-cursor)

(define timestamp (box 0))
; draw
(gl:set-renderer (lambda (mouse)
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

      ;;       ; и герою тоже пошлем сообщение
      ;;       ;(mail 'hero (tuple 'idle))

      ;;       ;; ; maybe move chest
      ;;       ;; (for-each (lambda (id)
      ;;       ;;       (define pos (interact id (tuple 'get-location)))

      ;;       ;;       (if (equal? pos destination)
      ;;       ;;          (let loop ()
      ;;       ;;             (let ((x (rand! 64))
      ;;       ;;                   (y (rand! 64)))
      ;;       ;;                (if (eq? 0 (at x y))
      ;;       ;;                   (begin
      ;;       ;;                      (set-car! destination x)
      ;;       ;;                      (set-cdr! destination y))
      ;;       ;;                   (loop))))))
      ;;       ;;    skeletons)
      ;;    )))

   ; убедимся, что ВСЕ npc готовы к отображению
   ;(interact 'creatures (tuple 'ready?))

   ; drawing
   (glClearColor 0.0 0.0 0.0 1)
   (glClear GL_COLOR_BUFFER_BIT)
   (glLoadIdentity)
   (glOrtho (ref window 1) (ref window 3) (ref window 4) (ref window 2) -1 1) ; invert axis Y on screen!
   (glEnable GL_TEXTURE_2D)
   (glEnable GL_BLEND)

   ; попросим уровень отрисовать себя
   (interact 'level (tuple 'draw (if mouse (xy:screen->tile mouse)))) ; (level:draw)

   ; окошки, консолька, etc.
   (render-windows)

   ; let's draw mouse pointer
   (if mouse
      (let*((ms (mod (floor (/ (time-ms) 100)) 40))
            (tile (getf (interact 'level (tuple 'get 'tileset))
               (if (eq? (unbox calculating-world) 0)
                  (+ 1212 ms)
                  (+ 1292 ms))))
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
            (glVertex2f (+ x 64) y)

            (glTexCoord2f (ref st 3) (ref st 4))
            (glVertex2f (+ x 64) (+ y 64))

            (glTexCoord2f (ref st 1) (ref st 4))
            (glVertex2f x (+ y 64))
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
         (mail 'music (tuple 'shutdown))
         (shutdown 1))))) ; q - quit

(gl:set-mouse-handler (lambda (button x y)
   (print "mouse: " button " (" x ", " y ")")
   (if (eq? (unbox calculating-world) 0) ; если мир сейчас не просчитывается (todo: оформить отдельной функцией)
      (cond
         ((eq? button 1)
            ; tile - tile-under-attack
            (let ((tile (xy:screen->tile (cons x y))))
               (begin
                  (set-car! calculating-world 42)
                  (mail 'game (tuple 'fire-in-the-tile tile))))
            #true)
         ((eq? button 3) ; ПКМ
            (if (eq? (unbox calculating-world) 0) ; если мир сейчас не просчитывается (todo: оформить отдельной функцией)
               (begin
                  (set-car! calculating-world 42)
                  (mail 'game (tuple 'turn)))))
         (else
            ; nothing
            #true))
   )))

(fork-server 'game (lambda ()
   (let this ((itself #empty))
   (let*((envelope (wait-mail))
         (sender msg envelope))
      (tuple-case msg
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
               (interact 'creatures (tuple 'get 'skeletons)))
            (print "turn done.")

            ; вроде все обработали, можно переходить в состояние "готов к следующему ходу"
            (set-car! calculating-world 0)
            (this itself))
         ((fire-in-the-tile xy)
            (for-each (lambda (creature)
                  ; для тестов - пусть каждый скелет получает урон "-50"
                  (if (equal? (interact creature (tuple 'get 'location)) xy)
                     (ai:make-action creature 'damage 50)))
               (interact 'creatures (tuple 'get 'skeletons)))
            (set-car! calculating-world 0)
            (this itself))
         (else
            (print "logic: unhandled event: " msg)
            (this itself)))))))
