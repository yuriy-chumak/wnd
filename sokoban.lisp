#!/usr/bin/ol
; правила создания уровней:
; * уровень должен содержать 4 слоя, в порядке снизу-вверх
;   4) background, слой в котором нарисована "земля"
;   3) object, слой с недвижимыми игровыми объектами
;   2) setup, слой с движимыми игровыми объектами, причем
;     * первая строка слоя - настроечная и не отображается/не просчитывается, в ней в первых трех клетках должны быть записаны "настроечные" тайлы:
;       * в клетке (0,0) - "место": место, куда надо поставить "чаши"
;       * в клетке (1,0) - "чаша" : предмет, который должен двигать игрок и который надо в процессе игры установить на какое-нибудь из "мест"
;       * в клетке (2,0) - "горящая чаша": "чаша", которую должен двигать игрок и которая сигнализирует, что она уже стоит на "месте"
;     * остальные строки уровня - "рабочие", в них расставляются "чаши", "горящие чаши", если есть и ставится игрок на клетку, с которой начинается игра
;     * обратите внимание, что тайл "игрока" на карте, это всегда первый тайл слоя с анимацией игрока; таким образом если в карту включено три анимированных слоя "skeleton", "antlion", "wyvern" - то можно просто сменив тайл игрока на первый из этих трех сменить самого игрока со скелетона, например, на виверну.
;   1) collision, слой в котором записаны возможные для движения игрока (и установки "чаши") тайлы; причем пустой тайл означает возможность использования тайла, любой не пустой - запрет.
;     * на этот слой накладывается условие - из тайла в котором стоит игрок НЕ должно быть выхода за границу карты.
(import
   (lib math) (otus random!)
   (lang sexp) (scheme misc)
   (file xml) (lib rlutil))

; ----------------------------------
; зададим размеры графического окна
(define-library (lib gl config)
(export config) (import (otus lisp))
(begin
   (define config (pairs->ff `(
      ; напомню, что мы используем фиксированный шрифт размера 9*16
      (width  . ,(* 1  9 80))      ; 80 знакомест в ширину
      (height . ,(* 1 16 25))      ; 25 знакомест в высоту
      (move-with-push . #false)    ; двигаться ли вместе с толчком чаши
)))))
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

(gl:set-window-title "Sokoban ...")

; -------------------------------------------------------
; теперь текстовая консолька
(import (lib gl console))

; окно дебага (покажем fps):
(define fps (create-window 60 24 10 1))
(define started (time-ms)) (define time '(0))
(define frames '(0 . 0))
(define steps (box 0))

(set-window-writer fps (lambda (print)
   (set-car! frames (+ (car frames) 1))
   (let ((now (time-ms)))
      (if (> now (+ started (car time) 1000))
         (begin
            (set-cdr! frames (car frames))
            (set-car! frames 0)
            (set-car! time (- now started)))))
   (print WHITE (car steps) " steps,  ")
   (print GRAY (cdr frames) " fps")
))

(define notes (create-window 3 24 40 2))

(define messages (pairs->ff `(
   (1 . (,GRAY "Нажмите Q для выхода, Z для отмены хода."))
   (2 . (,LIGHTGREEN "Вы выиграли!"))
)))
(define notes-message (box 1))
(set-window-writer notes (lambda (print)
   (apply print (messages (car notes-message)))
))


; -----------------------------
; список отмены ходов
; сюда записывается кто и куда переместился
(fork-server 'stash (lambda ()
   (let this ((itself #null))
   (let*((envelope (wait-mail))
         (sender msg envelope))
      (case msg
         (['push message]
            (this (cons message itself)))
         (['pop]
            (case itself
               (#null
                  (mail sender (vector 'empty))
                  (this #null))
               (else
                  (mail sender (car itself))
                  (this (cdr itself))))))))))

; -=( level )=-----------------
;     заведует игровой картой
,load "game/level.lisp"

,load "creature.lisp"

; =============================
; 1. Загрузим игровой уровень
(define level-name (or (lref *vm-args* 0) "001.tmx"))
(level:load level-name)

; временная функция работы с level-collision
(define collision-data (level:get-layer 'collision))

(define H (length collision-data))       ; высота уровня
(define W (length (car collision-data))) ; ширина уровня
; а теперь подготовим collision-data к быстрой работе - превратим его в статический vector
(define collision-data (list->vector (map list->vector collision-data)))
(define collision-data-at (lambda (xy)
   (ref (ref collision-data (+ (cdr xy) 1)) (+ (car xy) 1))))

; настройки уровня:
(define setup-data (level:get-layer 'setup))
; номера жаровни, горящей жаровни, решетки
(define grid-id (lref (lref setup-data 0) 0))
(define gem-id  (lref (lref setup-data 0) 1))
(define gemH-id (lref (lref setup-data 0) 2))

(print "grid-id: " grid-id)
(print "gem-id: " gem-id)

; ========================
; найдем все gem-id объекты и сложим их в список "монстров"
(define gems
   (fold (lambda (L line y)
            (fold (lambda (L o x)
                     (if (or (eq? o gem-id) (eq? o gemH-id))
                        (cons (cons (cons x y) o) L)
                        L))
               L line (iota W)))
      #null (cdr setup-data) (iota (- H 1) 1)))

; =================================================================
; A*
; упрощенный A* алгоритм под нашу задачу.
; допущения:
; 1. выход за границу уровня невозможен (поэтому мы уберем проверки границ)
;#|
(define (A* from to)
   (let*((xy from) ; начальное значение поиска пути
         ; для быстрого обращения к элементам карты сконвертируем ее из списка в кортеж
         ; функция (list->vector) реализована на стороне виртуальной машины, посему очень быстрая
         (level collision-data)
         ; получить значение из карты по координатам '(x.y), координаты начинаются с 0
         (level-at collision-data-at)
         ; функция хеширования пары '(x.y), для быстрого поиска в словаре
         (hash (lambda (xy)
            (+ (<< (car xy) 16) (cdr xy))))
         (gems (fold (lambda (ff o)
                        (put ff (hash (car o)) #true))
                  #empty gems))
         ; пуста ли клетка карты "в голове" персонажа, работает для любых координат, даже отрицательных
         (floor? (lambda (xy)
            (and
               (eq? (level-at xy) 0)
               (eq? (get gems (hash xy) #f) #f)))))

   (if (equal? from to) ; а никуда идти и не надо?
      #false ;(vector 0 0 #empty #empty)
   ; ищем без ограничений, это упростит алгоритм
   (let step1 ((c-list-set #empty)
               (o-list-set (put #empty (hash xy)  (vector xy #f  0 0 0))))
      (if (eq? o-list-set #empty) ; некуда идти :(
         #false ;(vector 0 0 #empty #empty)

      ; найдем клетку с минимальной стоимостью:
      (let*((f (ff-fold (lambda (s key value)
                           (if (< (ref value 5) (car s))
                              (cons (ref value 5) value)
                              s))
                  (cons 9999 #f) o-list-set))
;                  (_ (print "next: " f))
            (xy (ref (cdr f) 1)) ; положение клетки с минимальным весом '(x.y)
            ; перенесем ее из открытого в закрытый список
            (o-list-set (del o-list-set (hash xy)))
            (c-list-set (put c-list-set (hash xy) (cdr f))))

         ; если мы наконец добрались до нужной точки
         (if (equal? xy to)
            (let rev ((xy xy))
               ; обратный проход по найденному пути, вернуть только первый шаг
               ;  (в сторону предполагаемого маршрута
               (let*((parent (ref (get c-list-set (hash xy) #f) 2)) ; todo: переделать
                     (parent-of-parent (ref (get c-list-set (hash parent) #f) 2)))
                  (if parent-of-parent (rev parent)
                     (cons ;(vector
                        (- (car xy) (car parent))
                        (- (cdr xy) (cdr parent))
                        ;c-list-set
                        ;o-list-set
                        ))))
            ; хм, пока еще не добрались?
            ; тогда:
            ; Проверяем все соседние клетки.
            ;  игнорируем те, которые находятся в закрытом списке или непроходимы
            ;  (поверхность со стенами, водой), остальные добавляем в открытый список,
            ;  если они там еще не находятся. Делаем выбранную клетку "родительской"
            ;  для всех этих клеток.
            (let*((x (car xy))
                  (y (cdr xy))
                  (o-list-set (fold (lambda (o v)
                                 (if (and
                                       (floor? v)                       ; если в эту клетку можно встать
                                       (eq? #f (get c-list-set (hash v) #f))) ; и она не в закрытом списке

                                    (let ((G (+ (ref (get c-list-set (hash xy) #f) 3) 1)); G родителя + 1
                                          ; H calculated by "Manhattan method"
                                          ; https://web.archive.org/web/20160930004511/http://www2.in.tu-clausthal.de/~zach/teaching/info_literatur/A_Star/A_star_tutorial/heuristics.htm.html
                                          (H (* (+ (abs (- (car v) (car to)))
                                                   (abs (- (cdr v) (cdr to)))) 2))
                                          ; 6: Если соседняя клетка уже находится в открытом списке
                                          (got (get o-list-set (hash v) #f)))

                                       ; если эта клетка уже в списке
                                       (if got
                                          (if (< G (ref got 3)) ; но наш путь короче
                                             (put o (hash v)  (vector v xy  G H (+ G H)))
                                             ;else ничего не делаем
                                             o)
                                          ; else
                                          (put o (hash v)  (vector v xy  G H (+ G H)))))
                                    o))
                                 o-list-set (list
                                                (cons x (- y 1))
                                                (cons x (+ y 1))
                                                (cons (- x 1) y)
                                                (cons (+ x 1) y)))))
               (step1 c-list-set o-list-set)))))))))
;|#

; =================================================================
; -=( hero )=---------
(define hero (make-creature 'hero #empty))
; зададим позицию героя в мире (найдем его в сетапе)
(creature:set-location 'hero
   (call/cc (lambda (return)
      (define used (list 0 gem-id gemH-id))
      (map (lambda (y row)
               (map (lambda (x id)
                        (unless (has? used id)
                           (return (cons x y))))
                  (iota W) row))
         (iota (- H 1) 1) ; skip first line (with row id's data)
         (cdr setup-data)))))

; новое "улучшение" - а давайте не будем задавать вручную героя, но
;  прочитаем его с карты?
(define hero-id (let ((xy (creature:get-location 'hero)))
   (lref (lref setup-data (cdr xy)) (car xy))))
(print "hero-id: " hero-id)

(define hero-name
   (call/cc (lambda (return)
      (for-each (lambda (gid)
         (print "gid: " gid)
         (if (= hero-id (cdr gid))
            (return (car gid))))
         (reverse (ff->list (interact 'level (vector 'get 'gids))))))))
(print "hero-name: " hero-name)

;; (for-each (lambda (layer)
;; (ff->list (interact 'level (vector 'get 'layers))))
;; ;(for-each (lambda (layer)))

; зададим анимации герою, в нашем случае он будет выглядеть как скелет
(creature:set-animations 'hero hero-name (fold string-append "animations" (list "/" (symbol->string hero-name) ".ini")))
(creature:set-current-animation 'hero 'stance) ; пусть он просто стоит

; --------------------------------------------------------------------
; окно, через которое мы смотрим на мир
; todo: отцентрируем его на героя

(define tile (cons (floor (/ W 2)) (floor (/ H 2))));(creature:get-location 'hero))
(define w (level:get 'tilewidth))
(define h (level:get 'tileheight))
(define cx (- (* (car tile) (/ w 2)) (* (cdr tile) (/ w 2))))
(define cy (+ (* (car tile) (/ h 2)) (* (cdr tile) (/ h 2))))

(define width (* w (max W H)))
(define height (* h (max W H)))

(print "tile: " tile)
(print "cx: " cx ", cy: " cy)
(print "width: " width ", height: " height)


(define window (vector (- cx width) (- cy height)
                      (+ cx width) (+ cy height)))

(define (resize scale) ; изменение масштаба
   (let*((x (/ (+ (ref window 3) (ref window 1)) 2))
         (w (* (- (ref window 3) (ref window 1)) (/ scale 2)))
         (y (/ (+ (ref window 4) (ref window 2)) 2))
         (h (* (- (ref window 4) (ref window 2)) (/ scale 2))))
      (set-ref! window 1 (floor (- x w)))
      (set-ref! window 2 (floor (- y h)))
      (set-ref! window 3 (floor (+ x w)))
      (set-ref! window 4 (floor (+ y h)))))

(define (move dx dy) ; сдвинуть окно
   (let*((x (floor (* (- (ref window 3) (ref window 1)) 0.01)))
         (y (floor (* (- (ref window 4) (ref window 2)) 0.01))))
      (set-ref! window 1 (+ (ref window 1) (* dx x)))
      (set-ref! window 2 (- (ref window 2) (* dy y)))
      (set-ref! window 3 (+ (ref window 3) (* dx x)))
      (set-ref! window 4 (- (ref window 4) (* dy y)))))

(resize 0.4)

; функция перевода экранных координат в номер тайла, на который они попадают
(define (xy:screen->tile xy)
   (let ((x1 (ref window 1)) (x2 (ref window 3))
         (y1 (ref window 2)) (y2 (ref window 4)))
   (let ((x2-x1 (- x2 x1)) (y2-y1 (- y2 y1))
         (w screen-width) (h screen-height))
   (let ((X (floor (+ x1 (/ (* (car xy) x2-x1) w))))
         (Y (floor (+ y1 (/ (* (cdr xy) y2-y1) h)))))
   (let ((w (interact 'level (vector 'get 'tilewidth)))
         (h (interact 'level (vector 'get 'tileheight))))
   (let ((x (+ (/ X w) (/ Y h)))
         (y (- (/ Y h) (/ X w))))
      (cons (floor x) (floor y))))))))

; служебная функция: вычисляет, может ли герой ходить в указываемую клетку
;(define mouse-position '(-1 . -1)) ; положение, где сейчас мышка
; один шаг, можно толкать чаши
(define (step-available? from to)
   (let*((dx (- (car to) (car from)))
         (dy (- (cdr to) (cdr from)))
         (step (cons dx dy))
         (hash (lambda (xy)
            (+ (<< (car xy) 16) (cdr xy))))
         (level collision-data)
         ; получить значение из карты по координатам '(x.y), координаты начинаются с 0
         (level-at collision-data-at)

         (gems (fold (lambda (ff o)
                        (put ff (hash (car o)) #true))
                  #empty gems))

         (step-available (and
            ; только один шаг
            (or
               (and (eq? dx 0) (eq? dy +1))
               (and (eq? dx 0) (eq? dy -1))
               (and (eq? dx +1) (eq? dy 0))
               (and (eq? dx -1) (eq? dy 0)))
            ; и ничего не мешает либо пройти либо перед этим толкнуть и потом пройти
            (and
               (eq? (level-at to) 0)
               (if (getf gems (hash to)) ; если в клетке стоит чаша - то за чашей место должно быть свободно!
                  (let ((too (cons (+ (car to) dx) (+ (cdr to) dy))))
                     (and (eq? (level-at too) 0)
                          (eq? (get gems (hash too) #false) #false)))
                  ; иначе все ок
                  #true)))))
      step-available))

; можно пройти (не толкая ничего, просто пройти)
(define (move-available? from to)
   (A* from to))

; init
(glShadeModel GL_SMOOTH)
(glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
(gl:hide-cursor)

; служебные переменные
(define timestamp (box 0))

(gl:set-window-title (string-append "Sokoban " level-name))

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
;   (define gems (list
;      (vector '(0 . 1) (cons gem-id '(0 . 0)))))

   (level:draw #false gems)

   ; окошки, консолька, etc.
   (render-windows)

   ; let's draw mouse pointer
   (define from (creature:get-location 'hero))
   (if (and mouse
            (not (unbox *calculating*)))
      (let*((mousetile (xy:screen->tile mouse))
            (id (+ (level:get-gid 'pointer)
                  (cond
                     ((step-available? from mousetile) 0)
                     ((move-available? from mousetile) 2)
                     (else 1))))
            (tile (getf (interact 'level (vector 'get 'tileset)) id))
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
      (vkZ
         (mail 'game (vector 'undo)))
      (vkQ
         ;(mail 'music (vector 'shutdown))
         (halt 1))))) ; q - quit

(gl:set-mouse-handler (lambda (button x y)
   (print "mouse: " button " (" x ", " y ")")
   (unless (unbox *calculating*) ; если мир сейчас не просчитывается (todo: оформить отдельной функцией)
      (case button
         (1 ; left
            (let ((tile (xy:screen->tile (cons x y))))
               (set-world-busy #true)
               (mail 'game (vector 'move tile))))
         (5 ; scroll down
            (resize 1.1))
         (4 ; scroll up
            (resize 0.9))
         (else
            ; nothing
            #false)))))

(fork-server 'game (lambda ()
   (let this ((itself #empty))
   (let*((envelope (wait-mail))
         (sender msg envelope))
      (case msg
         (['undo]
            (case (interact 'stash (vector 'pop))
               ; простое перемещение по маршруту
               (['move step from to]
                  (creature:set-location 'hero from))
               (['push step id from gem]
                  (set-car! (car gem) (car from))
                  (set-cdr! (car gem) (cdr from))
                  (set-cdr! gem id))
               (else #false))
            (this itself))
         (['move to]
            (define from (creature:get-location 'hero))
            (cond
               ((equal? from to) ; если тыкнули на себя
                  (creature:play-animation 'hero 'block #f))
               ; сюда мы попадем либо если можно пойти на соседнее поле (возможно, с толчком чаши), либо вообще пойти куда-то
               ((step-available? from to)
                  (let ((rel (cons
                           (- (car to) (car from))
                           (- (cdr to) (cdr from)))))

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

                     ; а не надо ли подвинуть чашу перед нами?
                     (if (or
                           (let loop ((gems gems))
                              (cond
                                 ((null? gems) ; некого двигать
                                    #true)
                                 ((equal? (caar gems) to)
                                    ; хех, нашли чашу!
                                    (mail 'stash (vector 'push
                                       (vector 'push (car steps) (cdar gems) to (car gems))))
                                    ; давайте ее подвинем (мы уже раньше проверили, что ее можно двигать)
                                    (let ((x (+ (caaar gems) (car rel)))
                                          (y (+ (cdaar gems) (cdr rel)))
                                          (background (level:get-layer 'background)))
                                       (set-car! (caar gems) x)
                                       (set-cdr! (caar gems) y)
                                       ; заодно зажжем ее или потушим
                                       (if (eq? (lref (lref background y) x) grid-id) ; если на решетке - зажжем
                                          (set-cdr! (car gems) gemH-id)
                                          (set-cdr! (car gems) gem-id)))
                                    (creature:play-animation 'hero 'cast #f)
                                    (set-car! steps (+ (car steps) 1))
                                    #false)
                                 (else
                                    (loop (cdr gems)))))
                           (config 'move-with-push))
                     ; и пошлем его в дорогу (если некого двигать)
                     (begin
                        (mail 'stash (vector 'push (vector 'move (car steps) from to)))
                        (creature:move-with-animation 'hero rel 'run #f)
                        (set-car! steps (+ (car steps) 1))))))
               (else
                  ; иначе идем куда сказали
                  ; но сначала запишем этот ход в список
                  (mail 'stash (vector 'push (vector 'move (car steps) from to)))
                  (let loop ((from from))
                     (let ((rel (A* from to)))
                        (if rel ; если еще не пришли
                           ; повернем героя в нужную сторону
                           (begin
                              (cond
                                 ((equal? rel '(0 . -1))
                                    (creature:set-orientation 'hero 0))
                                 ((equal? rel '(+1 . 0))
                                    (creature:set-orientation 'hero 2))
                                 ((equal? rel '(0 . +1))
                                    (creature:set-orientation 'hero 4))
                                 ((equal? rel '(-1 . 0))
                                    (creature:set-orientation 'hero 6)))
                              (creature:move-with-animation 'hero rel 'run #f)
                              (set-car! steps (+ (car steps) 1))
                              (loop (creature:get-location 'hero))))))))

            ; а теперь проверка на выигрыш:
            ; todo: хорошо бы добавить еще и проверку на проигрыш...
            ;  проверка теперь простая - что все gems "объекты" горят
            (if (fold (lambda (r gem)
                        (and r (eq? (cdr gem) gemH-id)))
                  #true gems)
               (begin
                  (creature:play-animation 'hero 'block 'die)
                  (set-car! notes-message 2)
                  (gl:set-window-title "YOU WIN!!"))
               (set-world-busy #false))
            (this itself))
         (else
            (print "logic: unhandled event: " msg)
            (this itself)))))))

(print "ok")