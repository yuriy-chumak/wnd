#!/usr/bin/ol

; алгоритм поиска пути
; level: list of lists, collision сетка игрового уровня. все, что не 0 - непроходимо
(define (A* level from-x from-y to-x to-y)
   (let ((xy (cons from-x from-y))

         (hash (lambda (xy)
            (+ (<< (car xy) 16) (cdr xy))))
         ; пуста ли клетка карты "в голове" персонажа, работает для любых координат, даже отрицательных
         (floor? (lambda (x y)
            (let by-y ((y y) (map level))
               (if (< y 0) #true
               (if (null? map) #true
               (if (= y 0)
                  (let by-x ((x x) (map (car map)))
                     (if (< x 0) #true
                     (if (null? map) #true
                     (if (= x 0)
                        (eq? (car map) 0)
                     (by-x (- x 1) (cdr map))))))
               (by-y (- y 1) (cdr map)))))))))

   (if (and (= from-x to-x) (= from-y to-y)) ; уже пришли
      #false ;(tuple 0 0 #empty #empty)
   (let step1 ((n 999); количество шагов поиска
               (c-list-set #empty)
               (o-list-set (put #empty (hash xy)  (tuple xy #f  0 0 0))))
      (if (eq? o-list-set #empty)
         #false ;(tuple 0 0 #empty #empty) ; некуда идти - постоим

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

         ;
         (if (or (eq? n 0)
               (and
                  (eq? (car xy) to-x)
                  (eq? (cdr xy) to-y)))
            (let rev ((xy xy))
               ; обратный проход по найденному пути, вернуть только первый шаг
               ;  (в сторону предполагаемого маршрута
               (let*((parent (ref (get c-list-set (hash xy) #f) 2)) ; todo: переделать
                     (parent-of-parent (ref (get c-list-set (hash parent) #f) 2)))
                  (if parent-of-parent (rev parent)
                     (cons ;(tuple
                        (- (car xy) (car parent))
                        (- (cdr xy) (cdr parent))
                        ;c-list-set
                        ;o-list-set
                        ))))

            ; 5: Проверяем все соседние клетки.
            ;  Игнорируем те, которые находятся в закрытом списке или непроходимы
            ;  (поверхность со стенами, водой), остальные добавляем в открытый список,
            ;  если они там еще не находятся. Делаем выбранную клетку "родительской"
            ;  для всех этих клеток.
            (let*((x (car xy))
                  (y (cdr xy))
                  (o-list-set (fold (lambda (n v)
                                 (if (and
                                       (floor? (car v) (cdr v)) ; если туда можно передвинуться...
                                       (eq? #f (get c-list-set (hash v) #f)))
                                    (let ((G (+ (ref (get c-list-set (hash xy) #f) 3) 1)); G родителя + 1
                                          ; H calculated by "Manhattan method"
                                          ; http://www2.in.tu-clausthal.de/~zach/teaching/info_literatur/A_Star/A_star_tutorial/heuristics.htm.html
                                          (H (* (+ (abs (- (car v) to-x))
                                                (abs (- (cdr v) to-y))) 2))
                                          ; 6: Если соседняя клетка уже находится в открытом списке
                                          (got (get o-list-set (hash v) #f)))

                                       ; если эта клетка уже в списке
                                       (if got
                                          (if (< G (ref got 3)) ; но наш путь короче
                                             (put n (hash v)  (tuple v xy  G H (+ G H)))
                                             ;else ничего не делаем
                                             n)
                                          ; else
                                          (put n (hash v)  (tuple v xy  G H (+ G H)))))
                                    n))
                                 o-list-set (list
                                                (cons x (- y 1))
                                                (cons x (+ y 1))
                                                (cons (- x 1) y)
                                                (cons (+ x 1) y)))))
               (step1 (- n 1) c-list-set o-list-set)))))))))

; =================================================================
(define (ai:make-action creature action . args)
   (let ((state (interact creature (tuple 'get 'state)))
         (state-machine (or (interact creature (tuple 'get 'state-machine)) #empty)))
      (print "state: " state)
      (print "state-machine: " state-machine)

      (let*((state (get state-machine state #empty))
            (handler (getf state action)))
         (print "handler: " handler)
         (let ((state (apply handler (cons creature args))))
            (print "new-state: " state)
            (if state (mail creature (tuple 'set 'state state))))
         (print "ok."))))

; =================================================================
; NEW STATE SUBSYSTEM:
; каждое состояние первым параметром принимает pnc, чей стейт
; набор ключей в стейтах - то, что делают с нами (и что может изменить наш стейт)
; отдельно есть набор функций того, что можем делать мы

(define (do-nothing . creature)
   #false) ; состояние не меняем

(define (got-damage creature damage)
   (print creature ": got-damage " damage)
   ; пока просто будем отнимать урон от здоровья, без всякой умной логики
   (let*((life (or (interact creature (tuple 'get 'life)) 100))
         (life (- life damage)))
      ; todo: рисуем партикл "- ХХХ" или "miss"
      (print "damage done: " damage ", now life is: " life)
      (mail creature (tuple 'set 'life life))
      (cond
         ((eq? damage 0) ; если нету урона, ничего не делаем
            #false)
         ((> life 0) ; пока живые, рисуем "хит" (или можем ничего не рисовать)
            (creature:play-animation creature 'swing #f))
         ((<= life 0)
            (creature:play-animation creature 'die 'die) ; умираем, переходим в состояние "умерли"
            'dead))))

; машина состояний
(define default-mob-state-machine (list->ff `(
   ; состояние "сплю"
   (sleeping . ,(list->ff `(
      ; обычный регулярный мировой тик (а нужен ли?)
      (tick . ,do-nothing)

      ; до npc долетел какой-то звук (todo: добавить направление, возможно)
      (sound . ,(lambda (creature sound-level) ; сила звука, в дБ
            ; так как спим, то реагируем только на действительно сильные звуки
            (if (> sound-level 60)
               (begin
                  ; проиграть анимацию "встаю" (эта функция выйдет после того, как анимация закончится)
                  (creature:play-animation creature 'swing #f)
                  ; перейти в режим преследования, иначе спим дальше
                  'pursuit))))

      ; нам нанесли урон
      (damage . ,got-damage))))

   ; режим преследования
   (pursuit . ,(list->ff `(
      (tick . ,do-nothing)
      ;; (tick . ,(lambda (creature)
      ;;    ; пускай наш дорогой скелет поищет путь к сундуку и попытается его ударить
      ;;    (define location (interact creature (tuple 'get-location))) ; текущее положение npc
      ;;    (define chest (interact 'chest (tuple 'get-location))) ; положение сундука (в будущем - героя)
      ;;    ; moveq - возможное направление к сундуку, (tuple x y служебная-информация)
      ;;    (define moveq
      ;;       (A* collision-data
      ;;          (car location) (cdr location)
      ;;          (car chest) (cdr chest)))
      ;;    (define move (cons (ref moveq 1) (ref moveq 2)))

      ;;    ; move relative:
      ;;    ; todo: set as internal function(event or command)
      ;;    (let*((itself (put itself 'location (cons (+ (car location) (car move)) (+ (cdr location) (cdr move)))))
      ;;          (orientation (cond
      ;;             ((equal? move '(-1 . 0)) 6)
      ;;             ((equal? move '(0 . -1)) 0)
      ;;             ((equal? move '(+1 . 0)) 2)
      ;;             ((equal? move '(0 . +1)) 4)
      ;;             (else (get itself 'orientation 0))))
      ;;          (itself (put itself 'orientation orientation)))
      ;;       ; todo: проверить стоит ли продолжать преследование - если мало здоровья, то надо убегать.
      ;;    (values itself 'pursuit))))

      ; до нпс долетел звук (пофиг на звуки)
      (sound . ,do-nothing)
      ; нпс нанесен урон
      (damage . ,got-damage))))

   ; если умер, то умер :)
   (dead . ,(list->ff `(
      (tick . ,do-nothing)
      (sound . ,do-nothing)
      (damage . ,do-nothing)))))))

(define (invoke-state-action action . args)
   #true
)
