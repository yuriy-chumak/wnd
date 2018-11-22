; структура описания стейт-машины создания

; попробуем создать простого моба - скелета, который спит, но как только услышит шаги врага (героя) - просыпается и идет прямо к нему. При подъходе на расстояние удара (в соседнюю клетку) - бьет, пока не убьет. Если жизнь его самого заканчивается - умирает.
; при этом надо, чтобы последовательности анимаций это учитывали. Например, при умирании надо проиграть анимацию умирания и на этом остановиться, не повторять анимацию с начала.

; для тестов сделаем, чтобы скелет автоматически получал тот урон, который наносит.

; список состояний скелета:
; сон, тревога, преследование(агрессия), бегство, смерть

; возьмем упрощенное дерево
;'(sleeping pursuit dead)

; sleeping: сон
; pursuit: преследование, включает передвижение в сторону цели и попытку ее ударить если цель рядом
; dead: лежание мертвым и не реагирование ни на что (разве что пропадание через некоторые время)

; каждое состояние принимает набор раздражителей:
; '(tick) - один игровой тик
; '(sound sound-level) - в зависимости от состояние моб должен "не слышать" звуки ниже некоторого порога (точнее с некоторой вероятностью "не слышать")
; '(damage damage-level) - нанесение урона
;

(define skeleton-state-machine (list->ff `(
   ; состояние "сплю"
   (sleeping . ,(list->ff `(
      (tick . ,(lambda (itself creature)
         (print "'tick")
         (let*((started (time-ms))
               (itself (put itself 'action (lambda (itself) ; когда закончить экшен, (lambda (remaining-time))
                  (print "ACTION: for " (- (time-ms) started))
                  (let ((ms (- (time-ms) started)))
                     (if (> ms 1000)
                        (begin
                           (print "action done.")
                           (put (put itself 'action #f) 'state 'sleeping)) ; сменим состояние, закончим ролл "action"
                        itself)))))) ; todo: сместить текстурку анимации в нужное место (если передвигаемся)
            ; с некоторой вероятностью осмотреться, возможно.
            ; вернуть следующее состояние

            ; temp: для теста и примера: обязательно проснемся, и будем делать это 0.5 секунд
            ;  хотя по хорошему надо делать столько, сколько продлится анимация "просыпаюсь", которой у нас нет )

            ;;    ; ждем 0.5 секунд, снова для теста
            ;;    ; todo: ...
            ;;    ;(mail itself (tuple 'set 'state 'sleeping))
            ;;    (print "nothing...")
            ;;    (print sender)
            ;;    (mail sender #true)
            ;;    (print "xx")
            ;;    ))

            (values itself 'sleeping)))) ; не меняем состояние
      (sound . ,(lambda (itself creature sound-level)
         ; так как спим, то реагируем только на действительно сильные звуки
         (if (> sound-level 10)
            (values itself 'pursuit) ; сразу уйдем в рещжим преследования
            (values itself 'sleeping)))) ; иначе спим дальше
      (damage . ,(lambda (itself creature damage-level)
         (let ((itself (put itself 'health 10)))
            ; уменьшить количество здоровья, и если меньше 0 - умереть
            ; пока будем считать любой удар смертельным )
            (values itself 'dead)))))))
   ; режим преследования
   (pursuit . ,(list->ff `(
      (tick . ,(lambda (itself creature)
         ; пускай наш дорогой скелет поищет путь к сундуку и попытается его достичь
         (define location (getf itself 'location)) ; текущее положение
         (define chest (interact 'chest (tuple 'get-location))) ; положение сундука
         ; moveq - возможное направление к сундуку, (tuple x y служебная-информация)
         (define moveq
            (A* collision-data
               (car location) (cdr location)
               (car chest) (cdr chest)))
         (define move (cons (ref moveq 1) (ref moveq 2)))

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
            ; todo: проверить стоит ли продолжать преследование - если мало здоровья, то надо убегать.
         (values itself 'pursuit))))
      ; до нпс долетел звук
      (sound . ,(lambda (itself creature sound-level)
         ; пофиг на звуки
         (values itself 'pursuit)))
      ; нпс нанесен урон
      (damage . ,(lambda (itself creature damage-level)
         ; уменьшить количество здоровья, и если меньше 0 - умереть
         ; пока будем считать любой удар смертельным )
         (values itself 'dead))))))
   ; если умер, то умер :)
   (dead . ,(list->ff '(
      (tick . ,(lambda ? (value itself 'dead)))
      (sound . ,(lambda ? (value itself 'dead)))
      (damage . ,(lambda ? (value itself 'dead)))))))))


;; (define skeleton-state-machine
;;    ; состояние "сплю"
;;    (define (sleeping itself) (list->ff `(
;;       (tick . ,(lambda ()
;;          ; с некоторой вероятностью осмотреться, возможно.
;;          ; вернуть следующее состояние
;;          (values itself 'sleeping))) ; не меняем состояние
;;       (sound . ,(lambda (sound-level)
;;          ; так как спим, то реагируем только на действительно сильные звуки
;;          (if (> sound-level 10)
;;             (values itself 'pursuit) ; сразу уйдем в рещжим преследования
;;             (values itself 'sleeping)))) ; иначе спим дальше
;;       (damage . ,(lambda (damage-level)
;;          (let ((itself (put itself 'health 10)))
;;             ; уменьшить количество здоровья, и если меньше 0 - умереть
;;             ; пока будем считать любой удар смертельным )
;;             (values itself 'dead)))))))

;;    ; режим преследования
;;    (define (pursuit itself) (list->ff `(
;;       (tick . ,(lambda ()
;;          ;; ; пускай наш дорогой скелет поищет путь к сундуку и попытается его достичь
;;          ;; (define location (getf itself 'location)) ; текущее положение
;;          ;; (define chest (interact 'chest (tuple 'get-location))) ; положение сундука
;;          ;; ; moveq - возможное направление к сундуку, (tuple x y служебная-информация)
;;          ;; (define moveq
;;          ;;    (A* collision-data
;;          ;;       (car location) (cdr location)
;;          ;;       (car chest) (cdr chest)))
;;          ;; (define move (cons (ref moveq 1) (ref moveq 2)))

;;          ;; ; move relative:
;;          ;; ; todo: set as internal function(event or command)
;;          ;; (let*((itself (put itself 'location (cons (+ (car location) (car move)) (+ (cdr location) (cdr move)))))
;;          ;;       (orientation (cond
;;          ;;          ((equal? move '(-1 . 0)) 6)
;;          ;;          ((equal? move '(0 . -1)) 0)
;;          ;;          ((equal? move '(+1 . 0)) 2)
;;          ;;          ((equal? move '(0 . +1)) 4)
;;          ;;          (else (get itself 'orientation 0))))
;;          ;;       (itself (put itself 'orientation orientation)))
;;          ;;    ; todo: проверить стоит ли продолжать преследование - если мало здоровья, то надо убегать.
;;          (values itself 'pursuit)))
;;       ; до нпс долетел звук
;;       (sound . ,(lambda (sound-level)
;;          ; пофиг на звуки
;;          (values itself 'pursuit)))
;;       ; нпс нанесен урон
;;       (damage . ,(lambda (damage-level)
;;          ; уменьшить количество здоровья, и если меньше 0 - умереть
;;          ; пока будем считать любой удар смертельным )
;;          (values itself 'dead))))))

;;    ; если умер, то умер :)
;;    (define (dead itself) (list->ff '(
;;       (dead . ,(lambda ()
;;          ; do nothing.
;;          (values itself 'dead))))))

;;    ; стейт-машина
;;    (list->ff `(
;;       (sleeping . ,sleeping)
;;       (pursuit . ,pursuit)
;;       (dead . ,dead))))


;=============================================================
; набор yielded функций, которые осуществляют действия над npc, с проигрыванием анимации

; todo: оформить эту херню макросами...
;; (define (yielded-function)
;;    (call/cc (lambda (yield)
;;       (print "1")
;;       (call/cc (lambda (continue)
;;          (yield (cons 500 continue))))
;;       (print "2")
;;       (call/cc (lambda (continue)
;;          (yield (cons 500 continue))))
;;       (print "3")
;;       (call/cc (lambda (continue)
;;          (yield (cons 500 continue))))
;;       (print "done."))))
;; (print "-----------------------------")

;; (define (perform-step step)
;;    (let main ((a (step)))
;;       (print "a: " a)
;;       (if (pair? a)
;;          (let ((started (time-ms))
;;                (duration (car a)))
;;             (let loop ((unused #f))
;;                ;(print a ": waiting for " (- (time-ms) started))
;;                (if (< (- (time-ms) started) duration)
;;                   (loop (sleep 10))))
;;             (let ((next (cdr a)))
;;                (print "next: " next)
;;                (main (next #t)))))))

;; (perform-step yielded-function)


; =================================================================
; эта функция проигрывает анимацию creature, ждет пока анимация не закончится
; и восстанавливает начальную анимацию.
(define (play-animation creature animation next-animation) ; return #false
   (let ((started (time-ms))
         (saved-animation (or next-animation (interact creature (tuple 'get 'animation))))
         (duration (interact creature (tuple 'set-current-animation animation))))
      (print "duration: " duration)
      (print "saved-animation: " saved-animation)
      (if duration
         (let*((duration (substring duration 0 (- (string-length duration) 2)))
               (duration (string->number duration 10))); decoded duration in ms
            (let loop ((unused #f))
               ;(print creature ": waiting for " (- (time-ms) started))
               (if (< (- (time-ms) started) duration)
                  (loop (sleep 7))))
            (unless (eq? saved-animation animation)
               ; set next animation or restore saved
               (interact creature (tuple 'set-current-animation saved-animation)))))
      #false))

(define (apply-action creature action)
   #false
)

(define (make-damage me other)
   ; вот тут у нас
   #false
)
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
            (play-animation creature 'hit #f))
         ((<= life 0)
            (play-animation creature 'die 'die) ; умираем, переходим в состояние "умерли"
            'dead))))

; машина состояний
(define skeleton-state-machine (list->ff `(
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
                  (play-animation creature 'swing #f)
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