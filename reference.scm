#!/usr/bin/ol

; зададим конфигурацию графического окна
(define-library (lib gl config)
(export config) (import (otus lisp))
(begin
   (define config (list->ff `(
      ; размеры окна в знакоместах
      ; напомню, что мы используем фиксированный шрифт размера 9*16
      (width . ,(* 80 9))
      (height . ,(* 16 25)))))))

; блок переменных - индикаторов выбора пользователя
(define selection '(0 . 4))

; пара (текущий-выбор . количество-вариантов)
(define sex '(0 . 2)) ; Пол персонажа,
(define race '(0 . 11)) ; Раса и
(define class '(0 . 6)) ; Класс

; Показатели
(define strength '(0)) ; Сила
(define intelligence '(0)) ; Интеллект
(define wisdom '(0)) ; Мудрость
(define dexterity '(0)) ; Ловкость
(define constitution '(0)) ; Телосложение
(define charm '(0)) ; Обаяние

; Навыки



; -------------------------------------------------------------------
; подключение графической библиотеки и библиотеки графической консоли
(import (lib gl2))
(import (lib gl console))
(import (otus random!))

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

; -----------------
; окно приветствия:
(define welcome (create-window 2 2 76 5))
(set-window-writer welcome (lambda (type)
   (type CYAN "Создайте героя с помощью следующих меню:" "\n\n")
   (type WHITE "Используйте " GREEN "курсорные клавиши" WHITE " для прокрутки меню, "
      GREEN "Enter" WHITE " для выбора, '" GREEN "*" WHITE "' для" "\n")
   (type WHITE "случайного выбора, '" GREEN "ESC" "' - начать сначала, '"
          GREEN "=" WHITE "' - опции рождения, '" GREEN "?" WHITE "' -" "\n")
   (type WHITE "справка, '" GREEN "Ctrl-X" WHITE "' - для выхода.")
))

; окно с комментариями:
(define comments (create-window 2 8 76 2))
(set-window-writer comments (lambda (type)
   (type YELLOW
      (case (car selection)
         (0 "Ваш пол не имеет заметных эффектов в игре.")
         (1 "Ваша раса определяет различные возможности и бонусы.")
         (2 "Ваш класс определяет ваши способности и бонусы.\nЗатемненные пункты рекомендуется только опытным игрокам.")
         (3 "Нажмите ENTER для выбора текущего параметров.")))
))

; выбор пола персонажа
(define sex-selection (create-window 2 11 10 2))
(set-window-writer sex-selection (lambda (type)
   (define (color x)
      (if (eq? (car sex) x) CYAN WHITE))
   (type
      (color 0) "a) " 'female "Женский" #f "\n"
      (color 1) "b) " 'male "Мужской" #f)
))

; выбор расы персонажа
(define race-selection (create-window 14 11 32 11))
(set-window-writer race-selection (lambda (type)
   (define (color x)
      (if (eq? (car race) x) CYAN WHITE))
   (define (sign ab)
      (if (< ab 0) "-" "+"))
   (define str (car strength))
   (define int (car intelligence))
   (define wis (car wisdom))
   (define dex (car dexterity))
   (define con (car constitution))
   (define cha (car charm))

   (case (car selection)
      (0 #false) ; если выбирают персонажа, ничего не пишем
      (1 (apply type (list
            (color 0) "a) Человек" #f "     Сил: " (sign str) str "\n"
            (color 1) "b) Полуэльф" #f "    Инт: " (sign int) int "\n"
            (color 2) "c) Эльф" #f "        Муд: " (sign wis) wis "\n"
            (color 3) "d) Хоббит" #f "      Лов: " (sign dex) dex "\n"
            (color 4) "e) Карлик" #f "      Тел: " (sign con) con "\n"
            (color 5) "f) Гном" #f "        Оба: " (sign cha) cha "\n"
            (color 6) "g) Полуорк" #f "     Здоровье   : " 10     "\n"
            (color 7) "h) Полутролль" #f "  Опыт       : " 100 "%""\n"
            (color 8) "i) Дунадан" #f "     Инфразрение: " 0 " фт""\n"
            (color 9) "j) Высший Эльф" #f)))
;           (color 10) "k) Кобольд" #f
      ((2 3 4) (apply type (list
            (color 0) "a) Человек" #f "     \n"
            (color 1) "b) Полуэльф" #f "    \n"
            (color 2) "c) Эльф" #f "        \n"
            (color 3) "d) Хоббит" #f "      \n"
            (color 4) "e) Карлик" #f "      \n"
            (color 5) "f) Гном" #f "        \n"
            (color 6) "g) Полуорк" #f "     \n"
            (color 7) "h) Полутролль" #f "  \n"
            (color 8) "i) Дунадан" #f "     \n"
            (color 9) "j) Высший Эльф" #f)))
   )
))

; рендерер:
(gl:set-renderer (lambda (mouse)
   (glClearColor 0.2 0.2 0.2 1)
   (glClear GL_COLOR_BUFFER_BIT)
   (render-windows)
))

(gl:set-keyboard-handler (lambda (key)
   (define (incr x) (set-car! x (mod (+ (car x) +1) (cdr x))))
   (define (decr x) (set-car! x (mod (+ (car x) -1 (cdr x)) (cdr x))))
   (case key
      (vkQ (halt vkQ))
      (vkEsc (halt 0))

      (vkLeft
         (case (car selection)
            ((1 2 3)
               (decr selection))))
      (vkRight
         (case (car selection)
            ((0 1 2)
               (incr selection))))

      (vkUp
         (case (car selection)
            (0 (decr sex))
            (1 (decr race))
            (else
               (print "unknown selection " selection))))
      (vkDown
         (case (car selection)
            ; первая колонка?
            (0 (incr sex))
            (1 (incr race))
            ; третья колонка?
            ;((equal? selection '(3))
            (else
               (print "unknown selection " selection))))
      (vkEnter
         (case (car selection)
            ((0 1 2)
               (incr selection))
            (3
               (fasl-save (list->ff `(
                  (sex . ,(car sex)) ; Пол персонажа,
                  (race . ,(car race)) ; Раса и
                  (class . ,(car class)) ; Класс

                  ; Показатели
                  (strength . ,(car strength)) ; Сила
                  (intelligence . ,(car intelligence)) ; Интеллект
                  (wisdom . ,(car wisdom)) ; Мудрость
                  (dexterity . ,(car dexterity)) ; Ловкость
                  (constitution . ,(car constitution)) ; Телосложение
                  (charm . ,(car charm)) ; Обаяние
               )) "hero.bin")
               (syscall 59 (c-string "/usr/bin/ol") (map c-string '("/usr/bin/ol" "main.lisp")) #false)
               (halt 0))))
      ;; (vkRight
      ;;    (set-car! selection (+ (car selection) 1)))
      (63 ; *
         (set-car! sex (rand! (cdr sex)))
         (set-car! race (rand! (cdr race)))
         (set-car! class (rand! (cdr class)))
         (set-car! selection 2))



      (else
         (print "unhandled key: " key)))))

(gl:set-mouse-handler (lambda (button x y)
   (case (windows-make-selection x y)
      ('male
         (print "male"))
      ('female
         (print "female"))
   )))

; вот тут ждем пока не будет сделан окончательный выбор
;; (let loop ((* #f))
;;    (unless (eq? (car selection) 4)
;;       (loop (sleep 8))))
(print "ok.")

; тут надо пересоздать окна и обновить обработчик клавиатуры
; TBD.
(print ".")