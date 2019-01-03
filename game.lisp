#!/usr/bin/ol

; основнi показники персонажа
(define стать '(0))
(define раса '(0))
(define клас '(0))

(define сила '(0))

,load "характеристики.lisp"

(define (перерахувати-характеристики)
   (set-car! сила
      (get-сила (car раса) (car клас))))

(перерахувати-характеристики)

; зададим конфигурацию графического окна
(define-library (lib gl config)
(export config) (import (otus lisp))
(begin
   (define config (list->ff `(
      ; размеры окна в знакоместах
      ; напомню, что мы используем фиксированный шрифт размера 9*16
      (width . ,(* 80 9))
      (height . ,(* 16 25)))))))

(import (lib gl2))
(import (lib gl console))

(define запрошення (create-window 2 2 76 5))
(set-window-writer запрошення (lambda (write)
   (write CYAN 'hello "Створіть героя за допомогою наступних меню:\n\n" #f)
   (write WHITE "Використайте " BLUE "курсорні клавіші")
))

(define підказка (create-window 2 9 76 1))
(define номер-підказки '(0))
(set-window-writer підказка (lambda (write)
   (write YELLOW)
      (if (= (car номер-підказки) 0)
         (write "Ваша стать не має помітних ефектів у грі."))
      (if (= (car номер-підказки) 1)
         (write "Ваша раса визначає різноманітні можливості та бонуси."))
      (if (= (car номер-підказки) 2)
         (write "Ваш клас визначає різноманітні можливості та бонуси.\n"
             "Затемнені пункти рекомендуються тільки досвідченим гравцям"))
))

(define вибір-статі (create-window 2 12 11 2))
(set-window-writer вибір-статі (lambda (write)
   (if (= (car стать) 0)
      (write CYAN)
      (write WHITE))
   (write 'жіноча "а) жіноча\n" #f)
   (if (= (car стать) 1)
      (write CYAN)
      (write WHITE))
   (write 'чоловіча "б) чоловіча" #f)
))

(define вибір-раси (create-window 16 12 15 11))
(set-window-writer вибір-раси (lambda (write)
   (define (C n)
      (if (= (car раса) n)
         (write CYAN)
         (write WHITE)))
   (C 0) (write 'людина "а) Людина\n" #f)
   (C 1) (write 'напівельф "b) Напівельф\n" #f)
   (C 2) (write 'ельф "c) Ельф\n" #f)
   (C 3) (write 'хоббіт "d) Хоббіт\n" #f)
   (C 4) (write 'карлик "e) Карлик\n" #f)
   (C 5) (write 'гном "f) Гном\n" #f)
   (C 6) (write 'напіворк "g) Напіворк\n" #f)
   (C 7) (write 'напівтроль"h) Напівтроль\n" #f)
   (C 8) (write 'дунадан "i) Дунадан\n" #f)
   (C 9) (write 'високий-ельф "j) Високий Ельф\n" #f)
   (C 10) (write 'кобольд "k) Кобольд" #f)
))

(define вибір-класу (create-window 33 12 12 6))
(set-window-writer вибір-класу (lambda (write)
   (define (C n)
      (if (= (car клас) n)
         (write CYAN)
         (write WHITE)))
   (C 0) (write 'воїн "а) Воїн\n" #f)
   (C 1) (write 'маг "b) Маг\n" #f)
   (C 2) (write 'священник "c) Священник\n" #f)
   (C 3) (write 'крадій "d) Крадій\n" #f)
   (C 4) (write 'слідопит "e) Слідопит\n" #f)
   (C 5) (write 'паладін "f) Паладін" #f)
))


;(set-window-background вибір-раси RED)
(gl:set-window-title "Чарівники і Дракони: створення персонажа")
(gl:set-renderer (lambda (mouse)
   (glClearColor 0.2 0.2 0.2 1)
   (glClear GL_COLOR_BUFFER_BIT)

   (render-windows)
))

(gl:set-keyboard-handler (lambda (key)
   (case key
      (vkQ (halt vkQ))
      (vkEnter
         (print "зберігаю налаштування в файл.")
         (fasl-save (list (car стать) (car раса) (car клас) (car сила)) "hero.fasl")
         (halt vkEnter))
   )))

(gl:set-mouse-handler (lambda (button x y)
(print button)
   ;; (if (= (car стать) 0)
   ;;    (set-car! стать 1)
   ;;    (set-car! стать 0))
   
   (case (windows-make-selection x y)
      ('жіноча
         (set-car! номер-підказки 0)
         (set-car! стать 0))
      ('чоловіча
         (set-car! номер-підказки 0)
         (set-car! стать 1))
      ('людина
         (set-car! номер-підказки 1)
         (set-car! раса 0))
      ('напівельф
         (set-car! номер-підказки 1)
         (set-car! раса 1))
      ('ельф
         (set-car! номер-підказки 1)
         (set-car! раса 2))
      ('хоббіт
         (set-car! номер-підказки 1)
         (set-car! раса 3))
      ('карлик
         (set-car! номер-підказки 1)
         (set-car! раса 4))
      ('гном
         (set-car! номер-підказки 1)
         (set-car! раса 5))
      ('напіворк
         (set-car! номер-підказки 1)
         (set-car! раса 6))
      ('напівтроль
         (set-car! номер-підказки 1)
         (set-car! раса 7))
      ('дунадан
         (set-car! номер-підказки 1)
         (set-car! раса 8))
      ('високий-ельф
         (set-car! номер-підказки 1)
         (set-car! раса 9))
      ('кобольд
         (set-car! номер-підказки 1)
         (set-car! раса 10))
      ('воїн
         (set-car! номер-підказки 2)
         (set-car! клас 0))
      ('маг
         (set-car! номер-підказки 2)
         (set-car! клас 1))
      ('священник
         (set-car! номер-підказки 2)
         (set-car! клас 2))
      ('крадій
         (set-car! номер-підказки 2)
         (set-car! клас 3))
      ('слідопит
         (set-car! номер-підказки 2)
         (set-car! клас 4))
      ('паладін
         (set-car! номер-підказки 2)
         (set-car! клас 5))
   )
   (перерахувати-характеристики)
))


;при виборі класу, наприклад, воїн, буде сила 4. при магові -4.

(define показник (create-window 48 12 6 1))
(set-window-writer показник (lambda (write)
   (define (p x)
      (cond
         ((> x 0)
            (write "+"))
         ((= x 0)
            (write " ")))
      (write x))

   (write YELLOW)
   (write "сила ") (p (car сила))
))

