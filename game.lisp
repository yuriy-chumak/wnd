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

(import (lib gl2))
(import (lib gl console))

(define запрошення (create-window 2 2 76 5))
(set-window-writer запрошення (lambda (write)
   (write CYAN 'hello "Створіть героя за допомогою наступних меню:\n\n" #f)
   (write WHITE "Використайте " BLUE "курсорні клавіші")
))

(define підказка (create-window 2 9 76 1))
(set-window-writer підказка (lambda (write)
   (write YELLOW "Ваша стать не має помітних ефектів у грі.")
))

(define вибір-статі (create-window 2 12 11 2))
(define стать '(0))
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
(define раса '(0))
(set-window-writer вибір-раси (lambda (write)
   (define (C n)
      (if (= (car раса) n)
         (write CYAN)
         (write WHITE)))
   (C 0) (write "а) Людина\n")
   (C 1) (write "b) Напівельф\n")
   (C 2) (write "c) Ельф\n")
   (C 3) (write "d) Хоббіт\n")
   (C 4) (write "e) Карлик\n")
   (C 5) (write "f) Гном\n")
   (C 6) (write "g) Напіворк\n")
   (C 7) (write "h) Напівтроль\n")
   (C 8) (write "i) Дунадан\n")
   (C 9) (write "j) Високий Ельф\n")
   (C 10) (write "k) Кобольд")
))

;(set-window-background вибір-раси RED)

(gl:set-renderer (lambda (mouse)
   (glClearColor 0.2 0.2 0.2 1)
   (glClear GL_COLOR_BUFFER_BIT)

   (render-windows)
))

(gl:set-keyboard-handler (lambda (key)
   (case key
      (vkQ (halt vkQ))
   )))

(gl:set-mouse-handler (lambda (button x y)
   ;; (if (= (car стать) 0)
   ;;    (set-car! стать 1)
   ;;    (set-car! стать 0))
   
   (case (windows-make-selection x y)
      ('жіноча
         (set-car! стать 0))
      ('чоловіча
         (set-car! стать 1))
)))
