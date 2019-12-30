(import
   (otus lisp)
   (file xml))
(import (scheme misc))
(import (only (lang intern) string->symbol))

; public interface:

; загрузить уровень
;  filename: имя xml файла в формате TILED
(define (level:load filename)
   (interact 'level ['load filename]))

; получить характеристику уровня
; список:
;  'tileheight: высота тайла в пикселях
;  'tilewidth: ширина тайла в пикселях
(define (level:get property)
   (interact 'level ['get property]))

; получить слой уровня по имени
(define (level:get-layer name)
   (getf (interact 'level ['get 'layers]) name))

; получить первый номер тайла, ассоциированный с тайлсетом name
(define (level:get-gid name)
   (getf (interact 'level ['get 'gids]) name))

; возвращает количество тайлов в одной строке тайлсета name
(define (level:get-columns name)
   (getf (interact 'level ['get 'columns]) name))

; нарисовать уровень
;  вызывать только изнутри цикла рендеринга
;  mouse - тайл под курсором
;  creatures - кого рисуем
(define (level:draw creatures)
   (interact 'level ['draw creatures]))


; -------------------------------
; что касается "занятости" мира расчетами
(setq *calculating* (box #f)) ; внутренняя переменная
(define (set-world-busy busy)
   (set-car! *calculating* busy))
(define (world-busy?)
   (car *calculating*))

; -----------------------------------------------
; helper functions
(setq split-by-comma (string->regex "c/,/"))
(setq split-by-newline (string->regex "c/\n/"))

; гравная сопрограмму управления игровым уровнем
(fork-server 'level (lambda ()
   (let this ((itself #empty))
      (let*((envelope (wait-mail))
            (sender msg envelope))
         (case msg
            ; low level interaction interface
            (['set key value]
               (let ((itself (put itself key value)))
                  (this itself)))
            (['get key]
               (mail sender (get itself key #false))
               (this itself))
            (['debug]
               (mail sender itself)
               (this itself))

            ; загрузить новую карту
            (['load filename]
               (for-each display (list "Loading new level '" filename "'... "))
               (define xml (xml-parse-file filename))
               (define level (car (xml-get-value xml))) ; use <map>
               (print "ok.")

               ; compiling tilesets:

               (define tilewidth (string->number (xml-get-attribute level 'tilewidth #f) 10)) ; ширина тайла
               (define tileheight (string->number (xml-get-attribute level 'tileheight #f) 10)) ; высота тайла
               (define width (string->number (xml-get-attribute level 'width #f) 10))  ; количество тайлов по горизонтали
               (define height (string->number (xml-get-attribute level 'height #f) 10)); количество тайлов по вертикали

               (define WIDTH (string->number (xml-get-attribute level 'width #f) 10))
               (define HEIGHT (string->number (xml-get-attribute level 'height #f) 10))

               (define tilesets (xml-get-subtags level 'tileset))

               (define gids (pairs->ff (map (lambda (tileset)
                     (cons
                        (string->symbol (xml-get-attribute tileset 'name "noname"))
                        (string->number (xml-get-attribute tileset 'firstgid "0") 10)))
                  tilesets)))
               (define columns (pairs->ff (map (lambda (tileset)
                     (cons
                        (string->symbol (xml-get-attribute tileset 'name "noname"))
                        (/;(string->number (xml-get-attribute tileset 'columns "0") 10)) <- old code
                           (string->number (xml-get-attribute (xml-get-subtag tileset 'image) 'width "64") 10)
                           (string->number (xml-get-attribute tileset 'tilewidth "64") 10))))
                  tilesets)))

               (print "tilesets: " tilesets)
               (print)

               ; make ff (id > tileset element)
               (define tileset
               (fold (lambda (a b)
                           (ff-union a b #f))
                  #empty
                  (map
                     (lambda (tileset)
                        (let ((image (xml-get-subtag tileset 'image))
                              (tileoffset (xml-get-subtag tileset 'tileoffset)))
                           (define id ; OpenGL texture atlas id
                              (SOIL_load_OGL_texture (c-string (xml-get-attribute image 'source "checker.png")) SOIL_LOAD_RGBA SOIL_CREATE_NEW_ID 0))
                           (define image-width (string->number (xml-get-attribute image 'width "64") 10))
                           (define image-height (string->number (xml-get-attribute image 'height "32") 10))

                           (define first-gid (string->number (xml-get-attribute tileset 'firstgid "0") 10))
                           (define tile-width (string->number (xml-get-attribute tileset 'tilewidth "64") 10))
                           (define tile-height (string->number (xml-get-attribute tileset 'tileheight "32") 10))
                           (define tile-count (string->number (xml-get-attribute tileset 'tilecount "0") 10))
                           (define columns (/ image-width tile-width)) ;(string->number (xml-get-attribute tileset 'columns 0) 10)) <- old
                           (define tile-offsets (if tileoffset
                                                   (cons
                                                      (string->number (xml-get-attribute tileoffset 'x "0") 10)
                                                      (string->number (xml-get-attribute tileoffset 'y "0") 10))
                                                   '(0 . 0)))

                           (define tiles (fold append #null
                              (map (lambda (row)
                                    (map (lambda (col)
                                          (let ((ul_x (* col tile-width))
                                                (ul_y (* row tile-height)))
                                             [
                                                id ; texture id
                                                tile-width
                                                tile-height
                                                tile-offsets
                                                ; texcoords:
                                                [
                                                   (/ ul_x image-width)
                                                   (/ ul_y image-height)
                                                   (/ (+ ul_x tile-width) image-width)
                                                   (/ (+ ul_y tile-height) image-height)]]))
                                       (iota columns)))
                                 (iota (/ tile-count columns)))))
                           (pairs->ff (map cons
                                 (iota (length tiles) first-gid)
                                 tiles))))
                     tilesets)))

               ; prepare layers
               (define layers (fold (lambda (ff layer)
                                       (define name (xml-get-attribute layer 'name #f))
                                       (define data (xml-get-value (xml-get-subtag layer 'data)))
                                       (put ff (string->symbol name)
                                          (map (lambda (line)
                                                  (map (lambda (ch) (string->number ch 10))
                                                     (split-by-comma line)))
                                             (split-by-newline data))))
                                 #empty
                                 (xml-get-subtags level 'layer)))

               ;; (define npcs (fold (lambda (ff object)
               ;;                      (define id (string->number (xml-get-attribute object 'id #f) 10))
               ;;                      (define name (xml-get-attribute object 'name #f))
               ;;                      (define type (xml-get-attribute object 'type #f))
               ;;                      (define x (div (floor (string->number (xml-get-attribute object 'x #f) 10)) 32))
               ;;                      (define y (div (floor (string->number (xml-get-attribute object 'y #f) 10)) 32))
               ;;                      ;; (print "name: " name ", type: " type ", x/y: " x "/" y)

               ;;                      (put ff id [name type x y]))
               ;;                   #empty
               ;;                   (xml-get-subtags (xml-get-subtag level 'objectgroup) 'object)))

               ; ok
               (mail sender 'ok)
               ; парсинг и предвычисления закончены, запишем нужные параметры
               (this (fold (lambda (ff kv) (put ff (car kv) (cdr kv))) itself `(
                  (width . ,width) (height . ,height)
                  (tilewidth . ,tilewidth)
                  (tileheight . ,tileheight)
                  (gids . ,gids)
                  (columns . ,columns)
                  (tileset . ,tileset)
                  ;(npcs . ,npcs)
                  (layers . ,layers)))))

            ; draw the level on the screen
            (['draw creatures]; interact
               (let ((w (getf itself 'tilewidth))
                     (h (getf itself 'tileheight))
                     (width (getf itself 'width))
                     (height (getf itself 'height))
                     (tileset (getf itself 'tileset)))

                  (define (X x y tw th)
                     x)
                     ;; (- (* x (/ w 2))
                     ;;    (* y (/ w 2))
                     ;;    (/ w 2)))

                  (define (Y x y tw th)
                     y)
                     ;; (+ (* x (/ h 2))
                     ;;    (* y (/ h 2))
                     ;;    (- h th)))

                  ; эта функция рисует тайл на экране
                  ; примитивно, не оптимизировано - но ранняя оптимизация нам и не нужна
                  (define (draw-tile id i j) ; i means x, j means y
                     (let ((tile (getf tileset id)))
                        (if tile
                           (let*((x (X i j (ref tile 2) (ref tile 3)))
                                 (y (Y i j (ref tile 2) (ref tile 3)))
                                 (x (+ x (car (ref tile 4))))
                                 (y (+ y (cdr (ref tile 4))))
                                 (st (ref tile 5)))
                              (glBindTexture GL_TEXTURE_2D (ref tile 1))
                              (glBegin GL_QUADS)
                                 (glTexCoord2f (ref st 1) (ref st 2))
                                 (glVertex2f x y)

                                 (glTexCoord2f (ref st 3) (ref st 2))
                                 (glVertex2f (+ x 1#|(ref tile 2)|#) y)

                                 (glTexCoord2f (ref st 3) (ref st 4))
                                 (glVertex2f (+ x 1#|(ref tile 2)|#) (+ y 1#|(ref tile 3)|#))

                                 (glTexCoord2f (ref st 1) (ref st 4))
                                 (glVertex2f x (+ y 1#|(ref tile 3)|#))
                              (glEnd)))))

                  (glColor3f 1 1 1)
                  (glEnable GL_TEXTURE_2D)
                  (define (draw-layer data entities)
                     (map (lambda (line j)
                           (map (lambda (tid i)
                                 ; если есть что рисовать - рисуем
                                 (unless (eq? tid 0)
                                    (draw-tile tid i j))
                                 ; и если в клетке есть кого рисовать, то нарисуем
                                 (if entities
                                    (for-each (lambda (entity)
                                                (if (and
                                                      (eq? (car (ref entity 1)) i)
                                                      (eq? (cdr (ref entity 1)) j))
                                                   (let ((tile (ref entity 2)))
                                                      (if (pair? tile)
                                                         (draw-tile
                                                            (car tile)
                                                            (unless (cdr tile) i (+ i (cadr tile)))
                                                            (unless (cdr tile) j (+ j (cddr tile))))
                                                         (draw-tile
                                                            tile i j)))))
                                       entities)))
                              line
                              (iota (length line))))
                        data
                        (iota (length data))))

                  ; 1. нарисуем фон (трава, вода, но не стены или деревья)
                  (define background-data ((itself 'layers) 'background))
                  (draw-layer background-data #false)

                  ; 2. теперь очередь движимых и недвижимых объектов
                  ;   так как движимые объекты должны уметь прятаться за недвижимые, то
                  ;   рисовать мы их будем все вместе - слой "object" и наших creatures
                  (define object-data ((itself 'layers) 'interior))

                  ;; (draw-layer object-data (append creatures (list
                  ;;    [ (interact 'hero ['get-location])
                  ;;      (interact 'hero ['get-animation-frame])]
                  ;; )))
                  (draw-layer object-data #null #|creatures|#)

                  ; и всякие объекты на столаъ, стенах и т.д.
                  ;(draw-layer ((itself 'layers) 'background) #false)


                  (mail sender 'ok)
                  (this itself)))

            ;
            (else
               (print-to stderr "Unknown world command: " msg)
               (this itself)))))))

