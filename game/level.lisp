(import
   (otus lisp)
   (file xml))
(import (scheme misc))
(import (only (lang intern) string->symbol))

; public interface:

; загрузить уровень
(define (level:load filename)
   (interact 'level (tuple 'load filename)))

; попросить у уровня "background" слой
;  возвращает список списков с номерами тайлов
(define (level:get-background)
   (interact 'level (tuple 'get 'background-data)))
;  возвращает слой "collision"
(define (level:get-collisions)
   (interact 'level (tuple 'get 'collision-data)))

; возвращает первый номер тайла, ассоциированный с тайлсетом name
(define (level:get-gid name)
   (getf (interact 'level (tuple 'get 'gids)) name))
; возвращает количество тайлов в одной строке тайлсета name
(define (level:get-columns name)
   (getf (interact 'level (tuple 'get 'columns)) name))

; нарисовать уровень
(define (level:draw mouse)
   (interact 'level (tuple 'draw mouse)))


; -----------------------------------------------
; helper functions
(define split-by-comma (string->regex "c/,/"))
(define split-by-newline (string->regex "c/\n/"))

; игровой уровень
(fork-server 'level (lambda ()
   (let this ((itself #empty))
      (let*((envelope (wait-mail))
            (sender msg envelope))
         (tuple-case msg
            ; low level interaction interface
            ((set key value)
               (let ((itself (put itself key value)))
                  (this itself)))
            ((get key)
               (mail sender (get itself key #false))
               (this itself))
            ((debug)
               (mail sender itself)
               (this itself))

            ; загрузить новую карту
            ((load filename)
               (for-each display (list "Loading new level '" filename "'... "))
               (define xml (xml-parse-file (string-append filename ".tmx")))
               (define level (car (xml-get-value xml))) ; use <map>
               (print "ok.")

               ; compiling tilesets:

               (define tilewidth (string->number (xml-get-attribute level 'tilewidth 128) 10)) ; ширина тайла
               (define tileheight (string->number (xml-get-attribute level 'tileheight 64) 10)) ; высота тайла
               (define width (string->number (xml-get-attribute level 'width 64) 10))  ; количество тайлов по горизонтали
               (define height (string->number (xml-get-attribute level 'height 64) 10)); количество тайлов по вертикали

               ; (opengl should be enabled for loading images)
               (define WIDTH (string->number (xml-get-attribute level 'width "0") 10))
               (define HEIGHT (string->number (xml-get-attribute level 'height "0") 10))

               (define tilesets (xml-get-subtags level 'tileset))
               (print-to stderr "tilesets count: " (length tilesets))

               (define gids (list->ff (map (lambda (tileset)
                     (cons
                        (string->symbol (xml-get-attribute tileset 'name "noname"))
                        (string->number (xml-get-attribute tileset 'firstgid 0) 10)))
                  tilesets)))
               (define columns (list->ff (map (lambda (tileset)
                     (cons
                        (string->symbol (xml-get-attribute tileset 'name "noname"))
                        (string->number (xml-get-attribute tileset 'columns 0) 10)))
                  tilesets)))

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
                           (define image-width (string->number (xml-get-attribute image 'width 64) 10))
                           (define image-height (string->number (xml-get-attribute image 'height 32) 10))
                           ;; (print "image-width: " image-width)
                           ;; (print "image-height: " image-height)

                           (define first-gid (string->number (xml-get-attribute tileset 'firstgid 0) 10))
                           (define tile-width (string->number (xml-get-attribute tileset 'tilewidth 64) 10))
                           (define tile-height (string->number (xml-get-attribute tileset 'tileheight 32) 10))
                           (define tile-count (string->number (xml-get-attribute tileset 'tilecount 0) 10))
                           (define columns (/ image-width tile-width)) ;(string->number (xml-get-attribute tileset 'columns 0) 10))
                           (define tile-offsets (if tileoffset
                                                   (cons
                                                      (string->number (xml-get-attribute tileoffset 'x 0) 10)
                                                      (string->number (xml-get-attribute tileoffset 'y 0) 10))
                                                   '(0 . 0)))

                           ;; (print "tile-width: " tile-width)
                           ;; (print "tile-height: " tile-height)

                           ;; (print "tile-count: " tile-count)
                           ;; (print "columns: " columns)
                           ;; (print "tile-offsets: " tile-offsets)
                           ;; (print "------------------")

                        (define tiles (fold append #null
                           (map (lambda (row)
                                 (map (lambda (col)
                                       (let ((ul_x (* col tile-width))
                                             (ul_y (* row tile-height)))
                                          (tuple
                                             id
                                             tile-width
                                             tile-height
                                             tile-offsets
                                             ; texcoords
                                             (vector
                                                (/ ul_x image-width)
                                                (/ ul_y image-height)
                                                (/ (+ ul_x tile-width) image-width)
                                                (/ (+ ul_y tile-height) image-height)))))
                                    (iota columns)))
                              (iota (/ tile-count columns)))))
                        (list->ff (map cons
                              (iota (length tiles) first-gid)
                              tiles))))
                     tilesets)))


               ;; ; prepare layers
               (define (get-layer-data level name)
                  (define layer (car (filter (lambda (tag) (equal? (xml-get-attribute tag 'name #f) name)) (xml-get-subtags level 'layer))))
                  (define data (xml-get-value (xml-get-subtag layer 'data)))

                  data)

               (define background (get-layer-data level "background"))
               (define background-data (map (lambda (line)
                     (map (lambda (ch) (string->number ch 10)) (split-by-comma line))) ;(ref ch 0)
                  (split-by-newline background)))

               ; objects
               (define object (get-layer-data level "object"))
               (define object-data (map (lambda (line)
                     (map (lambda (ch) (string->number ch 10)) (split-by-comma line))) ;(ref ch 0)
                  (split-by-newline object)))

               ; collisions
               ; todo: make collision-data a tuple of tuple!
               (define collision (get-layer-data level "collision"))
               (define collision-data (map (lambda (line)
                     (map (lambda (ch) (string->number ch 10)) (split-by-comma line))) ;(ref ch 0)
                  (split-by-newline collision)))
               (define (at x y)
                  (if (and (< -1 x WIDTH) (< -1 y HEIGHT))
                     (lref (lref collision-data y) x)))


               ; ...
               (print "ok.")

               (mail sender 'ok)
               ; парсинг и предвычисления закончены, запишем нужные параметры
               (this (fold (lambda (ff kv) (put ff (car kv) (cdr kv))) itself `(
                  (width . ,width) (height . ,height)
                  (tilewidth . ,tilewidth)
                  (tileheight . ,tileheight)
                  (gids . ,gids)
                  (columns . ,columns)
                  (tileset . ,tileset)
                  (background-data . ,background-data)
                  (object-data . ,object-data)
                  (collision-data . ,collision-data)))))

            ; draw the level on the screen
            ((draw); interact
               (let ((w (getf itself 'tilewidth))
                     (h (getf itself 'tileheight))
                     (width (getf itself 'width))
                     (height (getf itself 'height))
                     (tileset (getf itself 'tileset))
                     (background-data (getf itself 'background-data))
                     (object-data (getf itself 'object-data)))

                  (define (X x y tw th)
                     (- (* x (/ w 2))
                        (* y (/ w 2))
                        (/ w 2)))

                  (define (Y x y tw th)
                     (+ (* x (/ h 2))
                        (* y (/ h 2))
                        (- h th)))

                  ; эта функция рисует тайл на экране
                  ; примитивно, не оптимизировано - но ранняя оптимизация нам не нужна
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
                                 (glVertex2f (+ x (ref tile 2)) y)

                                 (glTexCoord2f (ref st 3) (ref st 4))
                                 (glVertex2f (+ x (ref tile 2)) (+ y (ref tile 3)))

                                 (glTexCoord2f (ref st 1) (ref st 4))
                                 (glVertex2f x (+ y (ref tile 3)))
                              (glEnd)))))


                  (glColor3f 1 1 1)
                  (glEnable GL_TEXTURE_2D)
                  (define (draw-layer data entities)
                     (map (lambda (line j)
                           (map (lambda (tid i)
                                 (unless (eq? tid 0)
                                    (draw-tile tid i j))
                                 (if entities (for-each (lambda (entity)
                                                   (if (and
                                                         (eq? (car (ref entity 1)) i)
                                                         (eq? (cdr (ref entity 1)) j))
                                                      (let ((tile (ref entity 2)))
                                                      (draw-tile
                                                         (car tile)
                                                         (unless (cdr tile) i (+ i (cadr tile)))
                                                         (unless (cdr tile) j (+ j (cddr tile)))))))
                                                entities)))
                              line
                              (iota (length line))))
                        data
                        (iota (length data))))

                  ; 1. нарисуем фон (трава, вода, но не стены или деревья)
                  (if background-data
                     (draw-layer background-data #false))

                  ; 2. теперь очередь движимых и недвижимых объектов
                  ;   так как движимые объекты должны уметь прятаться за недвижимые, то
                  ;   рисовать мы их будем все вместе - слой "object" и наших creatures

                  ; список NPC:
                  (define creatures (map (lambda (id)
                        (tuple (interact id (tuple 'get-location)) (interact id (tuple 'get-animation-frame))))
                     (interact 'creatures (tuple 'get 'monsters))))

                  (draw-layer object-data (append creatures (list
                     (tuple (interact 'hero (tuple 'get-location))
                            (interact 'hero (tuple 'get-animation-frame)))
                  )))

                  (mail sender 'ok)
                  (this itself)))

            ;
            (else
               (print-to stderr "Unknown world command: " msg)
               (this itself)))))))

