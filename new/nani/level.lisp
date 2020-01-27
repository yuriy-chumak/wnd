(import
   (otus lisp)
   (file xml))
(import (scheme misc))
(import (file ini))
(import (only (lang intern) string->symbol))

; internal staff
   (define (number->string n)
      (list->string (render-number n null 10)))

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
(define (level:set property value)
   (mail 'level ['set property value]))

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

; -----------------------------------------------
; что касается "занятости" мира расчетами
(setq *calculating* (box #f)) ; внутренняя переменная
(define (set-world-busy busy)
   (set-car! *calculating* busy))
(define (world-busy?)
   (car *calculating*))
; -----------------------------------------------

(fork-server 'levels (lambda ()
   (let this ((itself #empty))
      (let*((envelope (wait-mail))
            (sender msg envelope))
         (case msg
            ; low level interaction interface
            (['set key value]
               (this (put itself key value)))
            (['get key]
               (mail sender (getf itself key))
               (this itself))
            (['debug]
               (mail sender itself)
               (this itself)))))))

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
               (this (put itself key value)))
            (['get key]
               (mail sender (getf itself key))
               (this itself))
            (['debug]
               (mail sender itself)
               (this itself))

            ; загрузить новую карту
            (['load filename]
               (define fn (string->symbol filename))

               (define loadedlevel (interact 'levels ['get fn]))
               (define level
                  (if loadedlevel loadedlevel
                     ; если уровень еще не был прочитан - прочитаем:
                  else (begin
                     (for-each display (list "Loading new level '" filename "'... "))
                     (define xml (xml-parse-file filename))
                     (define level (car (xml-get-value xml))) ; use <map>
                     (print "ok.")

                     ; имя уровня - название файла без расширения
                     (define name ((string->regex "s/([^.]+).*/\\1/") filename))

                     ; xml parser simplification
                     (define (I xml attribute)
                        (string->number (xml:attribute xml attribute #f) 10))
                     (define (S xml attribute)
                        (string->symbol (xml:attribute xml attribute #f)))

                     ; compiling tilesets:
                     (define tilewidth (I level 'tilewidth)) ; ширина тайла
                     (define tileheight (I level 'tileheight)) ; высота тайла
                     (define width (I level 'width))  ; количество тайлов по горизонтали
                     (define height (I level 'height)); количество тайлов по вертикали

                     (define WIDTH (I level 'width))  ; количество тайлов по горизонтали
                     (define HEIGHT (I level 'height)); количество тайлов по вертикали

                     (define tilesets (xml-get-subtags level 'tileset))

                     ; список начальных номеров тайлов
                     (define gids (pairs->ff (map (lambda (tileset)
                           (cons
                              (S tileset 'name)
                              (I tileset 'firstgid)))
                        tilesets)))
                     ; количество колонок в тайлсете (для персонажей. отдельная строка - отдельная ориентация перса)
                     (define columns (pairs->ff (map (lambda (tileset)
                           (cons
                              (string->symbol (xml-get-attribute tileset 'name "noname"))
                              (/;(string->number (xml-get-attribute tileset 'columns "0") 10)) <- old code
                                 (I (xml-get-subtag tileset 'image) 'width)
                                 (I tileset 'tilewidth))))
                        tilesets)))

                     ; тайлы: [gid ширина имя]
                     (define tilenames
                        (map
                           (lambda (tileset)
                              (let ((image (xml-get-subtag tileset 'image))
                                    (tileoffset (xml-get-subtag tileset 'tileoffset)))
                                 (define name (xml-get-attribute image 'source "checker.png"))
                                 (define first-gid (I tileset 'firstgid))
                                 (define columns (/ (I image 'width) (I tileset 'tilewidth)))

                                 [first-gid columns name]))
                           tilesets))


                     ; make ff (id > tileset element)
                     (define tileset
                     (fold (lambda (a b)
                                 (ff-union a b #f))
                        #empty
                        (map
                           (lambda (tileset)
                              (let ((image (xml-get-subtag tileset 'image))
                                    (tileoffset (xml-get-subtag tileset 'tileoffset)))
                                 (define name (xml:attribute image 'source "checker.png"))
                                 (define id ; OpenGL texture atlas id
                                    (SOIL_load_OGL_texture (c-string name) SOIL_LOAD_RGBA SOIL_CREATE_NEW_ID 0))
                                 (define image-width (I image 'width))
                                 (define image-height (I image 'height))

                                 (define first-gid (I tileset 'firstgid))
                                 (define tile-width (I tileset 'tilewidth))
                                 (define tile-height (I tileset 'tileheight))
                                 (define tile-count (I tileset 'tilecount))
                                 (define columns (/ image-width tile-width)) ;(string->number (xml-get-attribute tileset 'columns 0) 10)) <- old

                                 (define tile-offsets (if tileoffset
                                                         (cons
                                                            (I tileoffset 'x)
                                                            (I tileoffset 'y))
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

                     ; слои
                     (define layers (fold (lambda (ff layer)
                                             (define name (xml:attribute layer 'name #f))
                                             (define data (xml:value (xml-get-subtag layer 'data)))
                                             (put ff (string->symbol name)
                                                (map (lambda (line)
                                                      (map (lambda (ch) (string->number ch 10))
                                                         (split-by-comma line)))
                                                   (split-by-newline data))))
                                       #empty
                                       (xml-get-subtags level 'layer)))

                     ; npc
                     (define npcs
                        (ff-fold (lambda (& key value)
                              (define coroutine (string->symbol (fold string-append
                                 name (list "/" ((if (symbol? key) symbol->string number->string) key)))))
                              (define npc (make-creature coroutine value))

                              ((npc 'set-location) (cons
                                 (value 'x)
                                 (value 'y)))
                              ; тут надо найти какому тайлсету принадлежит этот моб
                              (define gid (value 'gid))
                              (call/cc (lambda (done)
                                 (let loop ((old tilenames) (tiles tilenames))
                                    (if (or
                                          (null? tiles)
                                          (< gid (ref (car tiles) 1)))
                                       (begin
                                          (define r (string->regex "s/^.+\\/(.+)\\..+/\\1/"))
                                          (define name (string->symbol (r (ref old 3))))

                                          
                                          ((npc 'set-animation-profile)
                                             name
                                             (fold string-append "" (list "animations/" (symbol->string name) ".ini"))
                                             (gids name)
                                             (columns name))
                                          
                                          (define orientation (div (- gid (ref old 1)) (ref old 2)))
                                          ((npc 'set-orientation) orientation)

                                          (done #t))
                                       (loop (car tiles) (cdr tiles))))))
                              ((npc 'set-current-animation) 'default)
                              (put & key npc))
                           {}
                           (fold (lambda (ff objectgroup)
                                    (fold (lambda (ff object)
                                             (if (string-eq? (xml:attribute object 'type "") "npc") (begin
                                                ; npc id is a name as symbol or id as integer
                                                (define id (or
                                                   (string->symbol (xml:attribute object 'name #false))
                                                   (string->number (xml:attribute object 'id 999) 10)))
                                                (print "npc id: " id)

                                                (put ff id {
                                                   'id  id
                                                   'name (xml:attribute object 'name #f)
                                                   'gid (I object 'gid)
                                                   'x (/ (I object 'x) tilewidth)
                                                   'y (/ (I object 'y) tileheight) }))
                                             else ff))
                                       ff
                                       (xml-get-subtags objectgroup 'object)))
                              {}
                              (filter
                                 (lambda (tag)
                                    (string-eq? (xml:attribute tag 'name "") "objects"))
                                 (xml-get-subtags level 'objectgroup)))))

                     ; порталы
                     (define portals
                        (fold (lambda (ff objectgroup)
                                 (fold (lambda (ff object)
                                          (if (string-eq? (xml:attribute object 'type "") "portal") (begin
                                             (define id (I object 'id))

                                             (define name (xml:attribute object 'name ""))
                                             (define target ((string->regex "c/\\//") name))

                                             (put ff id {
                                                'id id
                                                'target (cons
                                                   (string->symbol (lref target 0))
                                                   (string->symbol (lref target 1)))
                                                'x (/ (I object 'x) tilewidth)
                                                'y (/ (I object 'y) tileheight)
                                                'width  (/ (I object 'width) tilewidth)
                                                'height (/ (I object 'height) tileheight) }))
                                          else ff))
                                    ff
                                    (xml-get-subtags objectgroup 'object)))
                           {}
                           (filter
                              (lambda (tag)
                                 (string-eq? (xml-get-attribute tag 'name "") "objects"))
                              (xml-get-subtags level 'objectgroup))))

                     ; точки куда ведут порталы
                     (define spawns
                        (fold (lambda (ff objectgroup)
                                 (fold (lambda (ff object)
                                          (if (string-eq? (xml:attribute object 'type "") "spawn") (begin
                                             (define id (or
                                                (string->symbol (xml:attribute object 'name #false))
                                                (string->number (xml:attribute object 'id 999) 10)))
                                             (print "spawn id: " id)
                                             
                                             (put ff id {
                                                'id id
                                                'x (/ (I object 'x) tilewidth)
                                                'y (/ (I object 'y) tileheight) }))
                                          else ff))
                                    ff
                                    (xml-get-subtags objectgroup 'object)))
                           {}
                           (filter
                              (lambda (tag)
                                 (string-eq? (xml:attribute tag 'name "") "objects"))
                              (xml-get-subtags level 'objectgroup))))

                     ; парсинг и предвычисления закончены, создадим уровень
                     (print "+++++++")
                     (define newlevel
                        (fold (lambda (ff kv) (put ff (car kv) (cdr kv))) itself `(
                           (width . ,width) (height . ,height)
                           (tilewidth . ,tilewidth)
                           (tileheight . ,tileheight)
                           (gids . ,gids)
                           (columns . ,columns)
                           (tileset . ,tileset)

                           (npcs . ,npcs)
                           (portals . ,portals)
                           (spawns . ,spawns)

                           (tilenames . ,tilenames)
                           (layers . ,layers))))
                     (mail 'levels ['set fn newlevel])
                     newlevel)))
               (mail sender 'ok)
               (this level))
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
                              ; отрисуем движимое
                              (for-each (lambda (entity)
                                          (let ((x (car (ref entity 1)))
                                                (y (cdr (ref entity 1))))
                                          (if (<= j y (+ j 1))
                                             (let ((tile (ref entity 2)))
                                                (if (pair? tile)
                                                   (draw-tile
                                                      (car tile)
                                                      x (- y 1)))))))
                                 entities)
                              ; и недвижимое имущество
                              (map (lambda (tid i)
                                    ; если есть что рисовать - рисуем
                                    (unless (eq? tid 0)
                                       (draw-tile tid i j)))
                                 line
                                 (iota (length line))))
                        data
                        (iota (length data))))

                  (define (draw-layers data1 data2 entities)
                     (map (lambda (line1 line2 j)
                              ; отрисуем движимое
                              (for-each (lambda (entity)
                                          (let ((x (car (ref entity 1)))
                                                (y (cdr (ref entity 1))))
                                          (if (<= j y (+ j 1))
                                             (let ((tile (ref entity 2)))
                                                (if (pair? tile)
                                                   (draw-tile
                                                      (car tile)
                                                      x (- y 1)))))))
                                 entities)
                              ; и недвижимое имущество
                              (map (lambda (tid i)
                                    ; если есть что рисовать - рисуем
                                    (unless (eq? tid 0)
                                       (draw-tile tid i j)))
                                 line1
                                 (iota (length line1)))
                              (map (lambda (tid i)
                                    ; если есть что рисовать - рисуем
                                    (unless (eq? tid 0)
                                       (draw-tile tid i j)))
                                 line2
                                 (iota (length line2))))
                        data1
                        data2
                        (iota (length data1))))

                  ; 1. фон (пол, стены, мусор на полу)
                  (draw-layer ((itself 'layers) 'background)
                     #null)

                  ; 2. теперь очередь движимых и недвижимых объектов
                  ;   так как движимые объекты должны уметь прятаться за недвижимые, то
                  ;   рисовать мы их будем все вместе - слой "object" и наших creatures
                  (define object-data ((itself 'layers) 'interior))

                  (draw-layers
                     ((itself 'layers) 'interior)
                     ((itself 'layers) 'plates)
                     creatures)

                  (mail sender 'ok)
                  (this itself)))

            ;
            (else
               (print-to stderr "Unknown world command: " msg)
               (this itself)))))))

