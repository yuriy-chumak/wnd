(import
   (otus lisp)
   (file xml))
(import (scheme misc))
(import (file ini))
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


; =====================================================================
; === CREATURES =======================================================
(define orientations {
   0 0 ; top
   1 1 ; right
   2 2 ; bottom
   3 3 ; left
}) ;left-top



(define-syntax make-setter
   (syntax-rules (name)
      ((make-setter (function this sender . args) . body)
         (list (quote function)
            (lambda (this sender . args)
               . body)
            (lambda all
               (mail name (cons (quote function) all)))
            ))))
(define-syntax make-getter
   (syntax-rules (name)
      ((make-getter (function this sender . args) . body)
         (list (quote function)
            (lambda (this sender . args)
               . body)
            (lambda all
               (interact name (cons (quote function) all)))
            ))))


;; (define-syntax make-creature-macro
;;    (syntax-rules (SET)
;;       ((make-creature-macro (SET (function this sender . args) . body) . rest)
;;          (list (quote function)
;;             (lambda (this sender . args)
;;                . body)
;;             (lambda all
;;                (interact name (list->tuple (cons (quote function) all))))
;;             ))))

; ----------------------------------
; todo: make automatic id generation
; создать новое "создание"
(define (make-creature name initial)
(let*((I (fold (lambda (ff function)
                        (cons
                           (put (car ff) (car function) (cadr function))
                           (put (cdr ff) (car function) (caddr function))))
                  (cons initial #empty)
                  (list
         ; debug staff
         (make-setter (set this sender key value)
            (put this key value))
         (make-getter (get this sender key)
            (mail sender (get this key #f))
            this)
         (make-getter (debug this sender)
            (mail sender this)
            this)


         ; задать новое положение npc
         (make-setter (set-location this sender xy)
            (put this 'location xy))

         (make-setter (set-orientation this sender orientation)
            (put this 'orientation orientation))

         (make-getter (get-location this sender)
            (mail sender (get this 'location '(0 . 0)))
            this)


         ; конфигурирование анимации
         ;  задать имя тайловой карты и конфигурационный файл анимаций персонажа
         (make-setter (set-animation-profile this sender name ini)
            (let*((this (put this 'fg (level:get-gid name)))
                  (this (put this 'animations (pairs->ff (ini-parse-file ini))))
                  (this (put this 'columns (level:get-columns name))))
               this))

         (make-getter (set-current-animation this sender animation)
            ; set current animation (that will be changed, or not to default)
            ; return animation cycle time (for feature use by caller)
            (let*((this (put this 'animation animation))
                  (this (put this 'ssms (time-ms))))
               ; сообщим вызывающему сколько будет длиться полный цикл анимации
               ; (так как у нас пошаговая игра, то вызывающему надо подождать пока проиграется цикл анимации)
               (let*((animation-info (getf (get this 'animations #empty) (getf this 'animation)))
                     (duration (getf animation-info 'duration))
                     (duration (substring duration 0 (- (string-length duration) 2)))
                     (duration (string->number duration 10))
                     (frames (get animation-info 'frames "4"))
                     (frames (string->number frames 10))
                     (animation-type (get animation-info 'type ""))
                     (duration (if (string-eq? animation-type "back_forth")
                                 (floor (* (+ frames frames -1)))
                                 duration)))
                     ; decoded duration in ms
                  (mail sender #|duration|#
                     (if (string-eq? animation-type "back_forth")
                           (* 100 (+ frames frames -1))
                           (* 100 frames))
                  ))
               this))
         ; ...
         (make-getter (get-animation-frame this sender)
            ; todo: change frames count according to animation type (and fix according math)
            (let*((animation (get this 'animation 'stance)) ; соответствующая состояния анимация
                  (ssms (- (time-ms) (get this 'ssms 0))) ; количество ms с момента перехода в анимацию
                  (columns (get this 'columns 32))
                  (delta (getf this 'next-location))
                  (animations (get this 'animations #empty))
                  (animation (get animations animation #empty))
                  (animation-type (get animation 'type #false))
                  (duration (get animation 'duration "250ms"))
                  (duration (substring duration 0 (- (string-length duration) 2)))
                  (duration (string->number duration 10))
                  (frames (get animation 'frames "4"))
                  (frames (string->number frames 10))
                  (duration (if (string-eq? animation-type "back_forth")
                              (floor (* (+ frames frames -1)))
                              duration))

                  (duration
                     (if (string-eq? animation-type "back_forth")
                           (* 100 (+ frames frames -1))
                           (* 100 frames)))


                  (position (get animation 'position "4"))
                  (position (string->number position 10))
                  (orientation (get this 'orientation 0))
                  (frame (floor (/ (* ssms frames) duration)))

                  (delta (if delta (let ((n (if (string-eq? animation-type "back_forth")
                                                (+ frames frames -1)
                                                frames)))
                     (cons (* (min frame n) (/ (car delta) n))
                           (* (min frame n) (/ (cdr delta) n))))))

                  (frame (cond
                     ((string-eq? animation-type "play_once")
                        (min (- frames 1) frame))
                     ((string-eq? animation-type "looped")
                        (mod frame frames))
                     ((string-eq? animation-type "back_forth")
                        (list-ref (append (iota frames) (reverse (iota (- frames 2) 1))) (mod frame (+ frames frames -2)))))))
               (mail sender (cons (+ (get this 'fg 0) position
                  frame
                  (* columns (get orientations orientation 0)))
                  delta)))
            this)
      ))))
   (print "name: " name)

   (fork-server name (lambda ()
      (print "server " name " started.")
   (let this ((itself (car I)))
   (let*((envelope (wait-mail))
         (sender msg envelope))
      (let ((handler (get itself (car msg) #false)))
         (unless handler
            (print "Unhandled message " msg " from " sender))
         (this (if handler
            (apply handler (cons itself (cons sender (cdr msg))))
            itself)))))))

   (define creature (ff-union (cdr I)
      {'name name}
      (lambda (a b) a)))

   ;; ; добавим npc к общему списку npc
   ;; (mail 'creatures ['set name creature])
   creature))
; =====================================================================
; =====================================================================


; -------------------------------
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
               (define level (or
                  (interact 'levels ['get fn]))
                  ; если уровень еще не был прочитан - прочитаем:
                  (begin 
                     (for-each display (list "Loading new level '" filename "'... "))
                     (define xml (xml-parse-file filename))
                     (define level (car (xml-get-value xml))) ; use <map>
                     (print "ok.")

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
                              (define npc (make-creature key #empty))
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
                                          (define name (r (ref old 3)))
                                          
                                          ((npc 'set-animation-profile)
                                             (string->symbol name)
                                             (fold string-append "" (list "animations/" name ".ini")))
                                          
                                          (define orientation (div (- gid (ref old 1)) (ref old 2)))
                                          ((npc 'set-orientation) orientation)

                                          (done #t))
                                       (loop (car tiles) (cdr tiles))))))
                              ((npc 'set-current-animation) 'default)
                              (put & key npc))
                           {}
                           (fold (lambda (ff objectgroup)
                                    (fold (lambda (ff object)
                                             (define id (I object 'id))
                                             (if (string-eq? (xml:attribute object 'type "") "npc")
                                                (put ff id {
                                                   'id  id
                                                   'gid (I object 'gid)
                                                   'x (/ (I object 'x) tilewidth)
                                                   'y (/ (I object 'y) tileheight) })
                                                ff))
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
                                          (define id (I object 'id))
                                          (if (string-eq? (xml:attribute object 'type "") "portal")
                                             (put ff id {
                                                'id id
                                                'name (xml:attribute object 'name #f)
                                                'x (/ (I object 'x) tilewidth)
                                                'y (/ (I object 'y) tileheight)
                                                'width  (/ (I object 'width) tilewidth)
                                                'height (/ (I object 'height) tileheight) })
                                             ff))
                                    ff
                                    (xml-get-subtags objectgroup 'object)))
                           {}
                           (filter
                              (lambda (tag)
                                 (string-eq? (xml-get-attribute tag 'name "") "objects"))
                              (xml-get-subtags level 'objectgroup))))

                     ; порталы
                     (define spawns
                        (fold (lambda (ff objectgroup)
                                 (fold (lambda (ff object)
                                          (define id (I object 'id))
                                          (if (string-eq? (xml:attribute object 'type "") "spawn")
                                             (put ff id {
                                                'id id
                                                'name (xml:attribute object 'name #f)
                                                'x (/ (I object 'x) tilewidth)
                                                'y (/ (I object 'y) tileheight) })
                                             ff))
                                    ff
                                    (xml-get-subtags objectgroup 'object)))
                           {}
                           (filter
                              (lambda (tag)
                                 (string-eq? (xml:attribute tag 'name "") "objects"))
                              (xml-get-subtags level 'objectgroup))))

                     ; парсинг и предвычисления закончены, создадим уровень
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
                     newlevel))
               ; ok
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

