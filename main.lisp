#!/usr/bin/ol

; load the model
(import (lang sexp))
(import (scheme misc))
(import (file xml))
(import (lib rlutil))

(define split-by-comma (string->regex "c/,/"))
(define split-by-newline (string->regex "c/\n/"))

; load level
(define level (xml-parse-file "river_encampment.tmx"))
(define level (car (xml-get-value level))) ; use <map>

(define WIDTH (string->number (xml-get-attribute level 'width "0") 10))
(define HEIGHT (string->number (xml-get-attribute level 'height "0") 10))

,load "creature.lisp"

; loading OpenGL
(import (lib gl))
(import (otus ffi))
(import (OpenGL version-2-1))
(import (otus random!))

(gl:set-window-title "Drawing the tiled map")
(gl:set-window-size 1920 960)
(glViewport 0 0 1920 960)

(import (lib math))
(import (lib soil))

; compiling tilesets:
; (opengl should be enabled for loading images)
(define tilesets (xml-get-subtags level 'tileset))
(print "tilesets count: " (length tilesets))

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
            (print "image-width: " image-width)
            (print "image-height: " image-height)

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

            (print "tile-width: " tile-width)
            (print "tile-height: " tile-height)

            (print "tile-count: " tile-count)
            (print "columns: " columns)
            (print "tile-offsets: " tile-offsets)
            (print "------------------")

         (define tiles (fold append #null
            (map (lambda (row)
                  (print "doing row...")
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


; loading animations
(define skeleton-animation (list->ff (ini-parse-file "skeleton.ini")))
(define antlion-animation (list->ff (ini-parse-file "antlion.ini")))


; find first skeleton id
(define hero_pos (xml-get-subtag level 'properties))
(define hero_pos (car (filter (lambda (tag) (string-eq? (xml-get-attribute tag 'name "") "hero_pos")) (xml-get-value hero_pos))))
(define hero_pos (xml-get-attribute hero_pos 'value #f))
(define hero_pos (split-by-comma hero_pos))
(define hero_pos (cons
   (string->number (car hero_pos) 10)
   (string->number (cadr hero_pos) 10)))

; TEMP
;; ; spawn skeleton
;; (make-creature 'skeleton)
;; (let*((tilesets (filter (lambda (tag) (string-eq? (xml-get-attribute tag 'name "") "skeleton")) tilesets))
;;       (firstgid (string->number (xml-get-attribute (car tilesets) 'firstgid 0) 10)))
;;    (mail 'skeleton (tuple 'set-animations skeleton-animation firstgid)))
;; (mail 'skeleton (tuple 'set-location hero_pos))
;; (mail 'skeleton (tuple 'set-current-animation 'run))

(define skeleton (filter (lambda (tag) (string-eq? (xml-get-attribute tag 'name "") "skeleton")) tilesets))
(define firstgid (string->number (xml-get-attribute (car skeleton) 'firstgid 0) 10))
(define skeletons (map (lambda (id)
      (make-creature id)
      (mail id (tuple 'set-animations skeleton-animation firstgid))
      id)
   (iota 6 1000)))

(make-creature 'antlion)
(let*((tilesets (filter (lambda (tag) (string-eq? (xml-get-attribute tag 'name "") "antlion")) tilesets))
      (firstgid (string->number (xml-get-attribute (car tilesets) 'firstgid 0) 10)))
   (mail 'antlion (tuple 'set-animations antlion-animation firstgid)))
(mail 'antlion (tuple 'set-location hero_pos))
(mail 'antlion (tuple 'set-current-animation 'spawn))



(glShadeModel GL_SMOOTH)
(glClearColor 0.0 0.0 0.0 1)

(define window (vector (+ -32 -800) -32 (+ 3645 32 -800) (+ 2048 32))) ; vector
;(define window #(1000 1000 1890 1500)) ; vector

;; (glMatrixMode GL_PROJECTION)
;; (glLoadIdentity)
;; (gluPerspective 45 (/ 854 480) 0.1 100)

(glEnable GL_TEXTURE_2D)

(glEnable GL_BLEND)
(glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)

;(glEnable GL_DEPTH_TEST)


; prepare layers
(define (get-layer-data level name)
   (define layer (car (filter (lambda (tag) (equal? (xml-get-attribute tag 'name #f) name)) (xml-get-subtags level 'layer))))
   (define data (xml-get-value (xml-get-subtag layer 'data)))

   data)

(define csv (get-layer-data level "background"))
(define background-data (map (lambda (line)
      (map (lambda (ch) (string->number ch 10)) (split-by-comma line))) ;(ref ch 0)
   (split-by-newline csv)))

; objects
(define csv (get-layer-data level "object"))
(define object-data (map (lambda (line)
      (map (lambda (ch) (string->number ch 10)) (split-by-comma line))) ;(ref ch 0)
   (split-by-newline csv)))

; collisions
(define csv (get-layer-data level "collision"))
(define collision-data (map (lambda (line)
      (map (lambda (ch) (string->number ch 10)) (split-by-comma line))) ;(ref ch 0)
   (split-by-newline csv)))
(define (at x y)
   (if (and (< -1 x WIDTH) (< -1 y HEIGHT))
      (lref (lref collision-data y) x)))

(define timestamp (box 0))
(define destination (cons 0 0))
(let loop ()
   (let ((x (rand! 64))
         (y (rand! 64)))
      (if (eq? 0 (at x y))
         (begin
            (set-car! destination x)
            (set-cdr! destination y))
         (loop))))

; let skeletons random on map
(for-each (lambda (id)
      (let loop ()
         (let ((x (rand! 64))
               (y (rand! 64)))
            (if (eq? 0 (at x y))
               (mail id (tuple 'set-location (cons x y)))
               (loop))))
      (mail id (tuple 'set-current-animation 'run)))
   skeletons)


,load "ai.lisp"


; draw
(gl:set-renderer (lambda ()
   ; thinking!
   (let*((ss ms (clock))
         (i (mod (floor (/ (+ (* ss 1000) ms) (/ 1000 8))) 8)))

      (unless (eq? i (unbox timestamp))
         (begin
            (set-car! timestamp i)

            (for-each (lambda (id)
                  (define pos (interact id (tuple 'get-location)))
                  (define _move
                     (A* collision-data
                        (car pos) (cdr pos)
                        (car destination) (cdr destination)))
                  (define move (cons (ref _move 1) (ref _move 2)))
                  (mail id (tuple 'move-relative move)))
               skeletons)

            ; maybe move chest
            (for-each (lambda (id)
                  (define pos (interact id (tuple 'get-location)))

                  (if (equal? pos destination)
                     (let loop ()
                        (let ((x (rand! 64))
                              (y (rand! 64)))
                           (if (eq? 0 (at x y))
                              (begin
                                 (set-car! destination x)
                                 (set-cdr! destination y))
                              (loop))))))
               skeletons)

         )))

   ; drawing
   (glClear GL_COLOR_BUFFER_BIT) ;(vm:or GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))

   (glLoadIdentity)
   (glOrtho (ref window 1) (ref window 3) (ref window 4) (ref window 2) -1 1) ; invert axis Y on screen

   (define w (string->number (xml-get-attribute level 'tilewidth 128) 10)) ; ширина тайла
   (define h (string->number (xml-get-attribute level 'tileheight 64) 10)) ; высота тайла
   (define width (string->number (xml-get-attribute level 'width 64) 10))  ; количество тайлов по горизонтали
   (define height (string->number (xml-get-attribute level 'height 64) 10)); количество тайлов по вертикали

   (define (X x y tw th)
      (+ (- (* x (/ w 2))
            (* y (/ w 2)))
         (- (/ (* width w) 4) (/ w 2))))

   (define (Y x y tw th)
      (+ (+ (* x (/ h 2))
            (* y (/ h 2)))
         (- h th)))


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
                                       (draw-tile (ref entity 2) i j)))
                                 entities)))
               line
               (iota (length line))))
         data
         (iota (length data))))

   (draw-layer background-data #false)

   ; draw object layer with player
   (define creatures (map (lambda (id)
         (tuple (interact id (tuple 'get-location)) (interact id (tuple 'get-animation-frame))))
      skeletons))

   (draw-layer object-data (append creatures (list
      (tuple destination 552)
      (tuple (interact 'antlion (tuple 'get-location)) (interact 'antlion (tuple 'get-animation-frame)))
   )))

   ; keys
   (define (resize scale)
      (let*((x (floor (/ (+ (ref window 3) (ref window 1)) 2)))
            (w (floor (* (- (ref window 3) (ref window 1)) (/ scale 2))))
            (y (floor (/ (+ (ref window 4) (ref window 2)) 2)))
            (h (floor (* (- (ref window 4) (ref window 2)) (/ scale 2)))))
         (set-ref! window 1 (- x w))
         (set-ref! window 2 (- y h))
         (set-ref! window 3 (+ x w))
         (set-ref! window 4 (+ y h))))
   (define (move dx dy)
      (let*((x (floor (* (- (ref window 3) (ref window 1)) 0.01)))
            (y (floor (* (- (ref window 4) (ref window 2)) 0.01))))
         (set-ref! window 1 (+ (ref window 1) (* dx x)))
         (set-ref! window 2 (- (ref window 2) (* dy y)))
         (set-ref! window 3 (+ (ref window 3) (* dx x)))
         (set-ref! window 4 (- (ref window 4) (* dy y)))))

   (if (key-pressed #x3d) (resize 0.9)) ;=
   (if (key-pressed #x2d) (resize 1.1)) ;-
   (if (key-pressed #xff53) (move +1 0)) ; right
   (if (key-pressed #xff51) (move -1 0)); left
   (if (key-pressed #xff52) (move 0 +1)); up
   (if (key-pressed #xff54) (move 0 -1)); down


   (if (key-pressed #x71)
      (shutdown 1))

   ; coordinates
   #|
   (glDisable GL_TEXTURE_2D)
   (glEnable GL_LINE_STIPPLE)
   (glLineStipple 2 #xAAAA)
   (glBegin GL_LINES)
      (glColor3f 1 0 0)
      (glVertex2f -4096 0)
      (glVertex2f +4096 0)
      (glColor3f 1 0 1)
      (glVertex2f -4096 1024)
      (glVertex2f +4096 1024)
      (glColor3f 0 1 0)
      (glVertex2f 0 -4096)
      (glVertex2f 0 +4096)
      (glColor3f 0 1 1)
      (glVertex2f 1024 -4096)
      (glVertex2f 1024 +4096)
   (glEnd)
   (glDisable GL_LINE_STIPPLE)
   ;|#


   #null))
