(declare (usual-integrations))

; utility stuff

(define pi 3.14159265)
(define (radians x) (* pi (/ x 180)))
(define (isclose x y tol) (< (abs (- x y)) tol))
(define (pixel x y rgb)
  (map (lambda (c) (write-char (integer->char (floor->exact (* c 255)))))
	   rgb))

(define vec list)
(define mat list)
(define rgb list)

(define (cadr x) (car (cdr x)))
(define (caddr x) (car (cdr (cdr x))))
(define (cadddr x) (car (cdr (cdr (cdr x)))))
(define (caddddr x) (car (cdr (cdr (cdr (cdr x))))))

(define (x v) (car v))
(define (y v) (cadr v))
(define (z v) (caddr v))

(define (theta p) (car p))
(define (phi p) (cadr p))
(define (r p) (caddr p))

; vector/matrix operations

(define (cross u v)
  (vec (- (* (y u) (z v)) (* (z u) (y v)))
	   (- (* (z u) (x v)) (* (x u) (z v)))
	   (- (* (x u) (y v)) (* (y u) (x v)))))

(define (dot u v)
  (+ (* (x u) (x v))
	 (* (y u) (y v))
	 (* (z u) (z v))))

(define (mag v) (sqrt (dot v v)))

(define (normalize v) (map (lambda (c) (/ c (mag v))) v))

(define (neg v) (map - v))

(define (addv u v)
  (vec (+ (x u) (x v))
	   (+ (y u) (y v))
	   (+ (z u) (z v))))

(define (subv u v) (addv u (neg v)))

(define (scale v k) (map (lambda (c) (* k c)) v))

(define (apply-transform t v)
  (addv (scale (x t) (x v))
		(addv (scale (y t) (y v))
			  (scale (z t) (z v)))))

(define (look-at eye center up)
  (define look (normalize (subv center eye)))
  (define right (normalize (cross look up)))
  (define transform (mat right (normalize (cross right look)) look))
  (lambda (v) (addv eye (apply-transform transform v))))

; mandelbulb

(define (to-polar p)
  (vec (acos (/ (z p) (mag p)))
	   (atan (y p) (x p))
	   (mag p)))

(define (to-cartesian p)
  (scale (vec (* (sin (theta p)) (cos (phi p)))
			  (* (sin (phi p)) (sin (theta p)))
			  (cos (theta p)))
		 (r p)))

(define (expt-polar p n)
  (vec (* (theta p) n)
	   (* (phi p) n)
	   (pow (r p) n)))

(define (mandelbulb power iterations bailout)
  (define (iter p p0 dr i)
	(if (and (< i iterations) (< (mag p) bailout))
	  (iter (addv p0 (to-cartesian (expt-polar (to-polar p) power)))
			p0
			(+ 1 (* power dr (pow (mag p) (- power 1))))
			(+ i 1))
	  (* 0.25 (log (mag p)) (/ (mag p) dr))))
  (lambda (p) (iter p p 1 0)))

; utility functions

(define (clamp x lo hi)
  (max lo (min x hi)))

(define (mix a b k)
  (addv a (scale (subv b a) k)))

; raymarching

(define max-steps 256.0)
(define max-dist 64)
(define epsilon 5e-4)

(define (raymarch start dir surface)
  (define (raymarch-iter pos total-dist steps)
	(define surface-dist (surface pos))
	(cond
	  ((isclose surface-dist 0 epsilon)
	   (let ((p (addv start (scale dir total-dist))))
		 (list #t total-dist steps p (normal surface p))))
	  ((or (> surface-dist max-dist) (>= steps max-steps))
	   (list #f total-dist steps))
	  (else (raymarch-iter
			  (addv pos (scale dir (+ epsilon surface-dist)))
			  (+ total-dist surface-dist)
			  (+ steps 1)))))
  (raymarch-iter start 0 0))

(define (normal surface p)
  (define val (surface p))
  (normalize (map (lambda (v)
					(/ (- (surface (addv p (scale v epsilon))) val) epsilon))
				  (list '(1 0 0) '(0 1 0) '(0 0 1)))

(define (hit? hit) (car hit))
(define (dist hit) (cadr hit))
(define (steps hit) (caddr hit))
(define (pos hit) (cadddr hit))
(define (norm hit) (caddddr hit))

; stream stuff

(define (map-stream f s)
  (cons-stream (f (car s)) (map-stream f (cdr-stream s))))

(define (consume s n)
  (if (> n 1)
	(consume (cdr-stream s) (- n 1))))

(define (range-stream start end)
  (if (= start end)
	nil
	(cons-stream start (range-stream (+ start 1) end))))

(define (pixel-stream width height)
  (map-stream
	(lambda (i) (vec (modulo i width) (floordiv i height)))
	(range-stream 0 (* width height))))

; rendering

(define width 256)
(define height 256)
(define aspect-ratio (/ width height))
(define num-pixels (* width height))
(define fov (radians 90))

(define camera-pos '(0 -1 1.5))
(define scene-center '(0 0 0))
(define camera (look-at camera-pos scene-center '(0 1 0)))

(define surface (mandelbulb 8 32 4))

(define (ray-dir p)
  (define w (tan (/ fov 2)))
  (define h (tan (/ fov aspect-ratio 2)))
  (normalize (subv (camera (list (* w (* 2 (- (/ (x p) width) 0.5)))
								 (* h (* 2 (- 0.5 (/ (y p) height))))
								 1))
				   camera-pos)))

(define (pixel-color p light)
  (define hit (raymarch camera-pos (ray-dir p) surface))
  (define ambient
	(if (hit? hit)
	  (rgb 1 0.8 0.27)
	  (rgb 0.78 0.92 1)))
  (define diffuse 
	(if (hit? hit)
	  (clamp (dot (norm hit) light) 0 1)
	  1))
  (define ao
	(if (hit? hit)
	  (- 1 (/ (steps hit) max-steps))
	  1))
  (define shadow
	(if (hit? hit)
	  (- 1 (/ (steps (raymarch (pos hit) (neg light) surface)) max-steps))
	  1))
  (scale ambient (* diffuse ao shadow)))

(define (render surface camera pixel-color resolution fov)


(define (write-ppm-header width height)
  (display "P6")
  (newline)
  (display width)
  (display " ")
  (display height)
  (newline)
  (display 255)
  (newline))

(define (draw)
; (write-ppm-header width height)
  (consume (map-stream
			 (lambda (p) (pixel (x p) (y p) (pixel-color p '(0 -1 1))))
			 (pixel-stream width height))
		   num-pixels))

; Please leave this last line alone.  You may addv addvitional procedures above
; this line.
(draw)
;(with-output-to-file "render.ppm" draw)

