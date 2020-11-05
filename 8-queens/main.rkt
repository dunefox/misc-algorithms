; #lang sicp

(define (range from to)
  (if (= from to)
      (list)
      (cons from (range (+ 1 from) to))))

(define (one-of seq)
  (require (not (null? seq)))
  (amb (car seq) (one-of (cdr seq))))

(define (require p)
  (if (not p)
      (amb)))

(define (create-board x-dim y-dim zero)
  (define (make-list i len zero)
    (if (= i len)
        (list)
        (cons zero (make-list (+ 1 i) len zero))))
  
  (let ((board (list)))
    (do ((y 0 (+ 1 y)))
      ((= y y-dim) board)
      (set! board (cons (make-list 0 x-dim zero) board)))
    board))

(define (set-index! board x y value)
  (define (go x-ind seq value)
    (cond ((= (car seq) 1) (amb))
          ((= x-ind 0)     (set-car! seq value))
          (else            (go (- x-ind 1) (cdr seq) value))))
  
  (go x (list-ref board y) 1))

(define (env-sum x-coord y-coord)
  (define (get-coords x y start)
    (cond ((eq? start 'topleft)
           (cond ((< (+ x y) (- board-size 1)) (list 0 (+ x y)))
                 ((= (+ x y) (- board-size 1)) (list 0 (- board-size 1)))
                 (else (list (- board-size 1) (- x (- (- board-size 1) y)))))
          ((eq? start 'topright)
           (cond ((= x y) (list (- board-size 1) (- board-size 1)))
                 ((> x y) (list (- board-size 1) (+ y (- (- board-size 1) x))))
                 (else (list (+ (- (- board-size 1) y) x) (- board-size 1))))))))
  (define (get-value-xy x y)
    (display  (list-ref board y))
    (newline)
    (display (list-ref (list-ref board y) x))
    (newline) (newline)
    (list-ref (list-ref board y) x))
  (define (walk from direction)
    (if (or (= (car from)  -1)
            (= (cadr from) -1))
        0
        (+ (get-value-xy x-coord y-coord) (env-sum (+ x-coord (car from))
                                                   (+ y-coord (cadr from))))))
  
  (+ (walk (list board-size y-coord) (list -1 0))
     (walk (list x-coord board-size) (list 0 -1))
     (walk (get-coords x-coord y-coord 'topright) (list -1 -1))
     (walk (get-coords x-coord y-coord 'topleft) (list 1 -1))))

(define (solve-board board)
  (let ((x (one-of (range 0 8))))
    (let ((y (one-of (range 0 8))))
      (require (= 0 (env-sum x y)))
      (set-index! board x y 1)))
  board)

(define queen 1)
(define empty 0)
(define board-size 8)
    
(define test-board-4 (create-board 4 4 0))

(define test-board-2 (create-board 2 2 0))

(define test-board-1 (create-board 1 1 0))

(define test-board-8 (create-board 8 8 0))

(define board test-board-8)

;; amb version


