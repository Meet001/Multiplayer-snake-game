#lang racket
(provide new-multiplayer)
(require 2htdp/universe 2htdp/image)
(struct pit (snakep1 snakep2) #:transparent)
(struct snake (dir body) #:transparent)
(struct posn (x y) #:transparent)
(struct vec (x y) #:transparent)

(define TICK-RATE (/ 1 15))

(define SIZE-x 90)
(define SIZE-y 45)

(define SEG-SIZE 15)

(define WIDTH (* SIZE-x SEG-SIZE))
(define HEIGHT (* SIZE-y SEG-SIZE))

(define IMAGE1 (scale (/ 1 15) (bitmap "body1.png")))
(define IMAGE2 (scale (/ 1 12) (bitmap "body2.jpg")))
(define IMG (scale (/ 1 10) (bitmap "wall.png")))
(define HEAD-IMAGE (scale (/ 1 20) (bitmap "head.png")))

(define HEAD-LEFT-IMAGE HEAD-IMAGE)
(define HEAD-DOWN-IMAGE (rotate -90 HEAD-LEFT-IMAGE))
(define HEAD-RIGHT-IMAGE (flip-horizontal HEAD-LEFT-IMAGE))
(define HEAD-UP-IMAGE (flip-vertical HEAD-DOWN-IMAGE))

(define BOARD (empty-scene WIDTH HEIGHT))

(define (same a b)
    (and  (= (posn-x a) (posn-x b))
      (= (posn-y a) (posn-y b))))

(define (draw-scene posns img scene)
  (cond [(empty? posns) scene]
        [else (draw-image (first posns)
                         img 
                         (draw-scene (rest posns) img scene))]))

(define (draw-image posn img scene)
  (place-image img 
               (* (posn-x posn) SEG-SIZE)
               (* (posn-y posn) SEG-SIZE)
               scene))

(define (new-multiplayer)
  (big-bang (pit (snake (vec 1 0) (list (posn 46 22)))
                 (snake (vec -1 0) (list (posn 44 22))))
            (on-tick next-pit TICK-RATE)
            (on-key direct-snake)
            (to-draw render-pit)
            (stop-when dead? render-end)))

(define (dead? w)
  (define snake1 (pit-snakep1 w))
  (define snake2 (pit-snakep2 w))
  (or (self-colliding? snake1)
      (self-colliding? snake2)
      (wall-colliding? snake1)
      (wall-colliding? snake2)
      (other-colliding? snake1 snake2)))

(define (self-colliding? snake1)
    (cons? (member (head-snake snake1) (not-head snake1))))

(define (other-colliding? snake1 snake2)
  (or (cons? (member (head-snake snake1) (not-head snake2)))
      (cons? (member (head-snake snake2) (not-head snake1)))))
      

(define (wall-colliding? snake1)
  (define x (posn-x (head-snake snake1)))
  (define y (posn-y (head-snake snake1)))
  (or (= 0 x) (= x SIZE-x)
      (= 0 y) (= y SIZE-y)))

(define (render-pit pit1)
  (draw-snake (pit-snakep1 pit1)
              IMAGE1
  (draw-snake (pit-snakep2 pit1)
              IMAGE2 BOARD)))



(define (render-end pit1)
  (let* [(snake1 (pit-snakep1 pit1))
         (snake2 (pit-snakep2 pit1))]
    (cond [(or (self-colliding? snake1)
               (wall-colliding? snake1)
               (cons? (member (head-snake snake1) (snake-body snake2))))
          (overlay  (text "PLAYER 2 WINS" 50 "black")
           (render-pit pit1))]
          [else (overlay  (text "PLAYER 1 WINS" 50 "black")
           (render-pit pit1))])))



(define (head-snake snake1)
  (car (snake-body snake1)))

(define (not-head snake1)
  (cdr (snake-body snake1)))

(define (next-pit ini-pit)
  (let* [(snake1 (pit-snakep1 ini-pit))
         (snake2 (pit-snakep2 ini-pit))]
    
        (pit (just-ate snake1) (just-ate snake2))))


(define (just-ate snake1)
  (let* ((X (vec-x (snake-dir snake1)))
         (Y (vec-y (snake-dir snake1)))
         (P-x (posn-x (head-snake snake1)))
         (P-y(posn-y (head-snake snake1))))
  (snake (snake-dir snake1) (cons (posn (+ X P-x)
                                        (+ Y P-y)) 
                                (snake-body snake1)))))


(define (slither snake1)
  (let* ((X (vec-x (snake-dir snake1)))
         (Y (vec-y (snake-dir snake1)))
         (P-x (posn-x (head-snake snake1)))
         (P-y(posn-y (head-snake snake1))))
  (snake (snake-dir snake1)  (cons (posn (+ X P-x)
                               (+ Y P-y))
                                   (except-last (snake-body snake1))))))


(define (except-last body1)
  (if (null? (cdr body1)) '()
      (cons (car body1) (except-last (cdr body1)))))
        
(define (direct-snake pit1 ke)
  (cond [(key-set1? ke) (world-change-dir1 pit1 ke)]
        [(key-set2? ke) (world-change-dir2 pit1 ke)]
        [else pit1]))

(define (key-set1? ke)
  (or (string=? ke "up")
      (string=? ke "down")
      (string=? ke "left")
      (string=? ke "right")))

(define  (key-set2? ke)
  (or (string=? ke "w")
      (string=? ke "s")
      (string=? ke "a")
      (string=? ke "d")))

(define (world-change-dir1 pit1 ke)
  (let* [(snake1 (pit-snakep1 pit1))
         (snake2 (pit-snakep2 pit1))]
    (if (opposite1? (snake-dir snake1) ke) pit1

        (pit (change-dir1 snake1 ke) snake2))))

(define (world-change-dir2 pit1 ke)
  (let* [(snake1 (pit-snakep1 pit1))
         (snake2 (pit-snakep2 pit1))]
    (if (opposite2? (snake-dir snake2) ke) pit1

        (pit snake1 (change-dir2 snake2 ke)))))


   


(define (change-dir1 snake1 ke)
  (cond [(string=? ke "up") (snake (vec 0 -1) (snake-body snake1))] 
        [(string=? ke "down") (snake (vec 0 1) (snake-body snake1))]
        [(string=? ke "right") (snake (vec 1 0) (snake-body snake1))]
        [(string=? ke "left") (snake (vec -1 0) (snake-body snake1))]))

(define (change-dir2 snake1 ke)
  (cond [(string=? ke "w") (snake (vec 0 -1) (snake-body snake1))] 
        [(string=? ke "s") (snake (vec 0 1) (snake-body snake1))]
        [(string=? ke "a") (snake (vec -1 0) (snake-body snake1))]
        [(string=? ke "d") (snake (vec 1 0) (snake-body snake1))]))

(define (opposite1? d1 d2)
  (cond [(and (equal? d1 (vec 0 -1)) (string=? d2 "down"))#t]
        [(and (equal? d1 (vec 0 1)) (string=? d2 "up"))#t]
        [(and (equal? d1 (vec -1 0)) (string=? d2 "right"))#t]
        [(and (equal? d1 (vec 1 0)) (string=? d2 "left"))#t]
        [else #f]))

(define (opposite2? d1 d2)
  (cond [(and (equal? d1 (vec 0 -1)) (string=? d2 "s"))#t]
        [(and (equal? d1 (vec 0 1)) (string=? d2 "w"))#t]
        [(and (equal? d1 (vec -1 0)) (string=? d2 "d"))#t]
        [(and (equal? d1 (vec 1 0)) (string=? d2 "a"))#t]
        [else #f]))



(define (draw-snake snake1 img scene)
  (define draw-snake-body (draw-scene (not-head snake1)
                                     img
                                      scene))
  (define dir1 (snake-dir snake1))

  
  (draw-image (head-snake snake1)
              (cond [(equal? dir1 (vec 0 1)) HEAD-UP-IMAGE]
                    [(equal? dir1 (vec 0 -1)) HEAD-DOWN-IMAGE]
                    [(equal? dir1 (vec 1 0)) HEAD-RIGHT-IMAGE]
                    [(equal? dir1 (vec -1 0)) HEAD-LEFT-IMAGE])
              draw-snake-body))

 