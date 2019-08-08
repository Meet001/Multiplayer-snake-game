#lang racket/gui

(require 2htdp/universe 2htdp/image)

(require "Multi-player.rkt")

(struct pit (snake food) #:transparent)

(struct snake (dir body) #:transparent)

(struct posn (x y) #:transparent)

(struct food (loc) #:transparent)

(struct vec (x y) #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definitions and Initialisations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define TICK-RATE (/ 1 10))

(define SIZE-x 90)
(define SIZE-y 45)

(define SEG-SIZE 15)

(define WIDTH (* SIZE-x SEG-SIZE))
(define HEIGHT (* SIZE-y SEG-SIZE))

(define FOOD-IMAGE (scale (/ 1 40) (bitmap "food.png")))
(define SNAKE-BODY-IMAGE (scale (/ 1 15) (bitmap "body1.png")))
(define IMG (scale (/ 1 10) (bitmap "wall.png")))
(define HEAD-IMAGE (scale (/ 1 20) (bitmap "head.png")))

(define HEAD-LEFT-IMAGE HEAD-IMAGE)
(define HEAD-DOWN-IMAGE (rotate -90 HEAD-LEFT-IMAGE))
(define HEAD-RIGHT-IMAGE (flip-horizontal HEAD-LEFT-IMAGE))
(define HEAD-UP-IMAGE (flip-vertical HEAD-DOWN-IMAGE))

(define BOARD (empty-scene WIDTH HEIGHT))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Common Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (same a b)
    (and  (= (posn-x a) (posn-x b))
      (= (posn-y a) (posn-y b))))

(define (wall-gen posn1 posn2 list)
  (let*[(x1 (posn-x posn1))
        (x2 (posn-x posn2))
        (y1 (posn-y posn1))
        (y2 (posn-y posn2))]
    (cond[(same posn1 posn2) (reverse(append (cons posn2 '()) list))]
         [(> x1 x2) (wall-gen posn2 posn1 '())]
         [(> y1 y2) (wall-gen posn2 posn1 '())]
         [(= x1 x2)(wall-gen (posn x1 (+ y1 1)) posn2 (append (cons posn1 '()) list))]
         [(= y1 y2)(wall-gen (posn (+ x1 1) y1) posn2 (append (cons posn1 '()) list))]
         [else '()])))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; Level Specific Functions



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Level 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (lvl0)
  (big-bang (pit (snake (vec 1 0) (list (posn 4 4)))
                 (new-food))
            (on-tick next-pit TICK-RATE)
            (on-key direct-snake)
            (to-draw render-pit)
            (stop-when dead? render-end)))

(define (dead? w)
  (define snake1 (pit-snake w))
  (or (self-colliding? snake1) (wall-colliding? snake1)))

(define (self-colliding? snake1)
    (cons? (member (head-snake snake1) (not-head snake1))))


(define (wall-colliding? snake1)
  (define x (posn-x (head-snake snake1)))
  (define y (posn-y (head-snake snake1)))
  (or (= 0 x) (= x SIZE-x)
      (= 0 y) (= y SIZE-y)))

(define (render-pit pit1)
  (draw-snake (pit-snake pit1)
              (draw-food (pit-food pit1) BOARD)))



(define (render-end pit1)
  (define (final)
    (- (length (snake-body (pit-snake pit1))) 1))
  
  (overlay  (text "Score:-                 "  50 "black")
           (text (number->string (final))  50 "black")
           (render-pit pit1)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Level 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define scene1
  (append (wall-gen (posn 15 15) (posn 15 30) '())
          (wall-gen (posn 75 15) (posn 75 30) '())
          (wall-gen (posn 45 10) (posn 45 35) '())
          (wall-gen (posn 30 10) (posn 60 10) '())
          (wall-gen (posn 30 35) (posn 60 35) '())
          (wall-gen (posn 15 22) (posn 75 22) '())))


(define LVL1 (draw-scene scene1 IMG BOARD))


(define (lvl1)
  (big-bang (pit (snake (vec 1 0) (list (posn 1 1)))
                 (new-food))
            (on-tick next-pit TICK-RATE)
            (on-key direct-snake)
            (to-draw render-pit1)
            (stop-when dead1? render-end2)))


(define (render-pit1 pit1)
  (draw-snake (pit-snake pit1)
              (draw-food (pit-food pit1) LVL1)))

(define (dead1? w)
  (define snake1 (pit-snake w))
  (or (self-colliding? snake1) (wall-colliding1? snake1)))


(define (render-end2 pit1)
  (define (final)
    (- (length (snake-body (pit-snake pit1))) 1))
  
  (overlay  (text "Score:-                 "  50 "black")
           (text (number->string (final))  50 "black")
           (render-pit1 pit1)))


(define (wall-colliding1? snake1)
  (define x (posn-x (head-snake snake1)))
  (define y (posn-y (head-snake snake1)))
  (or (= 0 x) (= x SIZE-x)
      (= 0 y) (= y SIZE-y)
      (cons? (member (head-snake snake1) scene1))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Level 3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(define scene2 (append (wall-gen (posn 10 10) (posn 30 10) '())
                     (wall-gen (posn 10 10) (posn 10 15) '())
                     
                     (wall-gen (posn 60 10) (posn 80 10) '())
                     (wall-gen (posn 80 10) (posn 80 15) '())
                     
                     (wall-gen (posn 10 30) (posn 10 35) '())
                     (wall-gen (posn 10 35) (posn 30 35) '())
                     
                     (wall-gen (posn 80 30) (posn 80 35) '())
                     (wall-gen (posn 60 35) (posn 80 35) '())
                    ))


(define LVL2 (draw-scene scene2 IMG BOARD))


(define (lvl2)
  (big-bang (pit (snake (vec 1 0) (list (posn 1 1)))
                 (new-food))
            (on-tick next-pit TICK-RATE)
            (on-key direct-snake)
            (to-draw render-pit2)
            (stop-when dead2? render-end3)))


(define (render-pit2 pit1)
  (draw-snake (pit-snake pit1)
              (draw-food (pit-food pit1) LVL2)))

(define (dead2? w)
  (define snake1 (pit-snake w))
  (or (self-colliding? snake1) (wall-colliding2? snake1)))


(define (render-end3 pit1)
  (define (final)
    (- (length (snake-body (pit-snake pit1))) 1))
  
  (overlay  (text "Score:-                 "  50 "black")
           (text (number->string (final))  50 "black")
           (render-pit2 pit1)))

(define (wall-colliding2? snake1)
  (define x (posn-x (head-snake snake1)))
  (define y (posn-y (head-snake snake1)))
  (or (= 0 x) (= x SIZE-x)
      (= 0 y) (= y SIZE-y)
      (cons? (member (head-snake snake1) scene2))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Level 4
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(define scene3 (append (wall-gen (posn 10 10) (posn 80 10) '())
                     (wall-gen (posn 80 10) (posn 80 35) '())
                     (wall-gen (posn 80 35) (posn 10 35) '())
                     (wall-gen (posn 10 35) (posn 10 22) '())
                     (wall-gen (posn 10 22) (posn 60 22) '())
                      ))

(define LVL3 (draw-scene scene3 IMG BOARD))


(define (lvl3)
  (big-bang (pit (snake (vec 1 0) (list (posn 1 1)))
                 (new-food))
            (on-tick next-pit TICK-RATE)
            (on-key direct-snake)
            (to-draw render-pit3)
            (stop-when dead3? render-end4)))


(define (render-pit3 pit1)
  (draw-snake (pit-snake pit1)
              (draw-food (pit-food pit1) LVL3)))

(define (dead3? w)
  (define snake1 (pit-snake w))
  (or (self-colliding? snake1) (wall-colliding3? snake1)))


(define (render-end4 pit1)
  (define (final)
    (- (length (snake-body (pit-snake pit1))) 1))
  
  (overlay  (text "Score:-                 "  50 "black")
           (text (number->string (final))  50 "black")
           (render-pit3 pit1)))

(define (wall-colliding3? snake1)
  (define x (posn-x (head-snake snake1)))
  (define y (posn-y (head-snake snake1)))
  (or (= 0 x) (= x SIZE-x)
      (= 0 y) (= y SIZE-y)
      (cons? (member (head-snake snake1) scene3))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Remaining Common Functions

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define window  (new frame%
                   [label "Snake"]
                   [width 700]
                   [height 600]))

(define message (new message% [parent window]
                              [label "Play Classic Snake"]))

(send window show #t)

(define bitmap1 (read-bitmap "logo.png"))
(new message% [parent window]
              [label bitmap1])

(new button% [parent window]
             [label "Play Level 1"]
          [callback (lambda (button event)
                      (lvl0))])

(new button% [parent window]
             [label "Play Level 2"]
          [callback (lambda (button event)
                      (lvl1))])

(new button% [parent window]
             [label "Play Level 3"]
          [callback (lambda (button event)
                      (lvl2))])

(new button% [parent window]
             [label "Play Level 4"]
          [callback (lambda (button event)
                      (lvl3))])

(new button% [parent window]
             [label "Play Multiplayer"] 
          [callback (lambda (button event)
                      (new-multiplayer))])



(define (head-snake snake1)
  (car (snake-body snake1)))

(define (not-head snake1)
  (cdr (snake-body snake1)))

(define (next-pit ini-pit)
  (let* [(snake1 (pit-snake ini-pit))
         (food1 (pit-food ini-pit))]
    (if (can-eat snake1 food1)
        (pit (just-ate snake1) (new-food))
        (pit (slither snake1) food1))))

(define (can-eat snake1 food1)
  (if (same (food-loc food1) (head-snake snake1))#t
         #f))



(define (new-food)
  (define (scaled-random x1 x2)
          (+ x1 (* (random) (- x2 x1))))
  (let
        ((food-posn (posn (inexact->exact (round (* (scaled-random 1 8) 10)))
                          (inexact->exact (round (* (scaled-random 0.5 4) 10))))))

    (if (or (cons? (member food-posn scene1))
          (cons? (member food-posn scene2))
          (cons? (member food-posn scene3)))
      (new-food)
           (food food-posn)                                       
         )))

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
  (cond [(dir? ke) (world-change-dir pit1 ke)]
        [else pit1]))

(define (dir? x)
  (or (string=? x "up")
      (string=? x "down")
      (string=? x "left")
      (string=? x "right")))

(define (world-change-dir pit1 ke)
  (let* [(snake1 (pit-snake pit1))]
    (if (opposite? (snake-dir snake1) ke) pit1

        (pit (change-dir snake1 ke) (pit-food pit1)))))


(define (change-dir snake1 ke)
  (cond [(string=? ke "up") (snake (vec 0 -1) (snake-body snake1))] 
        [(string=? ke "down") (snake (vec 0 1) (snake-body snake1))]
        [(string=? ke "right") (snake (vec 1 0) (snake-body snake1))]
        [(string=? ke "left") (snake (vec -1 0) (snake-body snake1))]))

(define (opposite? d1 d2)
  (cond [(and (equal? d1 (vec 0 -1)) (string=? d2 "down"))#t]
        [(and (equal? d1 (vec 0 1)) (string=? d2 "up"))#t]
        [(and (equal? d1 (vec -1 0)) (string=? d2 "right"))#t]
        [(and (equal? d1 (vec 1 0)) (string=? d2 "left"))#t]
        [else #f]))



(define (draw-snake snake1 scene)
  (define draw-snake-body (draw-scene (not-head snake1)
                                      SNAKE-BODY-IMAGE
                                      scene))
  (define dir1 (snake-dir snake1))

  
  (draw-image (head-snake snake1)
              (cond [(equal? dir1 (vec 0 1)) HEAD-UP-IMAGE]
                    [(equal? dir1 (vec 0 -1)) HEAD-DOWN-IMAGE]
                    [(equal? dir1 (vec 1 0)) HEAD-RIGHT-IMAGE]
                    [(equal? dir1 (vec -1 0)) HEAD-LEFT-IMAGE])
              draw-snake-body))

(define (draw-food food1 scene)
   (draw-image (food-loc food1) FOOD-IMAGE scene))  





