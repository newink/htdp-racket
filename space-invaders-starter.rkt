;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders


;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 2)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 100)

(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))

(define MISSILE (ellipse 5 15 "solid" "red"))



;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))



(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))



(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))


(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))



(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))

;; ============ World ==============
;; Game -> Game
;; start the space invaders game
;; 
(define (main ws)
  (big-bang ws                   ; Game
    (on-tick   next-game)     ; Game -> Game
    (to-draw   render-game)   ; Game -> Image
    (stop-when game-over?)      ; Game -> Boolean
    (on-mouse  handle-mouse)      ; Game Integer Integer MouseEvent -> Game
    (on-key    handle-key)))    ; Game KeyEvent -> Game

;; Game -> Game
;; produces next state of the game

;(define (next-game g) (...)) ;stub

(define (next-game s)
  (make-game (next-invaders (game-invaders s) (game-missiles s))
             (next-missiles (game-missiles s))
             (next-tank (game-tank s))))

;; ListOfInvaders -> ListOfInvaders
;; produce next state of invaders on screen

;(define (next-invaders loi) loi)
(check-expect (next-invaders empty empty) empty)
(check-expect (next-invaders (list I1) empty) (list (make-invader 162 102 12)))

(define (next-invaders loi lom)
  (cond [(empty? loi) empty]
        [(> (random 100) 95) (move-invaders (add-invader loi))]
        [else (move-invaders loi)]))

;; ListOfInvaders ListOfMissiles -> ListOfInvaders
;; checks collisions, removes killed invaders

(define (remove-killed loi lom)
  (cond [(empty? loi) empty]
        [else (if (check-collisions (first loi) lom) (remove-killed (rest loi) lom) (cons (first loi) (remove-killed (rest loi) lom)))]))

;; Invader ListOfMissiles -> Boolean
;; checks if invader collides with any missile

(define (check-collisions i lom)
  (or (check-collision i (first lom)) (check-collisions i (rest lom))))

;; Invader Missile -> Boolean
;; checks if invader collides with missile

(check-expect (check-collision (make-invader 10 20 10) (make-missile 300 500)) false)
(check-expect (check-collision (make-invader 10 20 10) (make-missile 10 20)) false)

(define (check-collision i m)
  (and (< (missile-x m) (+ 10 (invader-x i))) (> (missile-x m) (- (invader-x i) 10)) (> (missile-y m) (- (invader-y i) 10)) (< (missile-x m) (+ 10 (invader-y i)))))

;; ListOfInvaders -> ListOfInvaders
;; produce list of moved invaders according to speed and direction

(check-expect (move-invaders (list I1)) (list (make-invader 162 102 12)))
;(define (move-invaders loi) loi) ;stub

(define (move-invaders loi)
  (cond [(empty? loi) empty]
        [else (cons (move-invader (first loi))
                    (move-invaders (rest loi)))]))

;; Invader -> Invader
;; produce next state of invader in game

(check-expect (move-invader I1) (make-invader 162 102 12))
(check-expect (move-invader I2) (make-invader 140 (+ HEIGHT INVADER-Y-SPEED) -10))

(define (move-invader i)
  (cond [(> (invader-x i) WIDTH) (make-invader WIDTH (+ (invader-y i) INVADER-Y-SPEED) (* -1 (invader-dx i)))]
        [(< (invader-x i) 0) (make-invader 0 (+ (invader-y i) INVADER-Y-SPEED) (* -1 (invader-dx i)))]
        [else (make-invader (+ (invader-x i) (invader-dx i)) (+ (invader-y i) INVADER-Y-SPEED) (invader-dx i))]))

;; ListOfInvaders -> ListOfInvaders
;; add new invader to list

(define (add-invader loi)
  (cons (make-invader (random WIDTH) 0 (random 5)) loi))

;(define (add-invader loi) loi) ;stub

;; ListOfMissiles -> ListOfMissiles
;; produce next state of missiles in game

;(define (next-missiles lom) lom) ;stub
(check-expect (next-missiles (list M1)) (list (make-missile 150 290)))

(define (next-missiles lom)
  (cond [(empty? lom) empty]
        [else (cons (make-missile (missile-x (first lom)) (- (missile-y (first lom)) MISSILE-SPEED)) (next-missiles (rest lom)))]))

;; Tank -> Tank
;; produce next state of tank

;(define (next-tank t) t) ;stub

(check-expect (next-tank T1) (make-tank 52 1))
(check-expect (next-tank T2) (make-tank 48 -1))
(check-expect (next-tank (make-tank -1 -1)) (make-tank 0 -1))
(check-expect (next-tank (make-tank (+ WIDTH 10) 1)) (make-tank WIDTH 1))

(define (next-tank t)
  (cond [(= (tank-dir t) 1) (if (< (tank-x t) WIDTH) (make-tank (+ (tank-x t) TANK-SPEED) (tank-dir t)) (make-tank WIDTH (tank-dir t)))]
        [else (if (> (tank-x t) 0) (make-tank (- (tank-x t) TANK-SPEED) (tank-dir t)) (make-tank 0 (tank-dir t)))]))


;======
;; Game -> Image
;; renders game state to image
(define (render-game g) (render-tank (game-tank g) (render-missiles (game-missiles g) (render-invaders (game-invaders g) BACKGROUND)))) ;stub

;; ListOfInvaders Image -> Image
;; render invaders on background

(define (render-invaders loi b)
  (place-images (render-obj loi INVADER) (invaders-to-posns loi) b))

;; ListOfMissiles Image -> Image
;; render missiles on background

(define (render-missiles lom b)
  (place-images (render-obj lom MISSILE) (missiles-to-posns lom) b))

;; Tank Image -> Image
;; render tank on background

(define (render-tank t b)
  (place-image TANK (tank-x t) (- HEIGHT 15) b))

;; ListOfInvader -> ListOfPosn
;; map list of invaders to list of positions

(define (invaders-to-posns loi)
  (cond [(empty? loi) empty]
        [else (cons (make-posn (invader-x (first loi)) (invader-y (first loi))) (invaders-to-posns (rest loi)))]))

;; ListOfMissiles -> ListOfPosn
;; map list of missiles to list of positions 
(define (missiles-to-posns lom)
  (cond [(empty? lom) empty]
        [else (cons (make-posn (missile-x (first lom)) (missile-y (first lom))) (missiles-to-posns (rest lom)))]))

;; List Image -> ListOfImage
;; map list of object to list of passed images

(define (render-obj loobj i)
  (cond [(empty? loobj) empty]
        [else (cons i (render-obj (rest loobj) i))]))

;======
;; Game Integer Integer MouseEvent -> Game
;; handle mouse event, producing new game state
;(define (handle-mouse g x y me) G0) ;stub

(define (handle-mouse g x y me)
  (cond [(mouse=? me "button-down") (fire-missile g)]
        [else g]))

;; Game -> Game
;; produce state of game with missile fired

(define (fire-missile g)
  (make-game (game-invaders g)
             (add-missile (game-missiles g) (game-tank g))
             (game-tank g)))

;; ListOfMissile Tank -> ListOfMissile
;; add missile at tank position

(define (add-missile lom t)
  (cons (make-missile (tank-x t) (- HEIGHT 10)) lom))


;======
;; Game KeyEvent -> Game
;; handle keyboard event, producing new game state
;(define (handle-key g ke) G0) ;stub

(define (handle-key g ke)
  (cond [(key=? ke "left") (change-direction g -1)]
        [(key=? ke "right") (change-direction g 1)]
        [else g]))

;; Game [-1, 1] -> Game
;; change direction of tank in game

(define (change-direction g n)
  (make-game (game-invaders g)
             (game-missiles g)
             (make-tank (tank-x (game-tank g)) n)))

;======

;; Game -> Boolean
;; checks if game is over

;(define (game-over? g) false) ;stub

(define (game-over? g)
  (invader-passed? (game-invaders g)))

;; ListOfInvaders -> Boolean
;; checks if invader passed the tank

(check-expect (invader-passed? (list I1)) false)
(check-expect (invader-passed? (list (make-invader WIDTH (+ 1 HEIGHT) 10))) true)

(define (invader-passed? loi)
  (cond [(empty? loi) false]
        [else (if (> (invader-y (first loi)) HEIGHT) true (invader-passed? (rest loi)))]))







