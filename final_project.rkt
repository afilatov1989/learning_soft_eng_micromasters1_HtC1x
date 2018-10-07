;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders

;; =================
;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
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


;; =================
;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game ListOfInvader ListOfMissile Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below ListOfInvader data definition

#;
(define (fn-for-game g)
  (... (fn-for-loinvader (game-invaders g))
       (fn-for-lom (game-missiles g))
       (fn-for-tank (game-tank g))))

;; Template rules used:
;;  - compound: (make-game ListOfInvader ListOfMissile Tank)
;;  - reference: invaders is ListOfInvader
;;  - reference: missiles is ListOfMissile
;;  - reference: tank is Tank


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

;; Template rules used:
;;  - compound: (make-tank Number Integer[-1, 1])


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

;; Template rules used:
;;  - compound: (make-invader Number Number Number)


(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1
(define M4 (make-missile (invader-x I2) (+ (invader-y I2)  5)))  ;> hit U2

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))

;; Template rules used:
;;  - compound: (make-missile Number Number)


;; ListOfMissile is one of:
;;  - empty
;;  - (cons Missile ListOfMissile)
;; interp. a list of missiles

(define LOM0 empty)
(define LOM1 (list M1))
(define LOM2 (list M1 M2))
(define LOM3 (list M1 M2 M3))

#;
(define (fn-for-lom lom)
  (cond [(empty? lom) (...)]                    ; Base case
        [else (... (fn-for-missile (first lom)) ; Natural helper
                   (fn-for-lom (rest lom)))]))  ; Natural recursion

;; Template rules used:
;;  - one of: 2 cases
;;  - atomic distinct: empty
;;  - compound: (cons Missile ListOfMissile)
;;  - reference: (first lom) is Missile
;;  - self-reference: (rest lom) is ListOfMissile


;; ListOfInvader is one of:
;;  - empty
;;  - (cons Invader ListOfInvader)
;; interp. a list of invaders

(define LOI0 empty)
(define LOI1 (list I1))
(define LOI2 (list I1 I2))
(define LOI3 (list I1 I2 I3))

#;
(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]                    ; Base case
        [else (... (fn-for-invader (first loi)) ; Natural helper
                   (fn-for-loi (rest loi)))]))  ; Natural recursion

;; Template rules used:
;;  - one of: 2 cases
;;  - atomic distinct: empty
;;  - compound: (cons Invader ListOfInvader)
;;  - reference: (first loi) is Invader
;;  - self-reference: (rest loi) is ListOfInvader


(define G000 (make-game empty empty T0))
(define G001 (make-game empty empty T1))
(define G002 (make-game empty empty T2))
(define G021 (make-game LOI0 LOM2 T1))
(define G111 (make-game LOI1 LOM1 T1))
(define G221 (make-game LOI2 LOM2 T1))
(define G222 (make-game LOI2 LOM2 T2))


;; =================
;; Functions:

;; Game -> Game
;; start the world with initial state g, for example G0, defined above
;; 
(define (main g)
  (big-bang g                  ; Game
    (on-tick   tock)           ; Game -> Game
    (to-draw   render)         ; Game -> Image
    (stop-when game-over?)     ; Game -> Boolean
    (on-key    handle-key)))   ; Game KeyEvent -> Game


;; Game -> Game
;; produce the next game state

;; can't write check-expects, because function includes randomness in generate-new-invaders

;(define (tock g) g) ; stub

(define (tock g)
  (process-hits (make-game (generate-new-invader (move-invaders (game-invaders g)))
                           (move-missiles (game-missiles g))
                           (move-tank (game-tank g)))))

;; ListOfInvader -> ListOfInvader
;; prepends a new invader with random x coord to the list with probability (1 / INVADE-RATE)

;; can't write any reliable check-expect as the result is random

;(define (generate-new-invader loi) loi) ; stub

(define (generate-new-invader loi)
  (if (= 0 (random INVADE-RATE))
      (cons (make-invader (random WIDTH) 0 INVADER-X-SPEED) loi)
      loi))


;; ListOfInvader -> ListOfInvader
;; moves all invaders to the next position
(check-expect (move-invaders empty) empty)
(check-expect (move-invaders (list I1)) (list (move-invader I1)))
(check-expect (move-invaders (list I1 I2)) (list (move-invader I1) (move-invader I2)))

;(define (move-invaders loi) loi) ;stub

(define (move-invaders loi)
  (cond [(empty? loi) empty]
        [else (cons (move-invader (first loi))
                    (move-invaders (rest loi)))]))


;; Invader -> Invader
;; moves an invader to the next position
(check-expect (move-invader (make-invader 50 70 10))
              (make-invader 60 (+ 70 INVADER-Y-SPEED) 10))
(check-expect (move-invader (make-invader 40 80 -15))
              (make-invader 25 (+ 80 INVADER-Y-SPEED) -15))
(check-expect (move-invader (make-invader WIDTH 20 10))  ; turn left
              (make-invader WIDTH (+ 20 INVADER-Y-SPEED) -10))
(check-expect (move-invader (make-invader 0 20 -15))  ; turn right
              (make-invader 0 (+ 20 INVADER-Y-SPEED) 15))
 
;(define (move-invader i) i) ; stub

(define (move-invader i)
  (cond [(and (< (invader-dx i) 0) (<= (invader-x i) 0))
         (make-invader 0 (+ (invader-y i) INVADER-Y-SPEED) (- (invader-dx i)))]
        [(and (> (invader-dx i) 0) (>= (invader-x i) WIDTH))
         (make-invader WIDTH (+ (invader-y i) INVADER-Y-SPEED) (- (invader-dx i)))]
        [else
         (make-invader (+ (invader-x i) (invader-dx i))
                       (+ (invader-y i) INVADER-Y-SPEED)
                       (invader-dx i))]))


;; ListOfMissile -> ListOfMissile
;; moves all missiles to the next positioni
;; if missile reaches the top of the screen, it gets removed from the list
(check-expect (move-missiles empty) empty)
(check-expect (move-missiles (list M1))
              (list (move-missile M1)))
(check-expect (move-missiles (list M1 M2))
              (list (move-missile M1) (move-missile M2)))
(check-expect (move-missiles (list M1 (make-missile 150 0) M2))
              (list (move-missile M1) (move-missile M2))) ; remove missile reached top

;(define (move-missiles lom) lom) ;stub

(define (move-missiles lom)
  (cond [(empty? lom) empty]
        [(<= (missile-y (first lom)) 0) (move-missiles (rest lom))]
        [else (cons (move-missile (first lom)) (move-missiles (rest lom)))]))

;; Missile -> Missile
;; move the missile to the next position
(check-expect (move-missile (make-missile 120 20))
              (make-missile 120 (- 20 MISSILE-SPEED)))

;(define (move-missile m) m) ;stub

(define (move-missile m)
  (make-missile (missile-x m) (- (missile-y m) MISSILE-SPEED)))


;; Tank -> Tank
;; moves a tank to the next position
(check-expect (move-tank (make-tank 20 1)) (make-tank (+ 20 TANK-SPEED) 1))   ;right
(check-expect (move-tank (make-tank 30 -1)) (make-tank (- 30 TANK-SPEED) -1)) ;left
(check-expect (move-tank (make-tank WIDTH 1)) (make-tank WIDTH -1))           ;turn left
(check-expect (move-tank (make-tank 0 -1)) (make-tank 0 1))                   ;turn right

;(define (move-tank t) t) ;stub

(define (move-tank t)
  (cond [(and (= (tank-dir t) -1) (<= (tank-x t) 0))
         (make-tank 0 1)]
        [(and (= (tank-dir t) 1) (>= (tank-x t) WIDTH))
         (make-tank WIDTH -1)]
        [(= (tank-dir t) 1)
         (make-tank (+ (tank-x t) TANK-SPEED) 1)]
        [(= (tank-dir t) -1)
         (make-tank (- (tank-x t) TANK-SPEED) -1)]
        ))


;; Game -> Game
;; removes missiles and invaders, which destroy each other, from the game state
(check-expect (process-hits G000) G000)                             ; 2 empty lists, no hits
(check-expect (process-hits (make-game (list I2 I3) (list M1) T0))
              (make-game (list I2 I3) (list M1) T0))                ; no hit
(check-expect (process-hits (make-game (list I1 I2) (list M2) T0))
              (make-game (list I2) empty T0))                       ; exact hit
(check-expect (process-hits (make-game (list I1 I2) (list M3) T0))
              (make-game (list I2) empty T0))                       ; not-exact hit
(check-expect (process-hits (make-game (list I1 I2 I3) (list M3 M4) T0))
              (make-game empty empty T0))                           ; not-exact 3 hits

;(define (process-hits g) g) ; stub

(define (process-hits g)
  (make-game (remove-invaders (game-invaders g) (game-missiles g))
             (remove-missiles (game-missiles g) (game-invaders g))
             (game-tank g)))


;; ListOfInvader ListOfMissile -> ListOfInvader
;; removes all invaders hitted by missiles from the list
(check-expect (remove-invaders empty empty) empty)     ; 2 empty lists, no hits
(check-expect (remove-invaders (list I2 I3) (list M1))
              (list I2 I3))                            ; no hit
(check-expect (remove-invaders (list I1 I2) (list M2))
              (list I2))                               ; exact hit
(check-expect (remove-invaders (list I1 I2) (list M3))
              (list I2))                               ; not-exact hit
(check-expect (remove-invaders (list I1 I2 I3) (list M3 M4))
              empty)                                   ; not-exact 3 hits

;(define (remove-invaders loi lom) loi) ; stub

(define (remove-invaders loi lom)
  (cond [(empty? loi) empty]
        [else (if (remove-invader? lom (first loi))
                  (remove-invaders (rest loi) lom)
                  (cons (first loi) (remove-invaders (rest loi) lom)))]))


;; ListOfMissile Invader -> Boolean
;; return true if and only if at least one missile hits the invader
(check-expect (remove-invader? LOM0 I1) false) ; empty list of missiles, no hit
(check-expect (remove-invader? LOM1 I1) false) ; 1 elem list, no hit
(check-expect (remove-invader? LOM2 I1) true)  ; exact hit
(check-expect (remove-invader? (list M3 M4) I1) true)  ; not-exact hit

;(define (remove-invader? lom i) false) ; stub

(define (remove-invader? lom i)
  (cond [(empty? lom) false]
        [else (if (missile-hits-invader? (first lom) i)
                  true
                  (remove-invader? (rest lom) i))]))


;; Missile Invader -> Boolean
;; return true if missile hits invader
(check-expect (missile-hits-invader? M1 I1) false)
(check-expect (missile-hits-invader? M2 I1) true) ; exact hit
(check-expect (missile-hits-invader? M3 I1) true) ; not-exact hit

;(define (missile-hits-invader? m i) false) ; stub

(define (missile-hits-invader? m i)
  (and (<= (abs (- (invader-x i) (missile-x m))) HIT-RANGE)
       (<= (abs (- (invader-y i) (missile-y m))) HIT-RANGE)))


;; ListOfMissile ListOfInvader -> ListOfMissile
;; remove all missiles, which hitted any invader from the list
(check-expect (remove-missiles empty empty) empty)            ; 2 empty lists, no hits
(check-expect (remove-missiles (list M1) (list I2 I3)) 
              (list M1))                                      ; no hit
(check-expect (remove-missiles (list M2) (list I1 I2))
              empty)                                          ; exact hit
(check-expect (remove-missiles (list M3) (list I1 I2))
              empty)                                          ; not-exact hit
(check-expect (remove-missiles (list M3 M4) (list I1 I2 I3))
              empty)                                          ; not-exact 3 hits

;(define (remove-missiles lom loi) lom) ; stub

(define (remove-missiles lom loi)
  (cond [(empty? lom) empty]
        [else (if (remove-missile? loi (first lom))
                  (remove-missiles (rest lom) loi)
                  (cons (first lom) (remove-missiles (rest lom) loi)))]))


;; ListOfInvader Missile -> Boolean
;; returns true if and only if a missile hits at least one invader from the list
(check-expect (remove-missile? empty M1) false) ; empty list, no hit
(check-expect (remove-missile? (list I2 I3) M1) false) ; no hit
(check-expect (remove-missile? (list I1 I2) M2) true)  ; exact hit
(check-expect (remove-missile? (list I1 I2) M3) true)  ; not-exact hit

;(define (remove-missile? loi m) false) ; stub

(define (remove-missile? loi m)
  (cond [(empty? loi) false]
        [else (if (missile-hits-invader? m (first loi))
                  true
                  (remove-missile? (rest loi) m))]))


;; Game -> Image
;; render the game state, placing the tank, missiles and invaders in proper positions
(check-expect (render G000)
              (render-invaders LOI0 (render-missiles LOM0 (render-tank T0 BACKGROUND))))
(check-expect (render G222)
              (render-invaders LOI2 (render-missiles LOM2 (render-tank T2 BACKGROUND))))
(check-expect (render G021)
              (render-invaders LOI0 (render-missiles LOM2 (render-tank T1 BACKGROUND))))

;(define (render g) BACKGROUND) ; stub

(define (render g)
  (render-invaders (game-invaders g)
                   (render-missiles (game-missiles g)
                                    (render-tank (game-tank g) BACKGROUND))))


;; Tank Image -> Image
;; render the tank on the provided image
(check-expect (render-tank T0 BACKGROUND)
              (place-image/align TANK (/ WIDTH 2) HEIGHT "center" "bottom" BACKGROUND))
(check-expect (render-tank T1 BACKGROUND)
              (place-image/align TANK 50 HEIGHT "center" "bottom" BACKGROUND))

;(define (render-tank tank i) i) ; stub

(define (render-tank tank i)
  (place-image/align TANK (tank-x tank) HEIGHT "center" "bottom" i))


;; ListOfMissile Image -> Image
;; render missiles on the provided image
(check-expect (render-missiles LOM0 BACKGROUND) BACKGROUND)
(check-expect (render-missiles LOM1 BACKGROUND)
              (render-missile M1 BACKGROUND))
(check-expect (render-missiles LOM3 BACKGROUND)
              (render-missile M1 (render-missile M2 (render-missile M3 BACKGROUND))))

;(define (render-missiles lom i) i) ; stub

(define (render-missiles lom i)
  (cond [(empty? lom) i]
        [else (render-missile (first lom)
                              (render-missiles (rest lom) i))]))


;; Missile Image -> Image
;; render a missile on the provided image
(check-expect (render-missile M1 BACKGROUND)
              (place-image MISSILE 150 300 BACKGROUND))

;(define (render-missile m i) i) ; stub

(define (render-missile m i)
  (place-image MISSILE (missile-x m) (missile-y m) i))


;; ListOfInvader Image -> Image
;; render invaders on the provided image
(check-expect (render-invaders LOI0 BACKGROUND) BACKGROUND)
(check-expect (render-invaders LOI1 BACKGROUND)
              (render-invader I1 BACKGROUND))
(check-expect (render-invaders LOI3 BACKGROUND)
              (render-invader I1 (render-invader I2 (render-invader I3 BACKGROUND))))

;(define (render-invaders loi i) i) ; stub

(define (render-invaders loi i)
  (cond [(empty? loi) i]
        [else (render-invader (first loi)
                              (render-invaders (rest loi) i))]))


;; Invader Image -> Image
;; render an invader on the provided image
(check-expect (render-invader I1 BACKGROUND)
              (place-image INVADER 150 100 BACKGROUND))

;(define (render-invader inv i) i) ; stub

(define (render-invader inv i)
  (place-image INVADER (invader-x inv) (invader-y inv) i))


;; Game -> Boolean
;; produce true if at least one missile reached the bottom, false otherwise
(check-expect (game-over? G000) false) ; no invaders at all
(check-expect (game-over? G111) false) ; no landed invaders
(check-expect (game-over? G221) true)  ; 1 landed invader

;(define (game-over? g) false) ; stub

(define (game-over? g)
  (has-landed-invaders? (game-invaders g)))


;; ListOfInvader -> Boolean
;; produce true if and only if at least 1 invader landed
(check-expect (has-landed-invaders? LOI0) false)
(check-expect (has-landed-invaders? LOI1) false)
(check-expect (has-landed-invaders? LOI2) true)
(check-expect (has-landed-invaders? LOI3) true)

;(define (has-landed-invaders? loi) false) ; stub

(define (has-landed-invaders? loi)
  (cond [(empty? loi) false]
        [else (if (landed? (first loi))
                  true
                  (has-landed-invaders? (rest loi)))]))


;; Invader -> Boolean
;; produce true if invaded has landed
(check-expect (landed? I1) false)
(check-expect (landed? I2) true)

;(define (landed? i) false) ; stub

(define (landed? i)
  (>= (invader-y i) HEIGHT))


;; Game KeyEvent -> Game
;; produce the next game state, depending on a key, pressed by the user
(check-expect (handle-key (make-game LOI2 (list M1 M2) T0) " ")
              (make-game LOI2
                         (list (make-missile (tank-x T0) (- HEIGHT (image-height TANK))) M1 M2)
                         T0))
(check-expect (handle-key (make-game LOI1 LOM1 T0) "left")
              (make-game LOI1 LOM1 (make-tank (tank-x T0) -1)))
(check-expect (handle-key (make-game LOI1 LOM1 T0) "right")
              (make-game LOI1 LOM1 (make-tank (tank-x T0) 1)))

;(define (handle-key g ke) g) ;stub

(define (handle-key g ke)
  (cond [(key=? ke " ")
         (make-game (game-invaders g)
                    (add-missile (game-missiles g)
                                 (tank-x (game-tank g))
                                 (- HEIGHT (image-height TANK)))
                    (game-tank g))]
        [(key=? ke "left")
         (make-game (game-invaders g)
                    (game-missiles g)
                    (make-tank (tank-x (game-tank g)) -1))]
        [(key=? ke "right")
         (make-game (game-invaders g)
                    (game-missiles g)
                    (make-tank (tank-x (game-tank g)) 1))]
        [else g]))

;; ListOfMissile x y -> ListOfMissile
;; adds a missile with (x,y) position to the list
(check-expect (add-missile empty 10 20) (list (make-missile 10 20)))
(check-expect (add-missile (list M1 M2) 10 20) (list (make-missile 10 20) M1 M2))

;(define (add-missile lom x y) lom) ; stub

(define (add-missile lom x y)
  (cons (make-missile x y) lom))