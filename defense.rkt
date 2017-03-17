#lang racket

(require "extras.rkt")
(require rackunit)
(require 2htdp/universe)
(require 2htdp/image) 
(require lang/posn)

(check-location "13" "defense.rkt")

;;===========================PROVIDE FUNCTIONS=================================

(provide Unit<%>)
(provide StatefulWorld<%>)
(provide mk-world)
(provide mk-ally)
(provide mk-enemy)
(provide mk-merc)

;;===============================CONSTANTS=====================================
(define CANVAS-WIDTH 400)
(define CANVAS-HEIGHT 500)
(define INITIAL-HEIGHT 50)
(define SAMPLE (rectangle 30 20 "solid" "white"))
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))
(define BASE
  (place-image 
   (rectangle CANVAS-WIDTH INITIAL-HEIGHT "solid" "yellow") 
   200 
   475 
   EMPTY-CANVAS))
(define AIMINGPOINT 
  (overlay/align 
   "middle" 
   "middle"
   (circle 10 "outline" "black")
   (circle 5 "outline" "black")
   (rectangle 5 20 "solid" "black")
   (rectangle 20 5 "solid" "black")))
(define SCORE-LABEL "Score: ")
(define SCORE-TXT-SIZE 10)
(define SCORE-TXT-COLOR "black")
(define SCORE-X-POS 200)
(define SCORE-Y-POS 475)
(define INIT-COUNTER-VALUE 1)
(define INIT-SCORE 0)
(define SCORE-DIV 5)
(define SCORE-ADD 50)
(define ADD-UNIT-TICK-COUNT 4)
(define MERC-SWITCH-TICK-COUNT 3)
(define ZERO 0)
(define TYPES-OF-UNITS 3)
(define ALLY-SIDE-HALF 10)
(define ALLY-SIDE 20)
(define ALLY-MODE "solid")
(define ALLY-COLOR "green")
(define ENEMY-RADIUS 12)
(define ENEMY-RADIUS-HALF 6)
(define ENEMY-MODE "solid")
(define ENEMY-COLOR "red")
(define BASE-INIT-HT 450)
(define ALLY-AT-BASE-SCORE 20)
(define ENEMY-AT-BASE-SCORE -40)
(define MERC-ALLY-AT-BASE-SCORE 60)
(define MERC-ENEMY-AT-BASE-SCORE -60)
(define ALLY-ELIMINATED-SCORE -20)
(define ENEMY-ELIMINATED-SCORE 40)
(define MERC-ALLY-ELIMINATED-SCORE -60)
(define MERC-ENEMY-ELIMINATED-SCORE 60)
(define ALLY-UNIT-TYPE 0)
(define ENEMY-UNIT-TYPE 1)
(define MERC-UNIT-TYPE 2)
(define UNIT-X-WINDOW 380)
(define WINDOW-OFFSET 10)
(define INIT-TARGET-LOC 200)
(define ALLYUNIT (square ALLY-SIDE ALLY-MODE ALLY-COLOR))
(define ENEMYUNIT (circle ENEMY-RADIUS ENEMY-MODE ENEMY-COLOR))

;;===========================DATA DEFINITIONS==================================

; A Velocity is a Natural, representing Pixels/tick in the downward direction.

; An Image is any value where applying image? from the 2htdp/image library to 
; that value returns true

; A Color represents a color recognized by the 2htdp/image library. 
; A string c is a Color if (image-color? c) is true.

; A PosInt is a positive integer starting from 1, 2, 3...

;-------------------------------------------------------------------------------

; A Unit is of 3 types:
; 1. AllyUnit: 
; Interp: Represented as a solid green square with a side of 20 pixels.
;         These are the reinforcements arriving to help.
;         While an Allyunit reaching a base adds to the game score, eliminating
;         Allyunits decreases the score

; 2. EnemyUnit: 
; Interp :  Represented as solid red circle with a radius of 12 pixels.
;           These are enemies attacking the base.
;           While eliminating Enemyunits adds to the game score,
;           Enemyunits reaching the base decreases the score
;
; 3. MercenaryUnit: 
; Interp :  These units switch sides from enemy to ally every 3 ticks.
;           When the game begins they behave and look like an AllyUnit.
;           On the third tick, they turn into an EnemyUnit.

;-------------------------------------------------------------------------------
; ListOf<Unit<%>>

; A ListOf<Unit<%>> is one of 
; -- empty                     
;            Interp: there are no units in the list
; -- (cons Unit<%> ListOf<Unit<%>>)
;            Interp: the ListOf<Unit<%>> consists of atleast one Unit<%>
;                    where Unit<%> refers to objects which implement the Unit<%>
;                    interface

; TEMPLATE:
; lstunit-fn: ListOf<Unit<%>> -> ??
;(define (lstunit-fn units)
;  (cond
;    [(empty? units) (...)]
;    [else (... 
;           (first units)
;           (lstunit-fn (rest units)))]))

;------------------------------------------------------------------------------

; ListOf<Subscriber<%>>

; A ListOf<Subscriber<%>> is one of 
; -- empty                     
;         Interp: there are no subscribers in the list
; -- (cons Subscriber<%> ListOf<Subscriber<%>>)
;      Interp: the ListOf<Subscriber<%>> consists of atleast one Subscriber<%>
;              where Subscriber<%> refers to objects which implement the  
;              Subscriber<%> interface

; TEMPLATE:
; lstsubscriber-fn: ListOf<Subscriber<%>> -> ??
;(define (lstsubscriber-fn subscribers)
;  (cond
;    [(empty? subscribers) (...)]
;    [else (... 
;           (first subscribers)
;           (lstsubscriber-fn (rest subscribers)))]))

;------------------------------------------------------------------------------

; A MouseEvent is one of
; following categories:
; -- "button-down"   (interp: maybe select the unit)
; -- "drag"          (interp: maybe drag the unit)
; -- "button-up"     (interp: unselect the unit)
; -- "move"          (interp: mouse movement in the universe)

; TEMPLATE:
; mev-fn : MouseEvent -> ??
;(define (mev-fn mev)
;  (cond
;    [(mouse=? mev "button-down") ...]
;    [(mouse=? mev "drag") ...]
;    [(mouse=? mev "button-up") ...]
;    [(mouse=? mev "move") ...]
;    [else ...]))

;------------------------------------------------------------------------------

; get-unit-fn : Natural -> Unit<%>
; This function returns one of the 3 types of units depending upon the 
; generated random number out of 0, 1 and 2 that is input to the function
; Strategy : Data Decomposition on rn : Natural

; Template :
; get-unit-fn : Natural -> Unit<%>
;(define (get-unit-fn rn)
;  (cond
;    [(equal? rn 0) ...]
;    [(equal? rn 1) ...]
;    [(equal? rn 2) ...]
;    ))

;-------------------------------------------------------------------------------

; unit-modification-fn : Image Image Image -> Image
; This function compares 2 unit images and switches from one unit to other and 
; vice-versa
; Strategy : data decomposition on unit : Image

; Template :
; unit-modification-fn : Image Image Image -> Image
;(define (unit-modification-fn unit img1 img2)
;  (cond
;    [(equal? unit img1) img2]
;    [(equal? unit img2) img1])) 



;;===============================INTERFACES====================================
; Represents a unit in the game.
(define Unit<%>
  (interface ()
    
    ; on-tick : -> MayBeUnit<%>
    ; This function updates a Unit<%> to the 
    ; state that it should be in after a tick
    on-tick
    
    ; unit-on-mouse : Coordinate Coordinate MouseEvent -> MayBeUnit<%>
    ; This function takes in the mouse co-ordinates and the mouse event
    ; and updates the state of a Unit<%> in the world after the 
    ; mouse event has occurred
    unit-on-mouse
    
    
    ; unit-selected? : Coordinate Coordinate -> Boolean
    ; This function takes in the coordinates of the mouse and returns true
    ; if they are within the bounds of a Unit<%>
    unit-selected?
    
    ; add-to-scene : Scene -> Scene
    ; returns a scene like the given one, but with a shape drawn 
    ; on it
    add-to-scene
    
    ; get-loc : -> posn
    ; Returns the location of this Unit<%> as a posn.
    get-loc
 
    ; get-color : -> Color
    ; Returns the color of this Unit<%>.
    get-color
    
    ; testing:get-x :-> Coordinate
    ; Returns the x-coordinate of a Unit<%>
    testing:get-x
    
    
    ; testing:get-y :-> Coordinate
    ; Returns the y-coordinate of a Unit<%>
    testing:get-y
      
    
    ; testing:get-speed :-> Coordinate
    ; Returns the speed of a Unit<%>
    testing:get-speed
    
    ))


; Represents a mutable world object.
(define StatefulWorld<%>
  (interface ()
    
    ; on-tick! : -> Void
    ; EFFECT: mutates this world to its next state.
    on-tick!
 
    ; on-mouse! : Coordinate Coordinate MouseEvent -> Void
    ; EFFECT: mutates this world to its next state from the given mouse 
    ; parameters.
    on-mouse!
 
    
    ; add-unit! : Unit<%> -> Void
    ; EFFECT: adds the given Unit<%> to the world
    add-unit!
    
    ; add-unit-and-subscribe-world-to-unit! : Unit<%> -> Void
    ; EFFECT: adds the given Unit<%> to the world and subscribes the world to 
    ; the added Unit<%>
    add-unit-and-subscribe-world-to-unit!
    
    ; add-unit-on-tick : -> ListOf<Unit<%>>
    ; This function generates a Unit<%> every four ticks and if it's
    ; not the fourth tick then it just returns the units already 
    ; present in the world
    add-unit-on-tick
 
    ; get-base-height : -> Natural
    ; Returns the height of the base, in pixels.
    get-base-height
    
    
    ; target-loc : -> posn
    ; Returns the center of the target as a posn.
    target-loc
 
    ; get-units : -> ListOf<Unit<%>>
    ; Returns the current units in this world.
    get-units
    
    
     ; testing:get-x :-> Coordinate
    ; Returns the x-coordinate of center of a Unit<%>
    testing:get-x
      
    
    ; testing:get-y :-> Coordinate
    ; Returns the y-coordinate of center of a Unit<%>
    testing:get-y
      
    
    ; testing:get-minvel :-> Velocity
    ; Returns the minvel of a Unit<%>
    testing:get-minvel
      
    
    ; testing:get-maxvel :-> Velocity
    ; Returns the maxvel of a Unit<%>
    testing:get-maxvel
    ))


(define Subscriber<%>
  (interface ()
    ; update-score! : Natural -> Void
    ; Updates the score of this subscriber.
    update-score!))


(define Publisher<%>
  (interface ()
    ; subscribe! : Subscriber<%> -> Void
    ; Subscribes to this publisher.
    subscribe!))



;;===============================CLASSES=======================================

; World% -- implements StatefulWorld<%> and Subscriber<%> interfaces

; A World% is a 

;(new World% 
;     [x Coordinate] 
;     [y Coordinate]
;     [minvel Velocity] 
;     [maxvel Velocity] 
;     [base-height Natural]
;     [units ListOf<Unit<%>>]
;     [counter Natural]
;     [score Integer])

; Interpretation:
; x -- x represents the x-coordinate of the centre of the target which mirrors 
;      the movement of the mouse in a world
; y -- y represents the y-coordinate of the centre of the target which mirrors 
;      the movement of the mouse in a world
; minvel -- represents the minimum velocity with which the units present
;           in a world move
; maxvel -- represents the maximum velocity with which the units present
;           in a world move
; base-height -- represents the height of the base in the game
; units -- represents the list of all units such as AllyUnits,EnemyUnits and
;          MercenaryUnits in a world
; counter -- represents the number of ticks that has elapsed since the start
;            of the simulation
; score -- represents the accumulated score achieved by elimination of 
;          the units present in a world and when the units reach or cross the
;          base-height in the game



(define World%
  (class* object% (StatefulWorld<%> Subscriber<%>)
    (init-field x
                y
                minvel
                maxvel
                base-height
                units
                counter)
    (init-field [score INIT-SCORE])
    
    (super-new)
    
    
    ; on-tick : -> Void
    ; EFFECT: This function updates the StatefulWorld<%> as it should be after
    ; a tick
    ; EXAMPLES:  (on-tick!) -> Void
    (define/public (on-tick!)
      (increment-counter!)
      (update-base-height!)
      (add-unit-on-tick)
      (process-units!))
    
    
    ; increment-counter! : -> Void
    ; EFFECT: This function updates the counter field of the world after every
    ; tick
    (define (increment-counter!)
      (set! counter (add1 counter)))
    
 
    ; update-base-height! : -> Void
    ; EFFECT: This function updates the base-height field of the world as and 
    ; when the score field is updated
    (define (update-base-height!)
      (set! base-height (+ (/ score SCORE-DIV) SCORE-ADD)))
    
    
    ; add-unit-on-tick : -> ListOf<Unit<%>>
    ; This function generates a Unit<%> every four ticks and if it's
    ; not the fourth tick then it just returns the units already 
    ; present in the world
    (define/public (add-unit-on-tick)
      (if (= (remainder counter ADD-UNIT-TICK-COUNT) ZERO)
          (add-unit-and-subscribe-world-to-unit! 
           (get-unit (random TYPES-OF-UNITS) minvel maxvel))
          units))
    
    ; process-units! : -> Void
    ; EFFECT: This function updates the units of the world to a filtered list of
    ; units that have'nt been eliminated from the world and updates each Unit<%>
    ; to its expected state after tick
   (define (process-units!)
     (set! units 
           (filter (lambda(x) (not (false? x))) 
                   (map(lambda (a) (send a on-tick)) units))))
    
  
    ; on-mouse! : Coordinate Coordinate MouseEvent -> Void
    ; EFFECT: This function takes in the co-ordinates of the mouse and the 
    ; mouse event and updates world as it should be after the mouse event has 
    ; occurred
    ; Example: (on-mouse! 10 10 "button-down") -> Void
    ; Strategy : Data Decomposition on mev : MouseEvent
    (define/public (on-mouse! mouse-x-coord mouse-y-coord mev) 
      (cond
        [(mouse=? mev "button-down")
         (world-after-button-down mouse-x-coord mouse-y-coord)]
        [else (world-after-move! mouse-x-coord mouse-y-coord)]))
    
    
    
    ; world-after-button-down : Coordinate Coordinate -> StatefulWorld<%>
    ; This function takes in the current coordinates of the mouse and checks if
    ; the coordinates of mouse are within any of the world units and if it's so,
    ; returns a world with a particular Unit<%> selected and consequently 
    ; removed after a button-down event is done on the Unit<%>
    ; Example :  (world-after-button-down 10 10) -> Void
    (define (world-after-button-down mx my)    
      (if (unit-clicked? mx my)
          (selected-units! mx my)
          this)) 
    
    
    ; world-after-move! : Coordinate Coordinate -> Void
    ; EFFECT: This function takes in the co-ordinates of the current 
    ; location of mouse on the canvas and updates target coordinates to 
    ; mouse coordinates
    (define (world-after-move! mx my)
      (set! x mx)
      (set! y my))
    
     
 
    ; unit-clicked? : Coordinate Coordinate -> Boolean
    ; This function takes in the current coordinates of the mouse and checks 
    ; if the coordinates are within the coordinates of a given Unit<%> and 
    ; if so, returns true
    ; Example : (unit-clicked? 10 10) -> true 
    (define (unit-clicked? mx my) 
      (ormap
       ; Unit<%> -> Boolean
       ; Returns true or false if a particular unit is selected
       (lambda(t)
         (send t unit-selected? mx my)) units))

    
    ; selected-units! : Coordinate Coordinate -> Void
    ; EFFECT: This function updates the units of the world to a filtered list of
    ; units that have'nt been eliminated from the world and updates each 
    ; selected Unit<%> to its expected state after a mouse button-down event
    (define (selected-units! mx my)
      (set! units (filter (lambda(x) (not (false? x))) 
                          (map (lambda(e) 
                                 (send e unit-on-mouse mx my "button-down")) 
                               units))))
    
    
    ; add-unit-and-subscribe-world-to-unit! : Unit<%> -> Void
    ; EFFECT: adds the given Unit<%> to the world and subscribes the current
    ; world to the added Unit<%>
    (define/public (add-unit-and-subscribe-world-to-unit! u)
      (send u subscribe! this)
      (set! units (cons u units)))
  
    ; on-draw -> Scene
    ; This function returns a scene with all the units and the base area
    ; painted on it
    ; Example :  (on-draw) -> Scene
    (define/public (on-draw)
      (place-image 
       AIMINGPOINT
       x 
       y
       (place-image 
        (text 
         (string-append SCORE-LABEL (number->string score))
         SCORE-TXT-SIZE SCORE-TXT-COLOR)
        SCORE-X-POS 
        SCORE-Y-POS
        (foldr
         ; Unit<%> Scene -> Scene
         ; Returns a scene with the unit painted on it
         (lambda (a scene)
           (send a add-to-scene scene))
         (place-image SAMPLE x y BASE)
         units))))
    
    
    ; update-score! : Natural -> Void
    ; This function updates the world score field with the given new score
    (define/public (update-score! new-score)
      (set! score (+ score new-score)))
    
    
    ; target-loc : -> posn
    ; Returns the center of the target as a posn.
    ; Example : (target-loc) -> (make-posn 10 10)
    (define/public (target-loc)
      (make-posn x y))
    
    ; get-units : -> ListOf<Unit<%>>
    ; Returns the current units present in the world.
    ; Example :  (get-units) -> ListOf<Unit<%>>
    (define/public (get-units)
      units) 
    
    ; add-unit! : Unit<%> -> Void
    ; This function takes in a Unit<%> and adds the given unit to the world
    ; Example :  (add-unit!) -> Void
     (define/public (add-unit! u)
      (set! units (cons u units)))
   
    
    ; get-base-height : -> Natural
    ; Returns the height of the base, in pixels.
    ; Example :  (get-base-height) -> Natural
    (define/public (get-base-height)
      base-height)
    
    ; testing:get-x :-> Coordinate
    ; Returns the x-coordinate of center of a Unit<%>
    (define/public (testing:get-x)
      x)
    
    ; testing:get-y :-> Coordinate
    ; Returns the y-coordinate of center of a Unit<%>
    (define/public (testing:get-y)
      y)
    
    ; testing:get-minvel :-> Velocity
    ; Returns the minvel of a Unit<%>
    (define/public (testing:get-minvel)
      minvel)
    
    ; testing:get-maxvel :-> Velocity
    ; Returns the maxvel of a Unit<%>
    (define/public (testing:get-maxvel)
      maxvel)
    
    )) 

;;=============================================================================

; Allyunit% implements Unit<%> and Publisher<%> interfaces
;
; An Allyunit% is a 
;(new Allyunit% 
;     [x PosInt] 
;     [y PosInt] 
;     [speed Velocity]
;     [color Color])

; Interpretation:
; x - represents the x coordinate of the center of an Allyunit in the world
; y - represents the y coordinate of the center of an Allyunit in the world
; speed - represents the speed with which an Allyunit is moving in the 
;         downward or increasing-y direction in the world
; color - represents the color of an Allyunit in the world

(define Allyunit%
  (class* object% (Unit<%> Publisher<%>)
    (init-field x
                y
                speed
                color)
    
    (super-new)
    
   
    (define worlds empty) ; ListOf<Subscriber<%>>
    
    
    ; subscribe! : StatefulWorld<%> -> Void
    ; EFFECT: 
    ; This function updates the Subscriber List by adding the given 
    ; StatefulWorld<%> to the subscriber list
    (define/public (subscribe! w)
      (set! worlds (cons w worlds)))
    
    
    
    ; publish : Integer -> Void
    ; This function takes in the new score and updates the World Score 
    ; accordingly
    (define (publish x)
      (for-each 
       ; StatefulWorld<%> -> Void
       ; publishes the new score to every subscriber
       (lambda (w) (send w update-score! x)) worlds))
    
    
    ; on-tick : -> MayBeUnit<%>
    ; This function updates this Unit<%> to the state that it should be in 
    ; after the tick
    ; Example :  (on-tick) -> Unit<%>
    (define/public (on-tick)
      (if (ally-reaches-base?) 
          (ally-on-reaching-base)
          (begin 
            (ally-after-tick!) 
            this)))
 
    ; ally-reaches-base? : -> Boolean
    ; This function returns true if this Unit<%> touches or crosses the base 
    ; area
    ; Example :  (ally-reaches-base?) -> true
    (define (ally-reaches-base?)
      (>= (+ speed ALLY-SIDE-HALF y) BASE-INIT-HT))
    
    
    ; ally-on-reaching-base : -> False
    ; This function publishes the specified score to the world and returns 
    ; false 
    ; Example : (ally-on-reaching-base) -> Void 
    (define (ally-on-reaching-base)
      (publish ALLY-AT-BASE-SCORE)   
      false)
    
    ; ally-after-tick! : -> Void
    ; EFFECT :
    ; This function updates the y-coordinate of center of this Unit<%> by adding
    ; speed to it
    ; Example :(ally-after-tick) -> Void 
    (define (ally-after-tick!)      
      (set! y (+ y speed)))
 
    ; unit-on-mouse : Coordinate  Coordinate Mouseevent -> MayBeUnit<%>
    ; This function takes in the current mouse co-ordinates and the mouse event
    ; and updates the state of this Unit<%> in the world after the mouse event
    ; has occured.
    ; Example :  (unit-on-mouse 10 10 "button-down") -> Unit<%>
    ; STRATEGY: data decomposition on mev : Mouseevent
    (define/public (unit-on-mouse mx my mev) 
        (cond 
        [(mouse=? mev "button-down") (unit-on-button-down mx my)]
        [else this]))
    
    ; unit-on-button-down : Coordinate Coordinate -> MayBeUnit<%>
    ; This function takes in the current mouse coordinates and checks whether
    ; the given coordinates are within this Unit<%> and if so then updates 
    ; this Unit<%> as it should be after the button-down event
    ; Example : (unit-on-button-down 10 10) -> Unit<%>
    (define (unit-on-button-down mx my)
      (if (unit-selected? mx my)
          (begin
            (publish ALLY-ELIMINATED-SCORE)
            false)
          this))
    
    
    ; unit-selected? : Coordinate Coordinate -> Boolean
    ; This function takes in the coordinates of the mouse and returns true
    ; if they are within the bounds of this Unit<%>
    ; Example :  (unit-selected? 10 10) -> true
    (define/public (unit-selected? mx my)
      (and 
       (and (<= (- x ALLY-SIDE-HALF) mx) (>= (+ x ALLY-SIDE-HALF) mx))
       (and (<= (- y ALLY-SIDE-HALF) my) (>= (+ y ALLY-SIDE-HALF) my)))) 
        
    
    ; add-to-scene : Scene -> Scene
    ; This function takes in a scene and paints the given Unit<%> 
    ; upon it
    ; Example : (add-to-scene Canvas) -> Scene
    (define/public (add-to-scene scene)
      (place-image ALLYUNIT x y scene))
    
    
    ; get-loc : -> posn
    ; Returns the location of this Unit<%> as a posn.
    ; Example : (get-loc) -> (make-posn 10 10)
    (define/public (get-loc)
      (make-posn x y))
    
    
    ; get-color : -> Color
    ; Returns the color of this Unit<%>.
    ; Example : (get-color) -> green
    (define/public (get-color)
      color) 
    
    ; testing:get-x :-> Coordinate
    ; Returns the x-coordinate of center of this Unit<%>
    (define/public (testing:get-x)
      x)
    
    ; testing:get-y :-> Coordinate
    ; Returns the y-coordinate of center of this Unit<%>
    (define/public (testing:get-y)
      y)
    
    ; testing:get-speed :-> Coordinate
    ; Returns the speed of this Unit<%>
    (define/public (testing:get-speed)
      speed)
    
    ))

;;=============================================================================

; Enemyunit% implements Unit<%> and Publisher<%> interfaces
;
; An Enemyunit% is a 
;(new Enemyunit% 
;     [x PosInt] 
;     [y PosInt] 
;     [speed Velocity]
;     [color Color])

; Interpretation:
; x - represents the x coordinate of the center of an Enemyunit in the world
; y - represents the y coordinate of the center of an Enemyunit in the world
; speed - represents the speed with which an Enemyunit is moving in the 
;         downward or increasing-y direction in the world
; color - represents the color of an Enemyunit in the world

(define Enemyunit%
  (class* object% (Unit<%> Publisher<%>)
    (init-field x
                y
                speed
                color) 
   
    (super-new)
    
    (define worlds empty) ; ListOf<Subscriber<%>>
    
   
    ; subscribe! : StatefulWorld<%> -> Void
    ; EFFECT: 
    ; This function updates the Subscriber List by adding the given 
    ; StatefulWorld<%> to the subscriber list
    (define/public (subscribe! w)
      (set! worlds (cons w worlds)))
    
   
    ; publish : Integer -> Void
    ; This function takes in the new score and updates the World Score 
    ; accordingly
    (define (publish x)
      (for-each 
       ; StatefulWorld<%> -> Void
       ; publishes the new score to every subscriber
       (lambda (w) (send w update-score! x)) worlds))
    
    
    ; on-tick : ->  MayBeUnit<%>
    ; This function updates this Unit<%> to the 
    ; state that it should be in after the tick
    ; Example : (on-tick!) -> Void
    (define/public (on-tick)
      (if (enemy-reaches-base?) 
          (enemy-on-reaching-base) 
          (begin 
            (enemy-after-tick!)
            this)))
    
  
    ; enemy-reaches-base? : -> Boolean
    ; This function returns true if this Unit<%> touches the wall
    ; Example : (enemy-reaches-base?) -> true 
    (define (enemy-reaches-base?) 
      (>= (+ speed ENEMY-RADIUS-HALF y) BASE-INIT-HT))
    
    
    ; enemy-on-reaching-base : -> False
    ; This function publishes the specified score to the world and returns 
    ; false  
    ; Example :  (enemy-on-reaching-base) -> Void 
    (define (enemy-on-reaching-base)
      (begin
        (publish ENEMY-AT-BASE-SCORE)  
        false))
    
           
   ; enemy-after-tick! : -> Void
   ; EFFECT:
   ; This function updates the y-coordinate of center of this Unit<%> by adding
   ; speed to it
   ; Example :  (enemy-after-tick) -> Void 
   (define (enemy-after-tick!)
    (set! y (+ y speed)))
        
    
    ; unit-on-mouse: Integer Integer Mouseevent ->  MayBeUnit<%>
    ; This function takes in the current mouse co-ordinates and the mouse event
    ; and updates the state of this Unit<%> in the world after the 
    ; mouse event has occured.
    ; Example :  (unit-on-mouse 10 10 "button-down") -> Unit<%>
    ; STRATEGY: data decomposition on mev : Mouseevent
    (define/public (unit-on-mouse mx my mev) 
        (cond 
        [(mouse=? mev "button-down") (unit-on-button-down mx my)]
        [else this]))
    
    
     
    ; unit-on-button-down : Coordinate Coordinate ->  MayBeUnit<%>
    ; This function takes in the current mouse coordinates and checks whether
    ; the given coordinates are within the unit and if so then updates 
    ; the unit as it should be after the button-down event
    ; Example :  (unit-on-button-down 10 10) ->  Unit<%>
    (define (unit-on-button-down mx my)
      (if (unit-selected? mx my)
          (begin
            (publish ENEMY-ELIMINATED-SCORE)
            false)
          this))
    
    
    
    ; unit-selected? : Coordinate Coordinate -> Boolean
    ; This function takes in the coordinates of the mouse and returns true
    ; if they are within the bounds of this Unit<%>
    ; Example :  (unit-selected? 10 10) -> true
    (define/public (unit-selected? mx my)
      (and (< (abs (- x mx)) ENEMY-RADIUS) (< (abs (- y my)) ENEMY-RADIUS)))
    
    
    
    ; add-to-scene : Scene -> Scene
    ; This function takes in a scene and paints the given Unit<%>
    ; upon it
    ; Example :  (add-to-scene Canvas) -> Scene
    (define/public (add-to-scene scene)
       (place-image ENEMYUNIT x y scene))
    
    
    ; get-loc : -> posn
    ; Returns the location of this Unit<%> as a posn.
    ; Example : (get-loc) -> (make-posn 10 10)
    (define/public (get-loc)
      (make-posn x y))
    
    
    ; get-color : -> Color
    ; Returns the color of this Unit<%>.
    ; Example : (get-color) -> red
    (define/public (get-color)
      color)  
    
    ; testing:get-x :-> Coordinate
    ; Returns the x-coordinate of center of this Unit<%>
    (define/public (testing:get-x)
      x)
    
    ; testing:get-y :-> Coordinate
    ; Returns the y-coordinate of center of this Unit<%>
    (define/public (testing:get-y)
      y)
    
    ; testing:get-speed :-> Coordinate
    ; Returns the speed of this Unit<%>
    (define/public (testing:get-speed)
      speed)
    
    ))

;;=============================================================================
1
; MercenaryUnit% implements Unit<%> and Publisher<%> interfaces
;
; A  MercenaryUnit% is a 
;(new MercenaryUnit% 
;     [x PosInt] 
;     [y PosInt] 
;     [speed Velocity]
;     [color Color])
;     [unit Image]
;     [counter Natural])

; Interpretation:
; x -- represents the x coordinate of the center of a Mercenaryunit in the world
; y -- represents the y coordinate of the center of a Mercenaryunit in the
;      world
; speed -- represents the speed with which a Mercenaryunit is moving in the 
;         downward or increasing-y direction in the world
; color -- represents the color of a Mercenaryunit in the world
; unit --  represents either an Allyunit or an Enemyunit
; counter -- represents the number of ticks that has elapsed since the start
;            of the simulation

(define MercenaryUnit%
  (class* object% (Unit<%> Publisher<%>)
    (init-field x
                y
                speed
                color
                unit
                counter)
    
    (super-new)
 
    
    (define worlds empty) ; ListOf<Subscriber<%>>
    
    ; subscribe! : StatefulWorld<%> -> Void
    ; EFFECT: 
    ; This function updates the Subscriber List by adding the given 
    ; StatefulWorld<%> to the subscriber list
    (define/public (subscribe! w)
      (set! worlds (cons w worlds)))
    
    
    ; publish : Integer -> Void
    ; This function takes in the new score and updates the World Score 
    ; accordingly
    (define (publish x)
      (for-each 
       ; StatefulWorld<%> -> Void
       ; publishes the new score to every subscriber 
       (lambda (w) (send w update-score! x)) worlds))
    
    
    ; on-tick : -> MayBeUnit<%>
    ; This function updates this Unit<%> to the state that it should be in 
    ; after the tick
    ; Example :  (on-tick!) -> Unit<%>
    (define/public (on-tick)
      (begin
        (counter-update!)
        (unit-update!)
     (merc-tick-behaviour))) 
    
   
    ; counter-update! : -> Void
    ; EFFECT: This function updates the counter 
    (define (counter-update!)
      (set! counter (add1 counter)))
    
    
    ; unit-update!: -> Void
    ; EFFECT :
    ; This function updates this Unit<%> according to the number of ticks
    ; that have passed
    (define (unit-update!)
      (set! unit (unit-switch)))
    
    
    ; merc-tick-behaviour : -> MayBeUnit<%>
    ; This function updates this Unit<%> in the world after every tick
    ; Example :  (merc-tick-behaviour) -> Unit<%>
   (define (merc-tick-behaviour)
     (if (merc-reaches-base?) 
          (merc-on-reaching-base)
          (begin 
            (merc-after-tick!)
           this)))
    
   ; merc-reaches-base? : -> Boolean
   ; This function returns true if this Unit<%> touches or crosses the base area
   ; Example :  (merc-reaches-base?) -> true
   (define (merc-reaches-base?)
     (>= (+ speed ENEMY-RADIUS-HALF y) BASE-INIT-HT))
    
    
    ; merc-on-reaching-base : -> False
    ; This function publishes the specified score to the world and returns 
    ; false   
    ; Example :  (merc-on-reaching-base) -> Unit<%>
    (define (merc-on-reaching-base)  
      (begin
        (if (equal? unit ALLYUNIT)
            (publish MERC-ALLY-AT-BASE-SCORE)
            (publish MERC-ENEMY-AT-BASE-SCORE))
        false))
    
           
   ; merc-after-tick! : -> Void
   ; EFFECT :
   ; This function updates the y-coordinate of center of this Unit<%> by adding
   ; speed to it
   ; Example :  (merc-after-tick) -> Void      
    (define (merc-after-tick!) 
      (set! y (+ y speed)))
    
     
    ; unit-switch : -> Unit<%>
    ; This function switches this Unit<%> between AllyUnit and EnemyUnit
    ; every 3 ticks
    ; Example : (unit-switch) -> Unit<%>
    (define (unit-switch)
      (if (= ZERO (remainder counter MERC-SWITCH-TICK-COUNT))
          (unit-modification)
          unit))
    
    
    ; unit-modification : -> Image
    ; This function switches this Unit<%> from AllyUnit to EnemyUnit and 
    ; vice-versa
    ; Example :  (unit-modification) -> ALLYUNIT
    ; Strategy : data decomposition on unit: Image
    (define (unit-modification)
      (cond
        [(equal? unit ALLYUNIT) ENEMYUNIT]
        [(equal? unit ENEMYUNIT) ALLYUNIT])) 
    
    
    ; unit-on-mouse : Coordinate  Coordinate Mouseevent -> MayBeUnit<%>
    ; This function takes in the current mouse co-ordinates and the mouse event
    ; and updates the state of this Unit<%> in the world after the 
    ; mouse event has occured.
    ; Example : (unit-on-mouse 10 10 "button-down") -> Unit<%>
    ; STRATEGY: data decomposition on mev : Mouseevent
    (define/public (unit-on-mouse mx my mev) 
        (cond 
        [(mouse=? mev "button-down") (unit-on-button-down mx my)]
        [else this]))
    
     
    ; unit-on-button-down : Coordinate Coordinate -> MayBeUnit<%>
    ; This function takes in the current mouse coordinates and checks whether
    ; the given coordinates are within this Unit<%> and if so then updates 
    ; this Unit<%> as it should be after the button-down event
    ; Example : (unit-on-button-down 10 10) -> Unit<%>
    (define (unit-on-button-down mx my)
      (if  (unit-selected? mx my)
          (begin
            (if (equal? unit ALLYUNIT)
                (publish MERC-ALLY-ELIMINATED-SCORE)
                (publish MERC-ENEMY-ELIMINATED-SCORE))
            false)
         this))
    
    
    ; unit-selected? : Coordinate Coordinate -> Boolean
    ; This function takes in the coordinates of the mouse and returns true
    ; if they are within the bounds of this Unit<%>
    ; Example : (unit-selected? 10 10) -> true
    (define/public (unit-selected? mx my)
      (and (< (abs (- x mx)) ENEMY-RADIUS) (< (abs (- y my)) ENEMY-RADIUS)))
    
    
    ; add-to-scene : Scene -> Scene
    ; This function takes in a scene and paints the given Unit<%>
    ; upon it
    ; Example : (add-to-scene Canvas) -> Scene
    (define/public (add-to-scene scene)
      (if (equal? unit ALLYUNIT)
          (place-image ALLYUNIT x y scene)
          (place-image ENEMYUNIT x y scene)))
    
    
    ; get-loc : -> posn
    ; Returns the location of this Unit<%> as a posn.
    ; Example :  (get-loc) -> (make-posn 10 10)
    (define/public (get-loc)
      (make-posn x y))
    
 
    ; get-color : -> Color
    ; Returns the color of this Unit<%>.
    ; Example :  (get-color) -> green
    (define/public (get-color)
      color)
    
    ; testing:get-x :-> Coordinate
    ; Returns the x-coordinate of center of this Unit<%>
    (define/public (testing:get-x)
      x)
    
    ; testing:get-y :-> Coordinate
    ; Returns the y-coordinate of center of this Unit<%>
    (define/public (testing:get-y)
      y)
    
    ; testing:get-speed :-> Coordinate
    ; Returns the speed of this Unit<%>
    (define/public (testing:get-speed)
      speed)
      ))

;;=============================================================================

; mk-ally : posn Velocity -> Unit<%>
; Creates an ally unit with the given parameters.
; Example :  (mk-ally (make-posn 10 10) 4)
; -> AllyUnit%

(define (mk-ally posn vel)
  (new Allyunit% 
       [x (posn-x posn)] 
       [y (posn-y posn)] 
       [speed vel]
       [color color]))
       
;;=============================================================================

; mk-enemy : posn Velocity -> Unit<%>
; Creates an enemy unit with the given parameters.
; Example : (mk-enemy (make-posn 20 20) 4) -> Enemyunit%

(define (mk-enemy posn vel)
  (new Enemyunit% 
       [x (posn-x posn)] 
       [y (posn-y posn)]
       [speed vel] 
       [color color]))

;;=============================================================================

; mk-merc : posn Velocity -> Unit<%>
; Creates a mercenary unit with the given parameters.
; Example : (mk-merc (make-posn 10 10) 5) -> MercenaryUnit%

(define (mk-merc posn vel)
  (new MercenaryUnit% 
       [x (posn-x posn)] 
       [y (posn-y posn)]
       [speed vel] 
       [color color] 
       [unit ALLYUNIT] 
       [counter INIT-COUNTER-VALUE]))

;;=============================================================================

; get-unit : Natural Velocity Velocity -> Unit<%>
; This function returns one of Allyunit%, Enemyunit% or Mercenaryunit%  
; depending upon the generated random number out of 0, 1 and 2
; Example : (get-unit 0 4 5) -> Allyunit%
; Strategy : Data Decomposition on rn : Natural

 (define (get-unit rn minvel maxvel)
      (cond
        [(equal? rn ALLY-UNIT-TYPE) 
         (mk-ally (make-posn (+ (random UNIT-X-WINDOW) WINDOW-OFFSET) ZERO) 
                                (+ (random (- maxvel minvel)) minvel))]
        [(equal? rn ENEMY-UNIT-TYPE) 
         (mk-enemy (make-posn (+ (random UNIT-X-WINDOW) WINDOW-OFFSET) ZERO) 
                                 (+ (random (- maxvel minvel)) minvel))]
        [(equal? rn MERC-UNIT-TYPE) 
         (mk-merc (make-posn (+ (random UNIT-X-WINDOW) WINDOW-OFFSET) ZERO) 
                                (+ (random (- maxvel minvel)) minvel))]
        ))

;;=============================================================================

; mk-world : Velocity Velocity Natural -> StatefulWorld<%>
; Creates a world with num-units initial random units,
; where units have the specified min and max velocity.
; WHERE: minvel <= maxvel
; Example : (mk-world 4 5 1) -> World%

(define (mk-world minvel maxvel num-units)
  (define empty-world (new World%
       [x INIT-TARGET-LOC]
       [y INIT-TARGET-LOC]
       [units empty]
       [score ZERO]
       [minvel minvel]
       [maxvel maxvel] 
       [base-height BASE-INIT-HT]
       [counter INIT-COUNTER-VALUE]
       ))
   (map (; Unit<%> -> Void
         ; The generated unit is added
         lambda (x) 
          (send empty-world add-unit-and-subscribe-world-to-unit! 
                (get-unit x minvel maxvel))) 
              (make-list num-units (random TYPES-OF-UNITS)))
  empty-world)
 
;;=============================================================================

; run : PosNum Velocity Velocity PosInt -> StatefulWorld<%> 
; This function takes in a frame rate (in seconds/tick) and a minimum velocity
; (in pixels/tick),minimum velocity(in pixels/tick) and numunits and creates 
; and runs a world and returns the final state of the world with the units
; of the world moving down

(define (run frame-rate minvel maxvel num-units)
 (big-bang  
   (mk-world minvel maxvel num-units)
            (on-tick
             ; World -> Void
             ; Returns world after tick
             (lambda (w) (send w on-tick!) w)
             frame-rate)
            
            (on-draw
             ; World-> Scene
             ; Takes an empty world and returns scene with random units 
             ; generated
             (lambda (w) (send w on-draw)))
            
            (on-mouse
             ; World Integer Integer MouseEvent->Void
             ; Returns the world after the mouseevent has taken place
             (lambda (w x y evt) (send w on-mouse! x y evt) w))))

;;=============================================================================

;; TESTCASES:-

; ally-equal? : Allyunit% Allyunit% -> Boolean
; This function takes in two ally units and returns a boolean stating whether
; both of them are equal
; Example :
; (define (ally-equal? (new Allyunit% [x 10] [y 10] [color "green"] [speed 8])
; (new Allyunit% [x 10] [y 10] [color "green"] [speed 8]) -> true
                     
(define (ally-equal? a1 a2)
  (and
   (equal? (send a1 testing:get-x) (send a2 testing:get-x))
   (equal? (send a1 testing:get-y) (send a2 testing:get-y))
   (equal? (send a1 testing:get-speed) (send a2 testing:get-speed))
   ))

(begin-for-test
  (local
    ((define a1 (new Allyunit% [x 10] [y 10] [color "green"] [speed 8]
                     ))
     (define a2 (send a1 unit-on-mouse 500 500 "button-down")))
    (check ally-equal? a2 (new Allyunit% [x 10] [y 10] [color "green"] 
                               [speed 8]
                     ))))

(begin-for-test
  (local
    ((define a1 (new Allyunit% [x 10] [y 10] [color "green"] [speed 8]
                     ))
     (define a2 (send a1 unit-on-mouse 500 500 "button-up")))
    (check ally-equal? a2 (new Allyunit% [x 10] [y 10] [color "green"] 
                               [speed 8]
                     ))))

(begin-for-test
  (local
    ((define a1 (new Allyunit% [x 10] [y 10] [color "green"] [speed 8]
                     ))
     (define a2 (send a1 unit-on-mouse 10 10 "button-down")))
    (check-equal? a2 #f)))
(begin-for-test
  (local
    ((define a1 (new Allyunit% [x 452] [y 452] [color "green"] [speed 8]
                      ))
     (define a2 (send a1 on-tick)))
    (check-equal? a2 false)))

(begin-for-test
  (local
    ((define a1 (new Allyunit% [x 50] [y 50] [color "green"] [speed 8]
                      ))
     (define a2 (send a1 on-tick)))
    (check ally-equal? a2 (new Allyunit% [x 50] [y 58] [color "green"] [speed 8]
                      ))))

(begin-for-test
  (local
    ((define a1 (new Allyunit% [x 452] [y 452] [color "green"] [speed 8]
                   ))
     (define a2 (send a1 add-to-scene EMPTY-CANVAS)))
    (check-true (image? a2) true)))

(begin-for-test
  (local
    ((define a1 (new Allyunit% [x 452] [y 452] [color "green"] [speed 8]
                   ))
     (define a2 (send a1 get-loc)))
    (check-equal? a2 (make-posn 452 452))))

(begin-for-test
  (local
    ((define a1 (new Allyunit% [x 452] [y 452] [color "green"] [speed 8]
                   ))
     (define a2 (send a1 subscribe! a1)))
    (check-equal? a2 (void a2))))

(begin-for-test
  (local
    ((define w1 (mk-ally (make-posn 10 10) 4)))
    (check ally-equal? w1 (new Allyunit% [x 10] [y 10]
                                    [speed 4][color "green"] 
                                    ))))

(begin-for-test
  (local
    ((define a1 (new Allyunit% [x 10] [y 10]
                                    [speed 4][color "green"] 
                                    ))
     (define a2 (send a1 get-color)))
    (check-equal? a2 "green")))

;;=============================================================================

; enemy-equal? : Enemyunit% Enemyunit% -> Boolean
; This function takes in two enemy units and returns a boolean stating whether
; both of them are equal
; Example :
; (define (enemy-equal? (new Enemyunit% [x 10] [y 10] [color "green"] [speed 8])
; (new Enemyunit% [x 10] [y 10] [color "green"] [speed 8]) -> true

(define (enemy-equal? e1 e2)
  (and
   (equal? (send e1 testing:get-x) (send e2 testing:get-x))
   (equal? (send e1 testing:get-y) (send e2 testing:get-y))
   (equal? (send e1 testing:get-speed) (send e2 testing:get-speed))
   ))

(begin-for-test
  (local
    ((define a1 (new Enemyunit% [x 10] [y 10] [color "green"] [speed 8]
                     ))
     (define a2 (send a1 unit-on-mouse 10 10 "button-up")))
    (check enemy-equal? a2 (new Enemyunit% [x 10] [y 10] [color "green"] 
                               [speed 8]
                     ))))

(begin-for-test
  (local
    ((define a1 (new Enemyunit% [x 10] [y 10] [color "green"] [speed 8]
                     ))
     (define a2 (send a1 unit-on-mouse 10 10 "button-down")))
    (check-equal? a2 #f)))

(begin-for-test
  (local
    ((define a1 (new Enemyunit% [x 10] [y 10] [color "green"] [speed 8]
                     ))
     (define a2 (send a1 unit-on-mouse 500 500 "button-down")))
    (check enemy-equal? a2 (new Enemyunit% [x 10] [y 10] [color "green"] 
                               [speed 8]
                     ))))

(begin-for-test
  (local
    ((define a1 (new Enemyunit% [x 50] [y 50] [color "green"] [speed 8]
                      ))
     (define a2 (send a1 on-tick)))
    (check ally-equal? a2 (new Enemyunit% [x 50] [y 58] [color "green"] 
                               [speed 8]
                      ))))

(begin-for-test
  (check-equal? (equal?(send (get-unit 0 5 10) get-color)
                       (send (get-unit 0 5 10) get-color))
                #t))

(begin-for-test
  (check-equal? (equal?(send (get-unit 1 5 10) get-color)
                       (send (get-unit 1 5 10) get-color))
                #t))

(begin-for-test
  (check-equal? (equal?(send (get-unit 2 5 10) get-color)
                       (send (get-unit 2 5 10) get-color))
                #t))

(begin-for-test
  (local
    ((define a1 (new Enemyunit% [x 452] [y 452] [color "green"] [speed 8]
                      ))
     (define a2 (send a1 on-tick)))
    (check-equal? a2 false)))

(begin-for-test
  (local
    ((define a1 (new Enemyunit% [x 452] [y 452] [color "green"] [speed 8]
                   ))
     (define a2 (send a1 add-to-scene EMPTY-CANVAS)))
    (check-true (image? a2) true)))

(begin-for-test
  (local
    ((define a1 (new Enemyunit% [x 452] [y 452] [color "green"] [speed 8]
                   ))
     (define a2 (send a1 get-loc)))
    (check-equal? a2 (make-posn 452 452))))

(begin-for-test
  (local
    ((define a1 (new Enemyunit% [x 452] [y 452] [color "green"] [speed 8]
                   ))
     (define a2 (send a1 subscribe! a1)))
    (check-equal? a2 (void a2))))

(begin-for-test
  (local
    ((define w1 (mk-enemy (make-posn 10 10) 4)))
    (check enemy-equal? w1 (new Enemyunit% [x 10] [y 10]
                                    [speed 4][color "green"] 
                                    ))))

(begin-for-test
  (local
    ((define a1 (new Enemyunit% [x 10] [y 10]
                                    [speed 4][color "red"] 
                                    ))
     (define a2 (send a1 get-color)))
    (check-equal? a2 "red")))

;;=============================================================================

; mercenary-equal? : MercenaryUnit% MercenaryUnit% -> Boolean
; This function takes in two mercenary units and returns a boolean stating 
; whether both of them are equal
; Example :
; (define (mercenary-equal? (new MercenaryUnit% [x 10] [y 10] [color "green"] 
; [speed 8] [unit ALLYUNIT] [counter 1])
; (new MercenaryUnit% [x 10] [y 10] [color "green"] [speed 8])
; [unit ALLUNIT] [counter 1])-> true

(define (mercenary-equal? m1 m2)
  (and
   (equal? (send m1 testing:get-x) (send m2 testing:get-x))
   (equal? (send m1 testing:get-y) (send m2 testing:get-y))
   (equal? (send m1 testing:get-speed) (send m2 testing:get-speed))
   ))

(begin-for-test
  (local
    ((define a1 (new MercenaryUnit% [x 10] [y 10] [color "green"] [speed 8]
                   [unit ALLYUNIT] [counter 1]  ))
     (define a2 (send a1 unit-on-mouse 10 10 "button-up")))
    (check mercenary-equal? a2 (new MercenaryUnit% [x 10] [y 10] 
                                    [color "green"] 
                               [speed 8][unit ALLYUNIT] [counter 1]
                     ))))
(begin-for-test
  (local
    ((define a1 (new MercenaryUnit% [x 10] [y 10] [color "green"] [speed 8]
                    [unit ALLYUNIT] [counter 1] ))
     (define a2 (send a1 unit-on-mouse 10 10 "button-down")))
    (check-equal? a2 #f)))

(begin-for-test
  (local
    ((define a1 (new MercenaryUnit% [x 10] [y 10] [color "green"] [speed 8]
                   [unit ALLYUNIT] [counter 1]  ))
     (define a2 (send a1 unit-on-mouse 10 10 "button-up")))
    (check mercenary-equal? a2 (new MercenaryUnit% [x 10] [y 10] 
                                    [color "green"] 
                               [speed 8][unit ALLYUNIT] [counter 1]
                     ))))

(begin-for-test
  (local
    ((define a1 (new MercenaryUnit% [x 10] [y 10] [color "green"] [speed 8]
                   [unit ALLYUNIT] [counter 1]  ))
     (define a2 (send a1 unit-on-mouse 500 500 "button-down")))
    (check mercenary-equal? a2 (new MercenaryUnit% [x 10] [y 10] 
                                    [color "green"] 
                               [speed 8][unit ALLYUNIT] [counter 1] 
                     ))))

(begin-for-test
  (local
    ((define a1 (new MercenaryUnit% [x 10] [y 10] [color "green"] [speed 8]
                   [unit ENEMYUNIT] [counter 1]  ))
     (define a2 (send a1 unit-on-mouse 10 10 "button-down")))
    (check-equal? a2 false)))

(begin-for-test
  (local
    ((define a1 (new MercenaryUnit% [x 50] [y 50] [color "green"] [speed 8]
                    [unit ALLYUNIT] [counter 1]  ))
     (define a2 (send a1 on-tick)))
    (check mercenary-equal? a2 (new MercenaryUnit% [x 50] [y 58] 
                                    [color "green"] 
                               [speed 8][unit ALLYUNIT] [counter 1]
                      ))))
(begin-for-test
  (local
    ((define a1 (new MercenaryUnit% [x 50] [y 50] [color "green"] [speed 8]
                    [unit ALLYUNIT] [counter 2]  ))
     (define a2 (send a1 on-tick)))
    (check mercenary-equal? a2 (new MercenaryUnit% [x 50] [y 58] 
                                    [color "green"] 
                               [speed 8][unit ENEMYUNIT] [counter 3]
                      ))))

(begin-for-test
  (local
    ((define a1 (new MercenaryUnit% [x 50] [y 50] [color "green"] [speed 8]
                    [unit ENEMYUNIT] [counter 2]  ))
     (define a2 (send a1 on-tick)))
    (check mercenary-equal? a2 (new MercenaryUnit% [x 50] [y 58] 
                                    [color "green"] 
                               [speed 8][unit ALLYUNIT] [counter 3]
                      ))))
(begin-for-test
  (local
    ((define a1 (new MercenaryUnit% [x 50] [y 50] [color "green"] [speed 8]
                    [unit ALLYUNIT] [counter 3]  ))
     (define a2 (send a1 on-tick)))
    (check mercenary-equal? a2 (new MercenaryUnit% [x 50] [y 58] 
                                    [color "green"] 
                               [speed 8][unit ALLYUNIT] [counter 4]
                      ))))

(begin-for-test
  (local
    ((define a1 (new MercenaryUnit% [x 50] [y 50] [color "green"] [speed 8]
                    [unit ALLYUNIT] [counter 3]  ))
     (define a2 (send a1 on-tick)))
    (check mercenary-equal? a2 (new MercenaryUnit% [x 50] [y 58] 
                                    [color "green"] 
                               [speed 8][unit ALLYUNIT] [counter 4]
                      ))))

(begin-for-test
  (local
    ((define a1 (new MercenaryUnit% [x 452] [y 452] [color "green"] [speed 8]
                  [unit ALLYUNIT] [counter 1]))
     (define a2 (send a1 on-tick)))
    (check-equal? a2 false)))

(begin-for-test
  (local
    ((define a1 (new MercenaryUnit% [x 452] [y 452] [color "green"] [speed 8]
                  [unit ENEMYUNIT] [counter 1]))
     (define a2 (send a1 on-tick)))
    (check-equal? a2 false)))

(begin-for-test
  (local
    ((define a1 (new MercenaryUnit% [x 452] [y 452] [color "green"] [speed 8]
                  [unit ENEMYUNIT] [counter 1]))
     (define a2 (send a1 add-to-scene EMPTY-CANVAS)))
    (check-true (image? a2) true)))

(begin-for-test
  (local
    ((define a1 (new MercenaryUnit% [x 452] [y 452] [color "green"] [speed 8]
                  [unit ALLYUNIT] [counter 1]))
     (define a2 (send a1 add-to-scene EMPTY-CANVAS)))
    (check-true (image? a2) true)))

(begin-for-test
  (local
    ((define a1 (new MercenaryUnit% [x 452] [y 452] [color "green"] [speed 8]
                  [unit ALLYUNIT] [counter 1] ))
     (define a2 (send a1 get-loc)))
    (check-equal? a2 (make-posn 452 452))))

(begin-for-test
  (local
    ((define a1 (new MercenaryUnit% [x 452] [y 452] [color "green"] [speed 8]
                   [unit ALLYUNIT] [counter 1]))
     (define a2 (send a1 subscribe! a1)))
    (check-equal? a2 (void a2))))


(begin-for-test
  (local
    ((define w1 (mk-merc (make-posn 10 10) 4)))
    (check mercenary-equal? w1 (new MercenaryUnit% [x 10] [y 10]
                                    [speed 4][color "green"] [unit ALLYUNIT]
                                    [counter 1]))))

(begin-for-test
  (local
    ((define a1 (new MercenaryUnit% [x 10] [y 10]
                                    [speed 4][color "green"] 
           [unit ALLYUNIT] [counter 1]))
     (define a2 (send a1 get-color)))
    (check-equal? a2 "green")))


;;=============================================================================

; world-equal? : World% World% -> Boolean
; This function takes in two objects of World% class and returns a boolean
; stating whether both of them are equal
; Example :
; (define (world-equal? (new World% [x 200] [y 200] [units 
; (list (mk-ally (make-posn 10 10) 8))][minvel 4] [maxvel 5] [height 450]
; [counter 5]) (new World% [x 200] [y 200] [units 
; (list (mk-ally (make-posn 10 10) 8))][minvel 4] [maxvel 5] [height 450]
;  [counter 5])) -> true

(define (world-equal? w1 w2)
  (and
   (equal? (send w1 testing:get-x) (send w2 testing:get-x))
   (equal? (send w1 testing:get-y) (send w2 testing:get-y))
   (equal? (send w1 testing:get-minvel) (send w2 testing:get-minvel))
   (equal? (send w1 testing:get-maxvel) (send w2 testing:get-maxvel))
   ))

(begin-for-test
  (local
    ((define w1 (new World% [x 200] [y 200] 
                     [units (list (mk-ally (make-posn 10 10) 8))]
                     [minvel 4] [maxvel 5] [base-height 450]
                     [counter 1]))
     (define w2 (send w1 on-tick!)))
    (check-equal? w2 (void w2))))

(begin-for-test
  (local
    ((define w1 (new World% [x 200] [y 200] 
                     [units (list (mk-ally (make-posn 10 10) 8))]
                     [minvel 4] [maxvel 5] [base-height 450]
                     [counter 5]))
     (define w2 (send w1 on-tick!)))
    (check-equal? w2 (void w2))))


(begin-for-test
  (local
    ((define w1 (new World% [x 200] [y 200] 
                     [units (list (mk-ally (make-posn 10 10) 8))]
                     [minvel 4] [maxvel 5] [base-height 450]
                     [counter 5]))
     (define w2 (send w1 on-mouse! 10 10 "button-down")))
    (check-equal? w2 (void w2))))

(begin-for-test
  (local
    ((define w1 (new World% [x 200] [y 200] 
                     [units (list (mk-ally (make-posn 10 10) 8))]
                     [minvel 4] [maxvel 5] [base-height 450]
                     [counter 5]))
     (define w2 (send w1 on-mouse! 10 10 "move")))
    (check-equal? w2 (void w2))))

(begin-for-test
  (local
    ((define w1 (new World% [x 200] [y 200] 
                     [units (list (mk-ally (make-posn 10 10) 8))]
                     [minvel 4] [maxvel 5] [base-height 450]
                     [counter 5]))
     (define w2 (send w1 on-mouse! 500 500 "button-down")))
    (check world-equal? w2 (new World% [x 200] [y 200] [minvel 4] [maxvel 5]
         [units (list (new Allyunit% [x 10] [y 10] [speed 8] [color "green"]))]
         [base-height 450] [counter 5]))))

(begin-for-test
  (local
    ((define w1 (new World% [x 200] [y 200] 
                     [units (list (mk-ally (make-posn 10 10) 8))]
                     [minvel 4] [maxvel 5] [base-height 450]
                     [counter 5]))
     (define w2 (send w1 target-loc)))
    (check-equal? w2 (make-posn 200 200))))

(begin-for-test
  (local
    ((define w1 (new World% [x 200] [y 200] 
                     [units (list (mk-ally (make-posn 10 10) 8))]
                     [minvel 4] [maxvel 5] [base-height 450]
                     [counter 5]))
     (define w2 (send w1 get-units)))
    (check-equal? (list? (list (new Allyunit% [x 10] [y 10] [speed 8] 
                               [color "green"]))) true)))

(begin-for-test
  (local
    ((define w1 (new World% [x 200] [y 200] 
                     [units (list (mk-ally (make-posn 10 10) 8))]
                     [minvel 4] [maxvel 5] [base-height 450]
                     [counter 5]))
     (define w2 (send w1 get-base-height)))
    (check-equal?  w2 450)))

(begin-for-test
  (local
    ((define w1 (new World% [x 200] [y 200] 
                     [units (list (mk-ally (make-posn 10 10) 8))]
                     [minvel 4] [maxvel 5] [base-height 450]
                     [counter 5]))
     (define w2 (send w1 add-unit! (new Allyunit% [x 10] [y 10] [speed 8] 
                               [color "green"]) )))
    (check-equal? w2 (void w2))))

(begin-for-test
  (local
    ((define w1 (new World% [x 200] [y 200] 
                     [units (list (mk-ally (make-posn 10 10) 8))]
                     [minvel 4] [maxvel 5] [base-height 450]
                     [counter 4]))
     (define w2 (send w1 add-unit-on-tick)))
    (check-equal? w2 (void w2))))

(begin-for-test 
  (local
    ((define a1 (new World% [x 200] [y 200] 
                     [units (list (mk-ally (make-posn 10 10) 8))]
                     [minvel 4] [maxvel 5] [base-height 450]
                     [counter 4]))
     (define a2 (send a1 on-draw)))
    (check-true (image? a2) true)))

(begin-for-test
  (local
    ((define a1 (new World% [x 200] [y 200] 
                     [units (list (mk-ally (make-posn 10 10) 8))]
                     [minvel 4] [maxvel 5] [base-height 450]
                     [counter 4]))
       (define a2 (send a1 update-score! 7)))
    (check-equal? a2 (void a2))))

(begin-for-test
  (local
    ((define w1 (new World% [x 200] [y 200] 
                     [units (list (mk-ally (make-posn 10 10) 8))]
                     [minvel 4] [maxvel 5] [base-height 450]
                     [counter 5]))
     (define w2 (send w1 on-mouse! 200 200 "button-down")))
    (check world-equal? w2 (new World% [x 200] [y 200] [minvel 4] [maxvel 5]
         [units (list (new Allyunit% [x 10] [y 10] [speed 8] [color "green"]))]
         [base-height 450] [counter 5]))))
 
(begin-for-test
  (local
    ((define w1 (new World% [x 200] [y 200] 
                     [units (list (mk-ally (make-posn 200 200) 8))]
                     [minvel 4] [maxvel 5] [base-height 450]
                     [counter 5]))
     (define w2 (send w1 on-mouse! 200 200 "button-down")))
    (check-equal? w2 (void w2))))

(begin-for-test
  (local
    ((define a1 (mk-world 3 4 1)))
    (check world-equal? a1 (new World% [x 200] [y 200]
                             [units (list (new Allyunit% [x 10] [y 10]
                                  [speed 4] [color "green"] 
                                    ))]
                             [score 0] [minvel 3] [maxvel 4] [base-height 450]
                             [counter 1]))))



;;=============================================================================


; Alternate Designs:

; Part 1:
; In our program we have used the push approch to obtain the scores of all the
; units.
; We have used the publish and subscribe interfaces where the world subscribes
; to each unit in order to obtain the scores and in turn the relevant unit 
; pushes its score to the world.

; An alternate approach would be to use the pull approach to get the scores.
; Here, we obtain the score of each unit by pulling the score from the relevant
; units.

; Here is a code snippet which can be placed in the world-after-button-down
; function in World%

; (set! score (foldr (lambda(unit x) 
;   (+ x (send unit get-score))) score units))


; Part 2:
; The same thing applies to the world's on-tick! function as well,where 
; we could have used a modified form of this function.

; However, the scores were not getting updated properly whenever a unit reached 
; the base which is why we decided to go ahead with the push approach.