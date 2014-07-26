; 1 - User versus user
; 2 - User versus computer
; 3 - Computer versus user
(define game-type 0)



; NOTICE: 0 index is ignored, there are 10 elements for easier refering
;         This is wasing resources, but still, who cares..

; Position indexes for x
(define pox (vector 0 0 0 0 0 0 0 0 0 0))
; Position indexes for o
(define poy (vector 0 0 0 0 0 0 0 0 0 0))


(define places (vector 0 0 0 0 0 0 0 0 0 0))

; Turn
(define turn 0)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FIRST FRAME (Choices)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define frame (instantiate frame% ("OX Game by Piotr Polak" #F 400 100)))
(define button1 (instantiate button% () (label "User versus User") (parent frame) (min-width 200)
             (callback (lambda (button event) 
                           (send frame show #f)
                           (send frame2 show #t)
                           (set! game-type 1)
                         ))))
(define button2 (instantiate button% () (label "User versus computer") (parent frame) (min-width 200)
             (callback (lambda (button event) 
                           (send frame show #f)
                           (send frame2 show #t)
                           (set! game-type 2)
                         ))))
(define button3 (instantiate button% () (label "Computer versus user") (parent frame) (min-width 200)
             (callback (lambda (button event) 
                           (send frame show #f)
                           (send frame2 show #t)
                           (set! game-type 3)
                           (do-click (get-computer-next-move))
                         ))))
(send frame show #t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; GAME FRAME
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define frame2 (instantiate frame% ("OX Game by Piotr Polak" #F 200 200)))

(define msg (instantiate message% ("Game ON! " frame2)))

(send frame2 show #f)


; ROW 1
(define panel1 (instantiate horizontal-panel%  (frame2) (alignment '(center center))))


(define ba1 (instantiate button% () (label " ") (parent panel1) (min-width 20)
             (callback (lambda (button event) 
                           (do-click 1)
                         ))))

(define ba2 (instantiate button% () (label " ") (parent panel1) (min-width 20)
             (callback (lambda (button event) 
                           (do-click 2)
                         ))))
(define ba3 (instantiate button% () (label " ") (parent panel1) (min-width 20)
             (callback (lambda (button event) 
                           (do-click 3)
                         ))))

; ROW 2
(define panel2 (instantiate horizontal-panel%  (frame2) (alignment '(center center))))
(define ba4 (instantiate button% () (label " ") (parent panel2) (min-width 20)
             (callback (lambda (button event) 
                            (do-click 4)
                         ))))

(define ba5 (instantiate button% () (label " ") (parent panel2) (min-width 20)
             (callback (lambda (button event) 
                            (do-click 5)
                         ))))
(define ba6 (instantiate button% () (label " ") (parent panel2) (min-width 20)
             (callback (lambda (button event) 
                           (do-click 6)
                         ))))

; ROW 3
(define panel3 (instantiate horizontal-panel%  (frame2) (alignment '(center center))))
(define ba7 (instantiate button% () (label " ") (parent panel3) (min-width 20)
             (callback (lambda (button event) 
                           (do-click 7)
                         ))))

(define ba8 (instantiate button% () (label " ") (parent panel3) (min-width 20)
             (callback (lambda (button event) 
                           (do-click 8)
                         ))))
(define ba9 (instantiate button% () (label " ") (parent panel3) (min-width 20)
             (callback (lambda (button event) 
                            (do-click 9)
                         ))))


; ACTIONS
(define switch-turn
  (lambda (n)
    (
     if (= n 0)
        (set! turn 1)
        (set! turn 0)
     )
   )
 )




; Changes gui button
(define do-click
  (lambda (n)
    (
      if (= (vector-ref pox n) 0)
       (
         if (= (vector-ref poy n) 0)
           (begin
            (update-button n)
            (
             if (= turn 0)
               (vector-set! pox n 1)
               (vector-set! poy n 1)
             )
            
            
           
            (vector-set! places n 1)
            
             (switch-turn turn)
             
             ;; FOR COMP
              (if (= game-type 3)
                (if (= turn 0)
                    (begin 
                      (sleep 3)
                      (do-click (get-computer-next-move))
                     )
                    )
                )
              
               (if (= game-type 2)
                (if (= turn 1)
                    (begin 
                      (sleep 3)
                      (do-click (get-computer-next-move))
                     )
                    )
                )
              ;; END FOR COMP
             
             (
              if (is-winer-x)
                 (begin
                   (freeze-buttons)
                   (send msg set-label "X wins!")
                 )
                 #f
             )
              (
              if (is-winer-y)
                 (begin
                   (freeze-buttons)
                   (send msg set-label "O wins!")
                 )
                 #f
             )    
           )
           #f
        )
      #f
    )
  )
)

; Associates X and Y to the current integer value of turn variable
(define get-current-turn-label
  (lambda (turn)
    (
     if (= turn 0)
        "X"
        "O"
    )
   )
 )

; Updates GUI button according to the given parameter (n as position from 1 to 9)
(define update-button
  (lambda (n)
    (cond 
      ((= n 1) (send ba1 set-label (get-current-turn-label turn)))
      ((= n 2) (send ba2 set-label (get-current-turn-label turn)))
      ((= n 3) (send ba3 set-label (get-current-turn-label turn)))
      ((= n 4) (send ba4 set-label (get-current-turn-label turn)))
      ((= n 5) (send ba5 set-label (get-current-turn-label turn)))
      ((= n 6) (send ba6 set-label (get-current-turn-label turn)))
      ((= n 7) (send ba7 set-label (get-current-turn-label turn)))
      ((= n 8) (send ba8 set-label (get-current-turn-label turn)))
      ((= n 9) (send ba9 set-label (get-current-turn-label turn)))
      (else 0)
     )
   )
 )

; Checks if X has a winning combination
(define is-winer-x
  (lambda ()
     (cond
      ((= (add3 (vector-ref pox 1) (vector-ref pox 2) (vector-ref pox 3)) 3) #t)
      ((= (add3 (vector-ref pox 4) (vector-ref pox 5) (vector-ref pox 6)) 3) #t)
      ((= (add3 (vector-ref pox 7) (vector-ref pox 8) (vector-ref pox 9)) 3) #t)
      
      ((= (add3 (vector-ref pox 1) (vector-ref pox 4) (vector-ref pox 7)) 3) #t)
      ((= (add3 (vector-ref pox 2) (vector-ref pox 5) (vector-ref pox 8)) 3) #t)
      ((= (add3 (vector-ref pox 3) (vector-ref pox 6) (vector-ref pox 9)) 3) #t)
      
      ((= (add3 (vector-ref pox 1) (vector-ref pox 5) (vector-ref pox 9)) 3) #t)
      ((= (add3 (vector-ref pox 3) (vector-ref pox 5) (vector-ref pox 7)) 3) #t)
      
      (else #f)
      )
    )
  )

; Checks if O has a winning combination
(define is-winer-y
  (lambda ()
     (cond
      ((= (add3 (vector-ref poy 1) (vector-ref poy 2) (vector-ref poy 3)) 3) #t)
      ((= (add3 (vector-ref poy 4) (vector-ref poy 5) (vector-ref poy 6)) 3) #t)
      ((= (add3 (vector-ref poy 7) (vector-ref poy 8) (vector-ref poy 9)) 3) #t)
      
      ((= (add3 (vector-ref poy 1) (vector-ref poy 4) (vector-ref poy 7)) 3) #t)
      ((= (add3 (vector-ref poy 2) (vector-ref poy 5) (vector-ref poy 8)) 3) #t)
      ((= (add3 (vector-ref poy 3) (vector-ref poy 6) (vector-ref poy 9)) 3) #t)
      
      ((= (add3 (vector-ref poy 1) (vector-ref poy 5) (vector-ref poy 9)) 3) #t)
      ((= (add3 (vector-ref poy 3) (vector-ref poy 5) (vector-ref poy 7)) 3) #t)
      
      (else #f)
      )
    )
  )

; Freezes the interface preventing the clicks
(define freeze-buttons
  (lambda()
    (begin
      (send ba1 enable #f)
      (send ba2 enable #f)
      (send ba3 enable #f)
      (send ba4 enable #f)
      (send ba5 enable #f)
      (send ba6 enable #f)
      (send ba7 enable #f)
      (send ba8 enable #f)
      (send ba9 enable #f)
      )
    )
  )
; Adds 3 numbers
(define add3
  (lambda (x y z)
    (+ (+ x y)z )))

;; This should implemet minmax
(define first-free-place
  (lambda()
    (cond
      ((= (vector-ref places 5) 0) 5)
      ((= (vector-ref places 1) 0) 1)
      ((= (vector-ref places 2) 0) 2)
      ((= (vector-ref places 3) 0) 3)
      ((= (vector-ref places 4) 0) 4)     
      ((= (vector-ref places 6) 0) 6)
      ((= (vector-ref places 7) 0) 7)
      ((= (vector-ref places 8) 0) 8)
      ((= (vector-ref places 9) 0) 9)
      (else #f)
     )
    )
  )

;; Returns first winning/blocking place
(define get-computer-next-move
  (lambda()
    (cond
      
      ;; HORIZONTAL
      ((is-winning-combination 1 2 3) (get-free-space 1 2 3))
      ((is-winning-combination 4 5 6) (get-free-space 4 5 6))
      ((is-winning-combination 7 8 9) (get-free-space 7 8 9))
      ;; VERTICAL
      ((is-winning-combination 1 4 7) (get-free-space 1 4 7))
      ((is-winning-combination 2 5 8) (get-free-space 2 5 8))
      ((is-winning-combination 3 6 9) (get-free-space 3 6 9))
      ;; ON DIAGONAL
      ((is-winning-combination 1 5 9) (get-free-space 1 5 9))
      ((is-winning-combination 3 5 7) (get-free-space 3 5 7))
      (else (first-free-place))
      )
    )
  )

; Adds 3 numbers
(define add6
  (lambda (x y z k l m)
    (+ (+ (+ (+ (+ x y)z ) k) l) m)
    )
  )

; Returns vector-ref depending on the game typechosen
(define vector-ref-computer
  (lambda (x)
    (if (= game-type 2)
        (vector-ref pox x)
        (vector-ref poy x)
    )
  )
)

(define get-free-space
  (lambda(x y z)
    (cond
      ((= (vector-ref places x) 0) x)
      ((= (vector-ref places y) 0) y)
      ((= (vector-ref places z) 0) z)
      (else (first-free-place))
     )
    )
  )


; Checks whenever a given combination is winning
(define is-winning-combination
  (lambda(x y z)
    (if (= (add6 (vector-ref places x) (vector-ref places y) (vector-ref places z) (vector-ref-computer x) (vector-ref-computer y) (vector-ref-computer z)) 4)
        #t
        #f
        
    )
   )
 )
    
    
        