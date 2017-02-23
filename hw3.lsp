;
; CS161 Hw3: Sokoban
; 
; *********************
;    READ THIS FIRST
; ********************* 
;
; All functions that you need to modify are marked with 'EXERCISE' in their header comments.
; Do not modify a-star.lsp.
; This file also contains many helper functions. You may call any of them in your functions.
;
; *Warning*: The provided A* code only supports the maximum cost of 4999 for any node.
; That is f(n)=g(n)+h(n) < 5000. So, be careful when you write your heuristic functions.
; Do not make them return anything too large.
;
; For Allegro Common Lisp users: The free version of Allegro puts a limit on memory.
; So, it may crash on some hard sokoban problems and there is no easy fix (unless you buy 
; Allegro). 
; Of course, other versions of Lisp may also crash if the problem is too hard, but the amount
; of memory available will be relatively more relaxed.
; Improving the quality of the heuristic will mitigate this problem, as it will allow A* to
; solve hard problems with fewer node expansions.
; 
; In either case, this limitation should not significantly affect your grade.
; 
; Remember that most functions are not graded on efficiency (only correctness).
; Efficiency can only influence your heuristic performance in the competition (which will
; affect your score).
;  
;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General utility functions
; They are not necessary for this homework.
; Use/modify them for your own convenience.
;

;
; For reloading modified code.
; I found this easier than typing (load "filename") every time. 
;
(defun reload()
  (load "hw3.lsp")
  )

;
; For loading a-star.lsp.
;
(defun load-a-star()
 (load "a-star.lsp"))

;
; Reloads hw3.lsp and a-star.lsp
;
(defun reload-all()
  (reload)
  (load-a-star)
  )

;
; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).
; 
;
(defun sokoban (s h)
  (a* s #'goal-test #'next-states h)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end general utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We now begin actual Sokoban code
;
; Define some global variables
(setq blank 0)
(setq wall 1)
(setq box 2)
(setq keeper 3)
(setq star 4)
(setq boxstar 5)
(setq keeperstar 6)

; Some helper functions for checking the content of a square
(defun isBlank (v)
  (= v blank)
  )

(defun isWall (v)
  (= v wall)
  )

(defun isBox (v)
  (= v box)
  )

(defun isKeeper (v)
  (= v keeper)
  )

(defun isStar (v)
  (= v star)
  )

(defun isBoxStar (v)
  (= v boxstar)
  )

(defun isKeeperStar (v)
  (= v keeperstar)
  )

;
; Helper function of getKeeperPosition
;
(defun getKeeperColumn (r col)
  (cond ((null r) nil)
	(t (if (or (isKeeper (car r)) (isKeeperStar (car r)))
	       col
	     (getKeeperColumn (cdr r) (+ col 1))
	     );end if
	   );end t
	);end cond
  )

;
; getKeeperPosition (s firstRow)
; Returns a list indicating the position of the keeper (c r).
; 
; Assumes that the keeper is in row >= firstRow.
; The top row is the zeroth row.
; The first (right) column is the zeroth column.
;
(defun getKeeperPosition (s row)
  (cond ((null s) nil)
	(t (let ((x (getKeeperColumn (car s) 0)))
	     (if x
		 ;keeper is in this row
		 (list x row)
		 ;otherwise move on
		 (getKeeperPosition (cdr s) (+ row 1))
		 );end if
	       );end let
	 );end t
	);end cond
  );end defun

;
; cleanUpList (l)
; returns l with any NIL element removed.
; For example, if l is '(1 2 NIL 3 NIL), returns '(1 2 3).
;
(defun cleanUpList (L)
  (cond ((null L) nil)
	(t (let ((cur (car L))
		 (res (cleanUpList (cdr L)))
		 )
	     (if cur 
		 (cons cur res)
		  res
		 )
	     );end let
	   );end t
	);end cond
  );end 

; EXERCISE: Modify this function to return true (t)
; if and only if s is a goal state of a Sokoban game.
; (no box is on a non-goal square)
;
; Currently, it always returns NIL. If A* is called with
; this function as the goal testing function, A* will never
; terminate until the whole search space is exhausted.
;
(defun goal-test (s)
  (cond ( (null s) t) ;;if reached end of state with no boxes
	;;if the first item is a list, check the first list for boxes and the rest.
	( (listp (car s)) (and (goal-test (car s)) (goal-test (cdr s))))
       
	( (isBox (car s)) NIL) ;; if box return false
	( t (goal-test (cdr s))) ;; otherwise keep checking. 
  );end cond
  );end defun

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;; Helper functions

;;gets value in row list (column) 
(defun get-col (r col)
  (cond ( (null r) 1)
	( (= col 0 ) (car r))
	( t (get-col (cdr r) (- col 1)))
   )
  )

;;gets value in state from square
(defun get-square (s r c)
  (cond ( (null s ) 1)
	( (= r 0) (get-col (car s) c))
	( t ( get-square (cdr s) (- r 1) c))
   )
  )

;;sets value in a row list (column)
(defun set-col (row col val )
  (cond ( (null row) '())
	( (= col 0) (cons val (cdr row)) )
	( (cons (car row) (set-col (cdr row) (- col 1) val  )))
   )
  )

;; sets square in state to value
(defun set-square (s r c v)
  (cond ( (null s) '())
	( (= r 0) (cons (set-col (car s) c v) (cdr s)))
	( t (cons (car s) (set-square (cdr s) (- r 1) c v)))
   )
  )

;;returns true if a move in a direction is invalid.
;;for each direction a move is invalid if:
;; 1) there is a wall 1 block away
;; 2) there is a box 1 block away and a box/wall/boxstar 2 blocks away
;; 3) boxstar 1 block way with a box/wall/boxstar 2 blocks away
(defun isInvalidMove ( s dir)
  (let* ( (up    0)
	  (down  1)
	  (right 2)
	  (left  3)
	  ( kpos (getKeeperPosition s 0))
	  (col (car kpos))
	  (row (cadr kpos))
	  (upBy1 (get-square s (- row 1) col))
	  (upBy2 (get-square s (- row 2) col))
	  (downBy1 (get-square s (+ row 1) col))
	  (downBy2 (get-square s (+ row 2) col))
	  (leftBy1 (get-square s row (- col 1)))
	  (leftBy2 (get-square s row (- col 2)))
	  (rightBy1 (get-square s row (+ col 1)))
	  (rightBy2 (get-square s row (+ col 2)))
	  
	  )
    
    (cond 
     ( (= dir up)  (cond ( (isWall upBy1) t)
			 
			 ( (and (isBox upBy1) (isBox upBy2))     t)
			 ( (and (isBox upBy1) (isWall upBy2))    t)
			 ( (and (isBox upBy1) (isBoxStar upBy2)) t)

			 ( (and (isBoxStar upBy1) (isBox upBy2))     t)
			 ( (and (isBoxStar upBy1) (isWall upBy2))    t)
			 ( (and (isBoxStar upBy1) (isBoxStar upBy2)) t)
			 ( t NIL)))
     
     ( (= dir down)  (cond ( (isWall downBy1) t)
			 
			   ( (and (isBox downBy1) (isBox downBy2))    t)
			   ( (and (isBox downBy1) (isWall downBy2))    t)
			   ( (and (isBox downBy1) (isBoxStar downBy2)) t)
			   
			   ( (and (isBoxStar downBy1) (isBox downBy2))     t)
			   ( (and (isBoxStar downBy1) (isWall downBy2))    t)
			   ( (and (isBoxStar downBy1) (isBoxStar downBy2)) t)
			   ( t NIL)))
     ( (= dir right)  (cond ( (isWall rightBy1) t)
			   
			    ( (and (isBox rightBy1) (isBox rightBy2))     t)
			    ( (and (isBox rightBy1) (isWall rightBy2))    t)
			    ( (and (isBox rightBy1) (isBoxStar rightBy2)) t)
			    
			    ( (and (isBoxStar rightBy1) (isBox rightBy2))     t)
			    ( (and (isBoxStar rightBy1) (isWall rightBy2))    t)
			    ( (and (isBoxStar rightBy1) (isBoxStar rightBy2)) t)
			    ( t NIL)))
     
     ( (= dir left)  (cond ( (isWall leftBy1) t)
			    
			    ( (and (isBox leftBy1) (isBox leftBy2))     t)
			    ( (and (isBox leftBy1) (isWall leftBy2))    t)
			    ( (and (isBox leftBy1) (isBoxStar leftBy2)) t)
			    
			    ( (and (isBoxStar leftBy1) (isBox leftBy2))     t)
			    ( (and (isBoxStar leftBy1) (isWall leftBy2))    t)
			    ( (and (isBoxStar leftBy1) (isBoxStar leftBy2)) t)
			    ( t NIL)))
     );end cond
    ) ;end let

  ) ;end defun

;;returns the state where keeper has been moved. i.e keeper square = blank/star. 
(defun movedKeeper (s kvalue krow kcol)
  (cond ( (isKeeper kvalue) (set-square s krow kcol blank))
	( (isKeeperStar kvalue) (set-square s krow kcol star))
	)
  )

;;returns the state where the keeper is in the spot where the box/blank was.
;; i.e keeper/keeperstar
(defun movedintoSq1 ( s bvalue brow bcol)
  (cond ( (or (isBlank bvalue) (isBox bvalue)) (set-square s brow bcol keeper))
	( (or (isStar bvalue) (isBoxStar bvalue)) (set-square s brow bcol keeperstar))
	( t s)
	)
  )

;;returns the state where the box is moved into the next spot. i.e sq = box/boxstar
(defun movedintoSq2 (s value row col)
  (cond ( (isBlank value) (set-square s row col box))
	( (isStar  value) (set-square s row col boxstar))
	)
  )

;; returns the finished state when the keeper and box are moved
;;                  keeper info        square 1 info      square 2 info
(defun allMoved ( s kval krow kcol sq1_val sq1_r sq1_c sq2_val sq2_r sq2_c)
  (let* ( (s1 (movedKeeper s kval krow kcol))
	  (s2 (movedintoSq1 s1 sq1_val sq1_r sq1_c))
	  )

    ;; if there is a block move the block as well. otherwise return the state where
    ;; only the keeper was moved. 
    (cond ( (or (isBoxStar sq1_val) (isBox sq1_val)) (movedintoSq2 s2 sq2_val sq2_r sq2_c))
	  ( t s2)
	    
	  );end cond
    );end let
  );end defun

;;check whether the move is legal. If it is, it will need to update some
;;squares of the current state to reflect the changes.
(defun try-move ( s dir)
  (let* ( (up    0)
	  (down  1)
	  (right 2)  
	  (left  3)
	  ;; keepers info
	  (kpos (getKeeperPosition s 0))
	  (kcol (car kpos))
	  (krow (cadr kpos))
	  (kval (get-square s krow kcol))

	  (up1_r (- krow 1))
	  (up1_c  kcol)
	  (up1_val (get-square s (- krow 1) kcol))
	  
	  (up2_r (- krow 2))
	  (up2_c  kcol)
	  (up2_val (get-square s (- krow 2) kcol))

	  (down1_r (+ krow 1) )
	  (down1_c kcol)
	  (down1_val (get-square s (+ krow 1) kcol))

	  (down2_r (+ krow 2) )
	  (down2_c kcol)
	  (down2_val (get-square s (+ krow 2) kcol))

	  (left1_r krow)
	  (left1_c (- kcol 1))
	  (left1_val (get-square s krow (- kcol 1)))

	  (left2_r krow)
	  (left2_c (- kcol 2))
	  (left2_val (get-square s krow (- kcol 2)))

	  (right1_r krow)
	  (right1_c (+ kcol 1))
	  (right1_val (get-square s krow (+ kcol 1)))

	  (right2_r krow)
	  (right2_c (+ kcol 2))
	  (right2_val (get-square s krow (+ kcol 2)))
	  
	  
	  )

    (cond ( (and (= dir up)    (isInValidMove s dir)) NIL)
	  ( (and (= dir down)  (isInvalidMove s dir)) NIL)
	  ( (and (= dir right) (isInvalidMove s dir)) NIL)
	  ( (and (= dir left)  (isInvalidMove s dir)) NIL)

	  ( (= dir up) (allMoved s kval krow kcol
				 up1_val up1_r up1_c
				 up2_val up2_r up2_c))

	  ( (= dir down) (allMoved s kval krow kcol
				   down1_val down1_r down1_c
				   down2_val down2_r down2_c))

	  ( (= dir right) (allMoved s kval krow kcol
				    right1_val right1_r right1_c
				    right2_val right2_r right2_c))

	  ( (= dir left) (allMoved s kval krow kcol
				   left1_val left1_r left1_c
				   left2_val left2_r left2_c))

       );end cond
    );end let 
  );end defun 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; EXERCISE: Modify this function to return the list of 
; sucessor states of s.
;
; This is the top-level next-states (successor) function.
; Some skeleton code is provided below.
; You may delete them totally, depending on your approach.
; 
; If you want to use it, you will need to set 'result' to be 
; the set of states after moving the keeper in each of the 4 directions.
; A pseudo-code for this is:
; 
; ...
; (result (list (try-move s UP) (try-move s DOWN) (try-move s LEFT) (try-move s RIGHT)))
; ...
; 
; You will need to define the function try-move and decide how to represent UP,DOWN,LEFT,RIGHT.
; Any NIL result returned from try-move can be removed by cleanUpList.


(defun next-states (s)
  (let* ( (up    0)
	  (down  1)
	  (right 2)  
	  (left  3)
	 ;x and y are now the coordinate of the keeper in s.
	 (result (list (try-move s UP) (try-move s DOWN)
		       (try-move s LEFT) (try-move s RIGHT)))
	 ) ;end 
    
    (cleanUpList result)
    
   );end let
  );end defun

; EXERCISE: Modify this function to compute the trivial
;; admissible heuristic.
;
(defun h0 (s)
  0 )

; EXERCISE: Modify this function to compute the 
; number of misplaced boxes in s.
					;

;; h1 is abmissible because it will never over estimate the cost of a goal
;; at least it takes 1 move for a box to reach a goal and h1 does not over estimate 
(defun h1 (s)
  (cond ( (null s) 0) ;base case 
	( (listp (car s)) (+ (h1 (car s)) (h1 (cdr s)))) ; check each sublist
	( (isBox (car s)) (+ 1 (h1 (cdr s)))) ;;if is box add 1 and check rest
	( t  (h1 (cdr s))) ;; else check rest of list
	)
  
  )

; EXERCISE: Change the name of this function to h<UID> where
; <UID> is your actual student ID number. Then, modify this 
; function to compute an admissible heuristic value of s. 
; 
; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the 
; running time of a function call.
					;

;; sum of manhattan distances of each box to closest star.
;; ( b1 b2 b3) (s1 s2 s3 s4)
;; ---->
;; (b1 -> s1) + (b2 -> s2) + (b3 -> s3)
(defun hMahDist (s)
  (let* ( (boxlist  (objlist s box  0 0))
	  (starlist (objlist s star 0 0))
	  )

    (cond ( (null s) 0)
	  ( t (score boxlist starlist))

	  ); end cond
    );end let
  ) ;end defun

;; sums the abs-value of the displacement of box to star. 
(defun score (blist slist )
  (cond
	  ( (null blist) 0)
	  ( (null slist) 0)
	  ( t  (let* ( 	  (brow (car (car blist)))
			  (bcol (cadr (car blist)))

			  (srow (car (car slist)))
			  (scol (cadr (car slist)))
			  )
		      ;; manhattan distance of one box to a goal. 
		 ( +  ( + (abs-value ( - brow srow)) (abs-value (- bcol scol)))

		      ;; other manhattan distances of other box -> goals
		      (score (cdr blist) (cdr slist)))
		 ))
	  )
)

(defun abs-value (x)
  (cond ( (< x 0) (- x) )
	( t x )))


;; creates a list of locations (row, col) of a given obj in the game. 
(defun objList ( s obj r c)
  (cond ( (null s) '())

	;;if (car s ) is a list then append objs locations r1 and in rest of S
	( (listp (car s)) (append (objList (car s)  obj r c)
				  (objList (cdr s) obj (+ r 1) c)))

	;; if (car s) is obj then add its (r , c) to list and check rest of row
	;; otherwise just check rest of row 
	( t (cond ((= (car s) obj) (cons (cons r (cons c '()))
					 (objList (cdr s) obj r (+ c 1))))
		  ( (objList (cdr s) obj r (+ c 1)))))  
   )
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.
 | Each problem can be visualized by calling (printstate <problem>). For example, (printstate p1).
 | Problems are ordered roughly by their difficulties.
 | For most problems, we also privide 2 additional number per problem:
 |    1) # of nodes expanded by A* using our next-states and h0 heuristic.
 |    2) the depth of the optimal solution.
 | These numbers are located at the comments of the problems. For example, the first problem below 
 | was solved by 80 nodes expansion of A* and its optimal solution depth is 7.
 | 
 | Your implementation may not result in the same number of nodes expanded, but it should probably
| give something in the same ballpark. As for the solution depth, any admissible heuristic must 
 | make A* return an optimal solution. So, the depths of the optimal solutions provided could be used
 | for checking whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible to solve without a good heuristic!
 | 
|#


;;testing different state purposes 
(setq goal1 '((1 1 1 1 1 1)
	      (1 2 0 0 2 1)
	      (1 0 2 0 0 1)
	      (1 0 0 4 0 1)
	      (1 0 4 4 0 1)
	      (1 0 0 0 0 1)
	      (1 1 1 1 1 1)))

;(80,7)
(setq p1 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 0 0 4 1)
	   (1 1 1 1 1 1)))

;(110,10)
(setq p2 '((1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 1) 
	   (1 0 0 0 0 0 1) 
	   (1 0 0 2 1 4 1) 
	   (1 3 0 0 1 0 1)
	   (1 1 1 1 1 1 1)))

;(211,12)
(setq p3 '((1 1 1 1 1 1 1 1 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 2 0 3 4 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 1 1 1 1 1 1 1 1)))

;(300,13)
(setq p4 '((1 1 1 1 1 1 1)
	   (0 0 0 0 0 1 4)
	   (0 0 0 0 0 0 0)
	   (0 0 1 1 1 0 0)
	   (0 0 1 0 0 0 0)
	   (0 2 1 0 0 0 0)
	   (0 3 1 0 0 0 0)))

;(551,10)
(setq p5 '((1 1 1 1 1 1)
	   (1 1 0 0 1 1)
	   (1 0 0 0 0 1)
	   (1 4 2 2 4 1)
	   (1 0 0 0 0 1)
	   (1 1 3 1 1 1)
	   (1 1 1 1 1 1)))

;(722,12)
(setq p6 '((1 1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 4 1)
	   (1 0 0 0 2 2 3 1)
	   (1 0 0 1 0 0 4 1)
	   (1 1 1 1 1 1 1 1)))

;(1738,50)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
	   (0 0 1 1 1 1 0 0 0 3)
	   (0 0 0 0 0 1 0 0 0 0)
	   (0 0 0 0 0 1 0 0 1 0)
	   (0 0 1 0 0 1 0 0 1 0)
	   (0 2 1 0 0 0 0 0 1 0)
	   (0 0 1 0 0 0 0 0 1 4)))

;(1763,22)
(setq p8 '((1 1 1 1 1 1)
	   (1 4 0 0 4 1)
	   (1 0 2 2 0 1)
	   (1 2 0 1 0 1)
	   (1 3 0 0 4 1)
	   (1 1 1 1 1 1)))

;(1806,41)
(setq p9 '((1 1 1 1 1 1 1 1 1) 
	   (1 1 1 0 0 1 1 1 1) 
	   (1 0 0 0 0 0 2 0 1) 
	   (1 0 1 0 0 1 2 0 1) 
	   (1 0 4 0 4 1 3 0 1) 
	   (1 1 1 1 1 1 1 1 1)))

;(10082,51)
(setq p10 '((1 1 1 1 1 0 0)
	    (1 0 0 0 1 1 0)
	    (1 3 2 0 0 1 1)
	    (1 1 0 2 0 0 1)
	    (0 1 1 0 2 0 1)
	    (0 0 1 1 0 0 1)
	    (0 0 0 1 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 1 1)))

;(16517,48)
(setq p11 '((1 1 1 1 1 1 1)
	    (1 4 0 0 0 4 1)
	    (1 0 2 2 1 0 1)
	    (1 0 2 0 1 3 1)
	    (1 1 2 0 1 0 1)
	    (1 4 0 0 4 0 1)
	    (1 1 1 1 1 1 1)))

;(22035,38)
(setq p12 '((0 0 0 0 1 1 1 1 1 0 0 0)
	    (1 1 1 1 1 0 0 0 1 1 1 1)
	    (1 0 0 0 2 0 0 0 0 0 0 1)
	    (1 3 0 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 2 1 1 1 0 0 0 1)
	    (1 0 0 0 0 1 0 1 4 0 4 1)
	    (1 1 1 1 1 1 0 1 1 1 1 1)))

;(26905,28)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
	    (1 4 0 0 0 0 0 2 0 1)
	    (1 0 2 0 0 0 0 0 4 1)
	    (1 0 3 0 0 0 0 0 2 1)
	    (1 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 0 0 0 0 4 1)
	    (1 1 1 1 1 1 1 1 1 1)))

;(41715,53)
(setq p14 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0 0 0)
	    (0 2 0 4 0 0 0)	   
	    (3 2 1 1 1 0 0)
	    (0 0 1 4 0 0 0)))

;(48695,44)
(setq p15 '((1 1 1 1 1 1 1)
	    (1 0 0 0 0 0 1)
	    (1 0 0 2 2 0 1)
	    (1 0 2 0 2 3 1)
	    (1 4 4 1 1 1 1)
	    (1 4 4 1 0 0 0)
	    (1 1 1 1 0 0 0)
	    ))

;(91344,111)
(setq p16 '((1 1 1 1 1 0 0 0)
	    (1 0 0 0 1 0 0 0)
	    (1 2 1 0 1 1 1 1)
	    (1 4 0 0 0 0 0 1)
	    (1 0 0 5 0 5 0 1)
	    (1 0 5 0 1 0 1 1)
	    (1 1 1 0 3 0 1 0)
	    (0 0 1 1 1 1 1 0)))

;(3301278,76)
(setq p17 '((1 1 1 1 1 1 1 1 1 1)
	    (1 3 0 0 1 0 0 0 4 1)
	    (1 0 2 0 2 0 0 4 4 1)
	    (1 0 2 2 2 1 1 4 4 1)
	    (1 0 0 0 0 1 1 4 4 1)
	    (1 1 1 1 1 1 0 0 0 0)))

;(??,25)
(setq p18 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 4 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 0 1 0 0 0 0)	    
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
	    ))
;(??,21)
(setq p19 '((0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 0 1 0 0 1 0 0 0 0)
	    (0 0 0 0 0 0 3 0 0 0 2 0)
	    (0 0 0 0 1 0 0 1 0 0 0 4)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 2 0 4 1 0 0 0)))

;(??,??)
(setq p20 '((0 0 0 1 1 1 1 0 0)
	    (1 1 1 1 0 0 1 1 0)
	    (1 0 0 0 2 0 0 1 0)
	    (1 0 0 5 5 5 0 1 0)
	    (1 0 0 4 0 4 0 1 1)
	    (1 1 0 5 0 5 0 0 1)
	    (0 1 1 5 5 5 0 0 1)
	    (0 0 1 0 2 0 1 1 1)
	    (0 0 1 0 3 0 1 0 0)
	    (0 0 1 1 1 1 1 0 0)))

;(??,??)
(setq p21 '((0 0 1 1 1 1 1 1 1 0)
	    (1 1 1 0 0 1 1 1 1 0)
	    (1 0 0 2 0 0 0 1 1 0)
	    (1 3 2 0 2 0 0 0 1 0)
	    (1 1 0 2 0 2 0 0 1 0)
	    (0 1 1 0 2 0 2 0 1 0)
	    (0 0 1 1 0 2 0 0 1 0)
	    (0 0 0 1 1 1 1 0 1 0)
	    (0 0 0 0 1 4 1 0 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 0 1 4 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 1 1 1 1 1)))

;(??,??)
(setq p22 '((0 0 0 0 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 2 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 1 1 1 0 0 2 1 1 0 0 0 0 0 0 0 0 0)
	    (0 0 1 0 0 2 0 2 0 1 0 0 0 0 0 0 0 0 0)
	    (1 1 1 0 1 0 1 1 0 1 0 0 0 1 1 1 1 1 1)
	    (1 0 0 0 1 0 1 1 0 1 1 1 1 1 0 0 4 4 1)
	    (1 0 2 0 0 2 0 0 0 0 0 0 0 0 0 0 4 4 1)
	    (1 1 1 1 1 0 1 1 1 0 1 3 1 1 0 0 4 4 1)
	    (0 0 0 0 1 0 0 0 0 0 1 1 1 1 1 1 1 1 1)
	    (0 0 0 0 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Utility functions for printing states and moves.
 | You do not need to understand any of the functions below this point.
 |#

;
; Helper function of prettyMoves
; from s1 --> s2
;
(defun detectDiff (s1 s2)
  (let* ((k1 (getKeeperPosition s1 0))
	 (k2 (getKeeperPosition s2 0))
	 (deltaX (- (car k2) (car k1)))
	 (deltaY (- (cadr k2) (cadr k1)))
	 )
    (cond ((= deltaX 0) (if (> deltaY 0) 'DOWN 'UP))
	  (t (if (> deltaX 0) 'RIGHT 'LEFT))
	  );end cond
    );end let
  );end defun

;
; Translates a list of states into a list of moves.
; Usage: (prettyMoves (a* <problem> #'goal-test #'next-states #'heuristic))
;
(defun prettyMoves (m)
  (cond ((null m) nil)
	((= 1 (length m)) (list 'END))
	(t (cons (detectDiff (car m) (cadr m)) (prettyMoves (cdr m))))
	);end cond
  );

;
; Print the content of the square to stdout.
;
(defun printSquare (s)
  (cond ((= s blank) (format t " "))
	((= s wall) (format t "#"))
	((= s box) (format t "$"))
	((= s keeper) (format t "@"))
	((= s star) (format t "."))
	((= s boxstar) (format t "*"))
	((= s keeperstar) (format t "+"))
	(t (format t "|"))
	);end cond
  )

;
; Print a row
;
(defun printRow (r)
  (dolist (cur r)
    (printSquare cur)    
    )
  );

;
; Print a state
;
(defun printState (s)
  (progn    
    (dolist (cur s)
      (printRow cur)
      (format t "~%")
      )
    );end progn
  )

;
; Print a list of states with delay.
;
(defun printStates (sl delay)
  (dolist (cur sl)
    (printState cur)
    (sleep delay)
    );end dolist
  );end defun
