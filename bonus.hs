type Cell = (Int,Int)
data MyState = Null | S Cell [Cell] String MyState deriving (Show, Eq)

main = do
	putStrLn "Please enter a size"
	sizex <- getLine
	sizey <- getLine
	putStrLn "Thanks"
	

up :: MyState -> MyState
up (S (x,y) l s m) = if x <= 0 then Null
					 else S (x-1,y) l "up"	(S (x,y) l s m)

down :: MyState -> MyState
down (S (x,y) l s m) = if x >= 9 then Null
					   else S (x+1,y) l "down"	(S (x,y) l s m)

left :: MyState -> MyState
left (S (x,y) l s m) = if y <= 0 then Null
					   else S (x,y-1) l "left"	(S (x,y) l s m)

right :: MyState -> MyState
right (S (x,y) l s m) = if y >= 9 then Null
						else S (x,y+1) l "right"	(S (x,y) l s m)					 
					 

collect :: MyState -> MyState
collect (S c l s m) = if found c l then S c (remove c l) "collect" (S c l s m) 
					  else Null	
							

						
found c (h:t) = if c == h then True else 
				  if t == [] then False
				  else found c t	

remove c [] = [] 				  
remove c (h:t) = if c == h then t 
				 else (h:remove c t) 	

					
					 
distance(x,y) (x1,y1) = abs(x1-x) + abs(y1-y)

nearest c [] nearestsofar = nearestsofar
nearest c (h:t) nearestsofar = if (distance c h) < (distance c nearestsofar) then nearest c t h
							   else nearest c t nearestsofar

		   
movev (S (x,y) l s m) (x1,y1) = if x1 > x then movev (down (S (x,y) l s m)) (x1,y1) 
								else if x1 < x then movev (up (S (x,y) l s m)) (x1,y1) 
								else S (x,y) l s m

										   
moveh (S (x,y) l s m) (x1,y1) = if y1 > y then moveh (right (S (x,y) l s m)) (x1,y1) 
								else if y1 < y then moveh (left (S (x,y) l s m)) (x1,y1)
								else S (x,y) l s m

								
move (S c l s m) = moveh (movev (S c l s m) (nearest c l (1000,1000))) (nearest c l (1000,1000))


search state = if isGoal state then state
			   else search (collect (move state))
				 
isGoal :: MyState->Bool					 
isGoal (S c [] s m) = True
isGoal (S c l s m) = False



constructSolution :: MyState -> [String]
constructSolution (S c l "" m) = [] 
constructSolution (S c l s m) = constructSolution m ++ [s] 

			
solve :: Cell -> [Cell] -> [String]
solve (x,y) l = if x > 9 || x < 0 || y > 9 || y < 0 then error "Please enter a number between 9 and 0"		
			else constructSolution (search (S (x,y) l "" Null)) 
