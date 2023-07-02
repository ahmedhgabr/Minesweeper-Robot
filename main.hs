type Cell = (Int,Int)
data MyState = Null | S Cell [Cell] String MyState deriving (Show, Eq)

 

up :: MyState -> MyState
up (S (x,y) l s m) = if x <= 0 then Null
					 else S (x-1,y) l "up"	(S (x,y) l s m)

down :: MyState -> MyState
down (S (x,y) l s m) = if x >= 3 then Null
					   else S (x+1,y) l "down"	(S (x,y) l s m)

left :: MyState -> MyState
left (S (x,y) l s m) = if y <= 0 then Null
					   else S (x,y-1) l "left"	(S (x,y) l s m)

right :: MyState -> MyState
right (S (x,y) l s m) = if y >= 3 then Null
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
					 
					 
nextMyStates :: MyState->[MyState]
nextMyStates ms = filter (/= Null) [up ms,down ms,left ms ,right ms ,collect ms]				 
					 

isGoal :: MyState->Bool					 
isGoal (S c [] s m) = True
isGoal (S c l s m) = False

search :: [MyState]->MyState
search (h:t) = if isGoal h then h
			   else search (t ++ nextMyStates h) 

constructSolution :: MyState -> [String]
constructSolution (S c l "" m) = [] 
constructSolution (S c l s m) = constructSolution m ++ [s] 

			
solve :: Cell -> [Cell] -> [String]
solve (x,y) [(x1,y1),(x2,y2)] = if x > 3 || x < 0 || y > 3 || y < 0 || x1 > 3 || x1 < 0 || y1 > 3 || y1 < 0 || x2 > 3 || x2 < 0 || y2 > 3 || y2 < 0 then error "Please enter a number between 3 and 0"
								else constructSolution (search ([S (x,y) [(x1,y1),(x2,y2)] "" Null]))




















					 
					 
					 
					 
					 