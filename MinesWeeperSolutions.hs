type Cell = (Int,Int)
data MyState = Null | S Cell [Cell] String MyState deriving (Show,Eq)


delete x []=[]
delete x (h:t) = if(x==h) then t
				 else (h:delete x t)

up (S (x,y) lcell string oldstate)= if(x>0) then ((S ((x-1),y) lcell "up" (S (x,y) lcell string oldstate)))
									else Null 
					
down (S (x,y) lcell string oldstate)= if(x<4) then (S ((x+1),y) lcell "down" (S (x,y) lcell string oldstate))
									  else Null
								
left (S (x,y) lcell string oldstate)= if(y>0) then (S (x,(y-1)) lcell "left" (S (x,y) lcell string oldstate))
									  else Null

right (S (x,y) lcell string oldstate)= if(y<4) then (S (x,(y+1)) lcell "right" (S (x,y) lcell string oldstate))
									  else Null
																	  								  
collect (S (x,y) lcell string oldstate)= if(elem (x,y) lcell) then (S (x,y) (delete (x,y) lcell) "collect" (S (x,y) lcell string oldstate))
										 else Null

removeNulls []=[]
removeNulls (h:t)= if(h==Null) then removeNulls t
					else (h:removeNulls t)

nextMyStates state= removeNulls [up state,down state,left state,right state,collect state] 
										 
isGoal (S (x,y) lcell string oldstate)= if(length lcell==0) then True
										else False
search []=Null										
search (h:t)= if(isGoal h) then h
			  else search (t ++ nextMyStates h)
			  
constructSolution (S (x,y) lcell string Null)=[]
constructSolution (S (x,y) lcell string oldstate)= constructSolution oldstate ++ [string]

solve pos mine= constructSolution (search [(S pos mine "" Null)])	


										