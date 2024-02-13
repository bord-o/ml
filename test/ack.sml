
val rec ackermann = fn m => fn n =>
	if m=0 then 
		n+1 
	else if m > 0 andalso n=0 then 
		ackermann (m-1) 1 
	else if m > 0 andalso n > 0 then
		ackermann (m-1) (ackermann m (n-1))
	else 99

val res = ackermann 3 3
 
