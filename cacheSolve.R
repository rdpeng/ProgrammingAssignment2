cacheSolve <- function(A, ...){
	m<- A$getinv()
	if(!is.null(m)){
		message("inverse matrix cached")
		return(INV)
	}
	data<- A$get()
	INV<<- solve(data)
	A$setinv(1)
	INV
		

}
