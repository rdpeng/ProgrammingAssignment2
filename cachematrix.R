#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix  <-function(x) {
	
		
	##check if x is a matrix	
	if(!is.matrix(x)){ stop("x must be a matrix") }
	
	#check if it is a square matrix	
	if(nrow(x) != ncol(x)){ stop("x must be a square matrix")  }
	
	#declare a variable to hold the inverse
	inverse <- NULL
	
	#function set saves values in the special environment
	set <- function(y){
			x <<- y
			inverse <<- NULL
	}

	
	#get-function returns the matrix x 
	get <- function() x
	
	#assign the calculated inverse of the matrix  to the special variable
	setinverse <- function(inversematrix) inverse <<- inversematrix 
	
	#getinverse  returns the special inverse matrix 
	getinverse <- function() inverse
	
	#the function returns a list of elements as follows:
	list(set=set, get=get, getinverse= getinverse, setinverse=setinverse)
	
}

#cacheSolve: This function computes the inverse of the special "matrix" saved by makeCacheMatrix above.

cacheSolve <- function(x,...) {
	#in order to use this function, x must already be processed by the makeCacheMatrix function
	#otherwise it gives an error
	
	#verify if x is of type list?
	if(!is.list(x)){
		stop("the argument to cacheSolve must be a list")
	}
	
	#look if x has a cached inverse
	myinverse <- x$getinverse()
	
	if ( !is.null(myinverse )) {
		message("getting cached inverse matrix")
		return(myinverse)
	}
	
	message("calculating inverse matrix")
	
	mymatrix <- x$get()
	
	#calculate the inverse
	myinverse <- solve(mymatrix)
	
	#asigne the inverse to the special variable
	x$setinverse(myinverse)
	
	#return the inverse
	myinverse
}

#testCache is a testfunction that allows for regression testing of the functions implemented above
testCache <- function(){
	
	x3 <-rbind(c(1,2,0), c(2,4,1), c(2,1,0))
	print(class(x3))
	
	#asign the special environment variables
	m3 <- makeCacheMatrix(x3)
	message("get cached x")
	print(m3$get())
	
	message("get inital value for inverse")

	print(m3$getinverse())
	
	#calculate the inverse for the first time
	i3 <- cacheSolve(m3)
	print(i3)
	
	#retrieve the inverse from the cache for the second time
	ic3 <- cacheSolve(m3)
	
	print(ic3)
	
	#verify the inverse == it must return a matrix with 1 on the diagonale
	
	message("verify the inverse")
	x3 %*% i3
	
	
	
}
