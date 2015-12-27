## The functions below can be used to process results faster because it caches the results

## Creates a special matrix
makeCacheMatrix <- function(x = matrix()) {
	m<-NULL

	set<-function(y){
		x<<-y
		m<<-NULL
	}

	get<-function() x
	setmatrix<-function(solve) m<<- solve
	getmatrix<-function() m

	list(set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix)
}


## Cache the result
cacheSolve <- function(x, ...) {
	m<-x$getmatrix()
    if(!is.null(m)){
      message("getting cached data")
      return(m)
    }
    matrix<-x$get()
    m<-solve(matrix, ...)
    x$setmatrix(m)

    ## Return a matrix that is the inverse of 'x'
    m
}
