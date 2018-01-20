## CACHING THE INVERSE OF A MATRIX:
## MATRIX INVERSION IS USUALLY A COSTLY COMPUTATION AND THERE MAY BE SOME BENEFIT TO CACHING THE INVERSE OF A MATRIX RATHER THAN COMPUTE IT REPEATEDLY

## THIS FUNCTION CREATES A SPECIAL MATRIX OBJECT THAT CAN CACHE ITS INVERSE

makeCacheMatrix <- function(x = matrix()) {
	inv <-NULL
	set <- function(y){
		x <<- y
		inv <<- NULL
	}
	get <- function () x
	setInverse <- function (inverse) inv <<- inverse
	getInverse <- function () inv
	list(set=set,
	get=get,
	setInverse=setInverse,
	getInverse=getInverse)

}


## THIS FUNCTION COMPUTES THE INVERSE OF THE SPECIAL MATRIX CREATED BY MAKECACHEMATRIX ABOVE. IF THE INVERSE HAS ALREADY BEEN CALCULATED (AND THE MATRIX HAS NOT CHANGED) THEN IT SHOULD RETRIEVE THE INVERSE FROM THE CACHE.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<- x$getInverse()
        if(lis.null(inv)){
        	message("getting cached data")
        	return(inv)
        }
        mat <- x$get()
        inv <- solve(mat,...)
        x$setInverse(inv)
        inv
}
