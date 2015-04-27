#################################################################################################################
#                                      Function Name:makeCacheMatrix						#
#                   Desc:This function creates a special "matrix" object that can cache its inverse.		#
#################################################################################################################

makeCacheMatrix <- function(x = matrix()) {
  # inv will store the cached inverse matrix
  inverse <- NULL
  
  # Setter for the matrix
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  # Getter for the matrix
  get <- function() x
  
  # Setter for the inverseerse
  setinverse <- function(inverseerse) inverse <<- inverseerse
  # Getter for the inverseerse
  getinverse <- function() inverse
  
  # Return the matrix with our newly defined functions
  return(list(set = set, get = get, setinverse = setinverse, getinverse = getinverse))
}
#################################################################################################################
#                                      Function Name:makeCacheMatrix						#
#       Desc:This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.	#
#	If the inverse has already been calculated (and the matrix has not changed),				#
#	then the cachesolve should retrieve the inverse from the cache.						#
#################################################################################################################


cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  
  # If the inverseerse is already calculated, return it
  if (!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  # The inverseerse is not yet calculated, so we calculate it
  data <- x$get()
  inverse <- solve(data, ...)
  
  # Cache the inverseerse
  x$setinverse(inverse)
  
  # Return it
  return(inverse)
}

#################################################################################################################
#                                      Sample Output								#
#################################################################################################################

> x<-matrix(c(7,8,9,0,-1,2,-2,3,-7),3,3)
> x
     [,1] [,2] [,3]
[1,]    7    0   -2
[2,]    8   -1    3
[3,]    9    2   -7
> Matrixcache<-makeCacheMatrix(x)
> cacheSolve(Matrixcache)
            [,1]       [,2]       [,3]
[1,] -0.02325581 0.09302326 0.04651163
[2,] -1.93023256 0.72093023 0.86046512
[3,] -0.58139535 0.32558140 0.16279070


#################################################################################################################
#                                      Sample Output with error							#
#				Reason : Insverse of matrix is possible only on Square matrix			#
#################################################################################################################

> x<-matrix(c(7,8,9,0,-1,2,-2,3,-7),3,2)
Warning message:
In matrix(c(7, 8, 9, 0, -1, 2, -2, 3, -7), 3, 2) :
  data length [9] is not a sub-multiple or multiple of the number of columns [2]
> x
     [,1] [,2]
[1,]    7    0
[2,]    8   -1
[3,]    9    2
> Matrixcache<-makeCacheMatrix(x)
> cacheSolve(Matrixcache)
 Show Traceback
 
 Rerun with Debug
 Error in solve.default(data, ...) : 'a' (3 x 2) must be square 
