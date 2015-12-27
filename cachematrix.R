## Programming assignment 2

## The following two functions will create a square invertible matrix, 
## make the inverse of that matrix available in the cache, and 
## return the cached inverse

##makeCachematrix creates and returns a list of functions
##to set and get the matrix and its inverse
makeCachematrix <- function(x = matrix()){
  cacheinverse <- NULL
  set <- function(y) {
    x <<- y
    cacheinverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) cacheinverse <<- inv
  getInverse <- function() cacheinverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## cacheSolve checks if the inverse is in cache
## by calling the getInverse function above
## If it is, it will return the cached value
## If it is not, it will solve the matrix and store it in cache
## by using the setInverse function above
## This assumes the matrix is square and invertible 

cacheSolve <- function(x, ...){
  inverse <- x$getInverse()
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data)
  x$setInverse(inverse)
  inverse
}

## Example run of functions
## source('cacheMatrix.R')                      Loads program
##
## test <- makeCacheMatrix()                    creates functions
## test$set(matrix(runif(9, -10, 10), 3, 3))    creates the matrix
##
## test$get()                                   returns the matrix just created
##            [,1]      [,2]      [,3]
## [1,] -0.6213507 -3.230644 -6.480540
## [2,]  1.1672411 -5.050263 -7.092414
## [3,]  1.5997631  7.427071 -2.138685
##
## cacheSolve(test)                             1st run returns calculated inverse matrix
##             [,1]         [,2]        [,3]
## [1,] -0.53167836  0.461018654  0.08221302
## [2,]  0.07412566 -0.097966805  0.10027041
## [3,] -0.14028382  0.004635667 -0.05786882
##
## cacheSolve(test)                             2nd run returns cached inverse matrix
## getting cached data
##          [,1]         [,2]        [,3]
## [1,] -0.53167836  0.461018654  0.08221302
## [2,]  0.07412566 -0.097966805  0.10027041
## [3,] -0.14028382  0.004635667 -0.05786882