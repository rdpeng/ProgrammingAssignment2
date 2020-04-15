## Put comments here that give an overall description of what your
## functions cache the inverse of a matrix

## Write a short comment describing this function
##it create matrix object that cache the inverse

makeCacheMatrix <- function(x1 = mat()) {
        inver <- NULL
  set <- function(y1){
    x1 <<- y1
    inver <<- NULL
  }
  get <- function() x1
  setInverse <- function(solveMatrix) inver <<- solveMatrix
  getInverse <- function() inver
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## Write a short comment describing this function
##it returns inverse of matrix returned in above makeCacheMatrix

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
  if(!is.null(inver)){
    message("getting cached data")
    return(inver)
  }
  data <- x$get()
  inver <- solve(data)
  x$setInverse(inver)
  inver     
        ## Return a matrix that is the inverse of 'x'
}
