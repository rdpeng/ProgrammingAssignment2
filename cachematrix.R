##Creating the matrix
##creating a function "invr" to solved value as null
makeCacheMatrix <- function(x = matrix(sample(1:100,9),3,3)){
  invr <- NULL
  crea <- function(y) {
    x <<- y
    invr <<- NULL
  }
  obt <- function() x
  settheinverse <- function(inverse) {invr <<- inverse} ##As you can see the "mean" is changed to "inverse"
  gettheinverse <- function() invr
  list(crea = crea, obt = obt, settheinverse = settheinverse, gettheinverse = gettheinverse)
}
##As i said earlier all "mean" function are changed to "inverse" which is "m" to "invr"
cacheSolve <- function(x, ...) {
  invr <- x$gettheinverse()
  if(!is.null(invr)) {
    message("calculating the inversed numeric") ##This message will appear when it tries to load
    return(invr)
  }
  data <- x$obt()
  invr <- solve(data, ...)
  x$settheinverse(invr)
  invr
}