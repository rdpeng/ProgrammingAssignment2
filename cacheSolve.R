cacheSolve <- function(x, ...) {
  x_inv <- x$getinverse()
  if(!is.null(x_inv)) {
    message("getting cached data.")
    return(x_inv)
  }
  data <- x$get()
  x_inv <- solve(data)
  x$setinverse(x_inv)
  x_inv
}
