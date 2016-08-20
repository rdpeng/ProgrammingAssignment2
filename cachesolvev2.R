cachesolve<- function(x, ...) {
  ##RETUREN THE INVERSE OF A MATRIX 'X'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  mat <- x$get()
  m <- solve(mat, ...)
  x$setsolve(solve)
 return(m)
}