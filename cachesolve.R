cachesolve<- function(x, ...) {
  solve <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(solve)
  }
  data <- x$get()
  solve<- solve(mat.data, ...)
  x$setsolve(solve)
 return(solve)
}