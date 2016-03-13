cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
   mat<- x$get()
   inv<- solve(mat,... =)
   x$setInverse(inv)
   inv
}

