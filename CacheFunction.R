makeCacheMatrix <- function (x = matrix ()) {
  inv <- NULL
  set <- function(y){
    x <<-y
    inv <<-NULL
  }
  get <- function()x
  setinv <- function(solve) inv <<-solve
  getinv <- function()m
  list (set=set, get=get, sitinv=setinv, getinv=getinv)
}

cacheMatrix <- function (x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setmean(inv)
  inv
}