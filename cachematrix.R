# Set of two functions to cache, set, and get the inverse of a given matrix

## Function to create a list of functions to set and get both 
##a matrix and that matrix's inverse

makeCacheMatrix <- function(x = matrix()) {
  im <- NULL
  set <- function(y){
    x<<-y
    im <<- NULL 
  }
  get <- function() {x}
  setim <- function(inversematrix) {im <<- inversematrix}
  getim <- function() {im}
  list(set=set, get=get, setim=setim, getim=getim)
}


## Function to cache the inverse of matrix x in object im

cacheSolve <- function(x, ...) {
  im<-x$getim()
  if(!is.null(im)){
    message("getting cached data")
    return(im)
  }
  matr<-x$get()
  im<-solve(matr, ...)
  x$setim(im)
  im
}
