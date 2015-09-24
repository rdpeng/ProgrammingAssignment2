## Put comments here that give an overall description of what your
## functions do

##makeCacheMatrix stores the input and its inverse.
##cacheSolve first detects if the inverse of the input has already been solved.
##If so, it returns the inverse stored in makeCacheMatrix
##If not, it calculates the inverse and stores it in makeCacheMatrix

## Write a short comment describing this function

## makeCacheMatrix returns a list of functions: xset,xget, inverseset and inverseget.
## It caches a matrix x and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  xinverse<-NULL
  xset<-function(y){
    x<-y
    xinverse<-NULL
  }
  xget<-function() x
  inverseset<-function(inverse) xinverse<<-inverse
  inverseget<-function() xinverse
  list(
    xset=xset,
    xget=xget,
    inverseset=inverseset,
    inverseget=inverseget)

}


## Write a short comment describing this function

##cacheSolve first detects if the inverse of the input has already been solved.
##If so, it returns the inverse stored in makeCacheMatrix
##If not, it calculates the inverse and stores it in makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  xinverse<-x$inverseget()
  
  if(!is.null(xinverse)){
    "getting cached data"
    return(xinverse)
  }
  
  data<-x$xget
  xinverse<-solve(data)
  x$inverseset(xinverse)
  return (xinverse)
  
}
