##Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix 
##rather than compute it repeatedly (there are also alternatives to matrix inversion that we will not discuss here).
##Your assignment is to write a pair of functions that cache the inverse of a matrix.

##Write the following functions:

##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {  ##define the function
  inv<-NULL   ##give null value to inv before you set the function
  set<-function(y){
    x<<-y   ##transit the set function workspace to the make function workspace
    inv<<-NULL
  }
  get<-function() x
  setInverse<-function(inverse) inv <<-inverse
  getInverse<-function() inv ##define the setInverse function and the getInverse Function
  list(set=set,
       get=get,
       setInverse=setInverse,
       getInverse=getInverse)
}


## ##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has 
##already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<-x$getInverse()
        if(!is.null(inv)){
          message("getting cached data")##check if the inverse value has been calculated already
          return(inv)
        }
        mat<-x$get()
        inv<-solve(mat,...)
        x$setInverse(inv)
        inv     ## return inv
}
