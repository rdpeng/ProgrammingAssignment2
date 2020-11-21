## makeCacheMatrix() creates a special matrix and CacheSolve() caches
## the inverse of the matrix created by makeCacheMatrix. If the 
## special matrix is reset, then CacheSolve recalculates the inverse

## makeCacheMatrix() creates a special matrix.

##  Inorder to use this function properly, you will have to separately create a 
## matrix variable first and then apply makeCacheMatrix() onto it. 
## For e.g.: a<- matrix(1:4,2,2) and then you can write,
## mymatrix<- makeCacheMatrix(a)


makeCacheMatrix <- function(x = matrix()) {
  inv<<- NULL
  set<- function(y){
    x<<- y
    inv<<- NULL
  }
  get<- function() x
  setInv<- function(Inverse){inv<<-Inverse}
  getInv<- function() inv
  list(set= set, get= get, setInv=setInv, getInv=getInv)

}


## CacheSolve() caches the inverse of the matrix created by makeCacheMatrix().
##  If the special matrix is reset, then CacheSolve()
##recalculates the inverse
 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<- x$getInv()
  if(!is.null(inv)){
    message('getting cached data')
    return(inv)
  }
  data<- x$get()
  inv<- solve(data)
  x$setInv(inv)
  inv
}
## a commit for github desktop