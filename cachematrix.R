
## makeCacheMatrix creates a special "matrix" object that can cache its inverse
## with set of functions: set - set matrix , get -get matrix, 
## setInv - set inverse of matrix, getInv - get inverse of matrix 

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y)
  {
    x<<-y
    inv<<- NULL
  }
  get <- function() x
  setInv <- function(inverse) inv<<-inverse
  getInv <- function() inv
  list(set=set, get=get,setInv=setInv,getInv=getInv)
}


## function computes the inverse of special matrix returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        inv<-x$getInv()
        if(!is.null(inv))
        {
          message("getting cached data")
          return(inv)
        }
        matrix <- x$get()
        inv<-solve(matrix)
        x$setInv(inv)
        inv
        ## Return a matrix that is the inverse of 'x'
}

