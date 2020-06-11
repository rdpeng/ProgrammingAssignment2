##Programming Assignment 2(by Piyush P)


## Solution 1(Function creating a special matrix object that can cache its inverse)

makeCacheMatrix<- function(x=matrix()){                     
  
  inv<- NULL
  set<- function(y){
    
    x<<- y
    inv<<- NULL
    
  }
  
  get<- function() {x}
  setInv<- function(inverse) {inv<<- inverse}
  getInv<- function() {inv}
  list(set = set, get = get,setInv= setInv, getInv = getInv )
  
}
## Solution 2(Function computing the inverse of the special matrix returned by the above function)

cacheSolve <- function(x, ...){
  inv<- x$getInv()
  if(!is.null(inv)){
    return(inv)
  }
  
  id<- x$get()
  inv<- solve(id,..)
  x$setInv(inv)
  inv
  
}