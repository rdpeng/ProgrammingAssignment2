##Funcation to set and get Matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <-function(y){  ## sets the value of x and inverse of X
    
    x <<- y
    inv <<- NULL
    
  } 
  get<- function() x ##gets the value of x
  
  setInverse <- function(inverse) inv<<- inverse ## to set inverse of x
  
  getInverse <-function() inv ## to get the inverse of x
  
  list(get=get, set=set,setInverse=setInverse,getInverse=getInverse)

}


##Uses solve function of R to calculate the inverse of square matrix  

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  
  inv<- x$getInverse()
  if(!is.null(inv)){
    message("Gettting cached data")
    return(inv)
    
  }
  mat <- x$get()
  
  inv<-solve(mat,...)
  x$setInverse(inv) 
  return(inv)
  
}
