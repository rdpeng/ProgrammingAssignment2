## The first function contains three functions that set, get or invert the matrix inserted as 'x'. Moreover, the outcome is commited to cache. 
## The second function either returns an inverse of x already in cache or inverts and returns x

## Function 1

makeCacheMatrix <- function(x = matrix()) {
  
  set<- function(y){
    inverse<<- NULL
    x<<- y
  }  
  
  get<- function(){
    
    inverse<<- NULL
    x<<- x
    return(x)
  }
  
  inv<- function(){
    
    inverse <<- solve(x)
    return(inverse)
  }
  
  list(set = set, get = get, inv = inv)
  
}


## Function 2

cacheSolve <- function(x, ...) {
  
  if (!is.null(inverse)){
    
    message("getting cached data")
    return(inverse)
    
  }
  
  else if(is.null(inverse)){
    
    a<- x$get()
    b<- solve(a)
    return(b)
    
  }
}
  