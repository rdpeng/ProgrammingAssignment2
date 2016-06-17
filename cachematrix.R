## The purpose of this script is to create a cache of the inverse of a matrix 
## to help save on computation time

## Creates a special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inver<-NULL
    
    set<- function(y){
      x <<- y
      inver <<- NULL
    }
  
    get <- function() x
    setinverse <- function(inverse) inver<<-inverse
    getinverse <- function() inver
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Computes the inverse of the special "matrix" returned by the
## makeCacheMatrix above.  If inverse has already been calculated the
## function will retrieve inverse from the cache

cacheSolve <- function(x, ...) {
        inver <- x$getinverse()
        
        if(!is.null(inver)){
          message("getting cached data")
          return(inver)
        }
        
        data <- x$get()
        inver <- solve(data, ...)
        x$setinverse(inver)
        inver
}
