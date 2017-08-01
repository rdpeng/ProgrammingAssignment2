## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

}


## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
      m  <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
          }
      get <- function() x
      setinverse <- function(inverse) m <<- inverse
      getinverse <- function() m
      list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
    }

sum <- function(a,b) { 
    return(a+b)
}

mul <- function(a,b){
    return(a*b)
}
# end of the program , output coming as expected 