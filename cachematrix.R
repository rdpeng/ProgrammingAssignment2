## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(m = matrix()) {
        i <- NULL
  
        set <- function(n) {
            m <<- n
            i <<- NULL
        }
        get <- function() m
        setInv <- function(inv) i <<- inv
        getInv <- function() i
          
        list(set=set, get=get, setInv=setInv, getInv=getInv)
}


## Write a short comment describing this function

cacheSolve <- function(m, ...) {
        i <- m$getInv()
  
  if(!is.null(i)) {
    message("Getting the Inverse from Cached Data")
    return(i)
  }
  data <- m$get()
  #m$set(mat)
  i <- solve(data)
  m$setInv(i)
  i
}
