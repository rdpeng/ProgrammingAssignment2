## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL ## initializing
  set<-function(y){ ## define set function
    x<<-y  ## value in parent environment
    i<<-NULL ##initializing in parent environmen
  }
  get<-function() i
  setInverse<-function(inverse) i<-solve  ## assign value of inverse in parent environment
  getInverse<-function() i
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i<-x$getInverse() ## use getInverse Feature
  if(!is.null(m)){ ## check if calculating inverse is possible
    message("getting cached data")
    return(i)  ## if not: print initial matrix
  
  }
  data<-x$get()
  i<-solve(data,...)  ## if not: print initial matrix
  x$setInverse(i)
  i
}
