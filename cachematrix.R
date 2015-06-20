## First function creates the required vector and second one calculates inverse
## by calculating and storing to cache or retreiving from cache (if available)

## Function returns a vector containing list of functions get(), set(), 
## getinverse() and setinverse())
## It also contains the input matrix stored as a matrix (x)

makeCacheMatrix <- function(x = matrix()) { ##input matrix specified as argument
  inverse<-NULL 
    ## initially inverse set as NULL to allow for creating new matrices
    ## which have no inverse inbuilt
  set<- function(y){ ## stores the input matrix x
    x<<-y
    inverse<<-NULL
  }
  get<-function() x
  setinverse<- function(inverse){ ## function to allow cachesolve to set inverse
    inverse<<-solve
  }
  getinverse<- function() inverse
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse ) 
  ## the return output as a list
}


## Gets the inverse if its cached and calculates inverse using solve if not

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse<- x$getinverse() ## To get inverse if it is cached
  if(!is.null(inverse)){ ## To check for cache
    message("Getting cache data ...")
    return(inverse) 
  }
  data<- x$get() ## Gets matrix for computing inverse
  inverse<- solve(data,...)
  x$setinverse(inverse) ##Stores inverse 
  inverse
}
