## Put comments here that give an overall description of what your
## functions do
##complete function returns inverse of an invertable matrix.
##stores it in cache memory for fast retreval for same matrix without unecessary computation.
##

## Write a short comment describing this function

#set inverse matrix value in first iteration to parent env using '<<-' operator.
## fast retrieval of cached inv matrix value using getinverse function.
makeCacheMatrix <- function(x = matrix()) 
{
  inv<-NULL
set<-function(y=matrix())  ## set value of matrix using set function
  {
  x<<-y
  inv<<-NULL
  }
  get<-function() x    ## get data of matrix to be inverted
  getinverse<-function() ## get cached inverse matrix  
  {
    inv
  }
  setinverse<-function(inverse) ## Set inverse matrix value in first iteration to cache.
    {
    inv<<-inverse
    }

  list(set=set,get=get,getinverse=getinverse,setinverse=setinverse)
}


## Write a short comment describing this function
##generates inv of matrix in first iteration.
##retrieve cached matrix value using function call
cacheSolve <- function(x, ...) {
  inv<-x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat)
  x$setinverse(inv)
  inv
  }
        ## Return a matrix that is the inverse of 'x'

