

##################################################################
#       Functions to find inverse of matrix with caching        ##
##################################################################
##complete function returns inverse of only invertable matrix.
##stores it in cache memory for fast retreval for same matrix without unecessary computation.


## makeCacheMatrix ##

#set inverse matrix value in first iteration to parent env using '<<-' operator.
## fast retrieval of cached inv matrix value using getinverse function.
# Function returns a list containing four functions:
# 1. set()
# 2. get()
# 3. getinverse()
# 4. setinverse()
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

#########################################################################################
##  cacheSolve ##
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
        
#  Example   #   
# z<-matrix(rnorm(9),3,3)
# x<-makeCacheMatrix(z)
# cacheSolve(x)

