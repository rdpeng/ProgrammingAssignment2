## Matrix inverse code 
##This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) 

## Initialize the inverse property
  m<-NULL
  
## set funciton to Set a matrix
  set<-function(y){
  x<<-y
  m<<-NULL
}

## get funciton to Get a matrix
get<-function() 

##Return the matrix
{ x }

## set and get the inverse of the matrix
setmatrixinverse<-function(solve)
{ m<<- solve }

getmatrixinverse<-function() 
 ## Return the inverse property
 { m }
 
 ## Return a list of the methods
list(set=set, get=get,
   setmatrixinverse=setmatrixinverse,
   getmatrixinverse=getmatrixinverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x=matrix(), ...) {
## Return a matrix that is the inverse of matrix 'x'

    m<-x$getmatrixinverse()
## Return the inverse from cache, if its already set

    if(!is.null(m)){
      message("getting cached data")
      return(m)
    }
 ## Get the matrix from our object
    data<-x$get()
    
## To calculate inverse using matrix multiplication
    m<-solve(data, ...)
    
    x$setmatrixinverse  (m)
## Return the matrix    
    m
}
