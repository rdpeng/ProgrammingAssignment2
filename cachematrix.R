##-----------------------------------------------------------------------------

## An overall Description about Program

## Here in this below program I am using the concept of Caching the data objects.
## So if I want to fetch the inverse of matrix which is already calculated previously
## then I have not to re-calculate it again.I can fetch it using caching.

##-----------------------------------------------------------------------------

##-----------------------------------------------------------------------------

## An overall Description of makeCacheMatrix

## This function is use to cache the inverse of matrix and fetch it back again 
## whenever we want to do so

makeCacheMatrix <- function(x = matrix()) 
                  {
                      invers <- NULL
                      set <- function(y) 
                            {
                                x <<- y     ## Assigning matrix to x
                                invers <<- NULL
                            }
                      get <- function() x   ## Returning the matrix
                      setinverse <- function(inverse) invers<<- inverse  ## Caching the inverse of matrix "x"
                      getinverse <- function() invers  ## Returning the cached inverse matrix of "x"
                      list
                      (    
                            set = set, get = get,
                            setinverse = setinverse,
                            getinverse = getinverse
                      )
                  }

##-----------------------------------------------------------------------------

##-----------------------------------------------------------------------------

## Description of cacheSolve Function

## In this function first we are checking that inverse of that matrix is present 
## or not with the help of above function.
## If it is present then we will get inverse from it else we will derive it using
## solve() function and for future we will cache it.

cacheSolve <- function(x, ...) 
              {
                  invers <- x$getinverse() ## Fetching inverse of matrix
                  if(!is.null(invers)) ## If it is then invers will not be equal to null
                  {
                      message("getting cached data")
                      return(invers) ## Returning cached inverse of matrix "x"
                  }
                  data <- x$get() ## else fetch matrix 
                  invers <- solve(data, ...) ## calculate inverse of matrix "x"
                  x$setinverse(invers) ## cachinng inverse of matrix "x"
                  invers   ## Returning a matrix that is the inverse of 'x'
              }
##-----------------------------------------------------------------------------              }

