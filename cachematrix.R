## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  object <- NULL #This creates an 'empty object that will store the value of the matrix'
  set <- function(y){ #another function(nested funtion) that is used to assign value of the 
    x <<- y      #matrix in another environment using <<-, so here it assigns x to y(y is in anothter env.)
    object <<- NULL #same as above
  }
           get <- function() x #this gets the value of our matrix
            setInverse <- function(inverse) object <<- inverse #here we sets the value of the inverse of our matrix and stick #to another environment called inverse
      getInverse <- function() object #this gets the inverse of our matrix                   
           list(set = set, get = get, 
       setInverse = setInverse, ##so in the end, the function returns 1. the set value,object. 2. get, x 3. setInverse, inverse(filled object), 4. getInverse(the inverse of x)
           getInverse = getInverse)
}


## this computes the inverse of a special matrix returned by the above function. If the inverse has already 
##been calculated, and the matrix is still the same, then this function will just go and just take the inverse from
## the already saved inverse(the cache)


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  object <- x$getInverse() #this takes the inverse from the above function--the matrix inverse you already calculated
  
  if(!is.null(object)){ #meaning--if the inverse has already been calculated then get it from the saved(cached) data
    message("getting cached data")#and return the inverse as it is.
    return(object)
  }
  mat <- x$get()
  object <- solve(mat,...) #this will recreate the matrix if it has not already been created
  x$setInverse(object)     # and prints it out
  object
}

