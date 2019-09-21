## makeCachematrix is a fuction that caches the inverse of a given matrix. it does so in the following order

## first it sets the value of the given matrix
## then it gets the value otf that matrix
## then it sets the value of inverse of that matrix
## finally it returns/get the value of inverse of the matrix



makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) { ## setting the vaue of matrix
    x <<- y
    i <<- NULL
  }
  get <- function() x ## getting the value of the matrix
  setinverse <- function(inverse) i <<- inverse  ## setting the value of the inverse
  getinverse <- function() i ## getting the value of inverse
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse) ## creating the output so it can be used in next functions
}


## cacheSolve is a function that calculates the inverse of.. 
##..the matrix set in the cache following makeCacheMatrix function

## first it looks for any inverse which has already been contained in the cache. If so it returns that.
## if not it does the calculations on its own and returns the output.

cacheSolve <- function(x, ...) {
  i <- x$getinverse() ## getting the value of inverse of matrix(if any) as specified in previous function
  if (!is.null(i)) { ##if inverse is already there in cache, returining it. 
  return(i)
  }
  
  matrix_data <- x$get() ## getting he data of matrix in order to calculate inverse in next function
  i <- solve(matrix_data, ...) ##using the 'solve' function to calculate the inverse
  x$setinverse(i)
  i ## returning the calculated inverse
}
