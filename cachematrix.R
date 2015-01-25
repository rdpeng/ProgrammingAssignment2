## The following two functions are used to create a special 
## matrix and cache its inverse (assuming the input matrix is nonsingular).

## makeCacheMatrix is a function that intakes a nonsingular matrix and outputs
##                 a list of four functions for such matrix:
##                 1.  set(): stores the matrix
##                 2.  get(): returns the matrix
##                 3.  setinverse(): stores (i.e. caches) the inverse matrix
##                 4.  getinverse(): returns the stored inverse matrix

makeCacheMatrix <- function(x = matrix()) {

  I <- NULL
  set <- function(y) {      # 1. Store the matrix
    x <<- y
    I <<- NULL
  }
  get <- function() x       # 2. Return the matrix
  
  setinverse <- function(inverse) I <<- inverse     # 3. Store the inverse matrix
  
  getinverse <- function() I     # 4. Return the cached inverse matrix
  
  list(set = set, get = get,     # Return list of functions 1. - 4.
       setinverse= setinverse,
       getinverse = getinverse)

}


## cacheSolve is a function intaking the special, four functions of x output by makeCacheMatrix and,  
##            returns the inverse of the matrix x.  If the inverse has already been calculated, it 
##            returns the cached inverse.  Otherwise it computes the inverse via the functions solve()

cacheSolve <- function(x,...) {
        
  inverse <- x$getinverse()  # Return a matrix that is the inverse of 'x'
  
  if(!is.null(inverse)) {    # If the stored inverse matrix is not NULL, return the cached inverse 
    message("getting cached data")
    return(inverse)
  }
  # Otherwise, compute the inverse, cache it using the setinverse() function, 
  # and return the computed inverse.
  data <- x$get()
  
  inv <- solve(data)
  
  x$setinverse(inv)
  inv
  
}

##  EXAMPLE USAGE:

# cachematrix <- makeCacheMatrix(diag(3^(1:7)))  # Creating special matrix object for the diagonal 
#                                                #    matrix having entries a_ii = 3^i

# cacheSolve(cachematrix)     # The first time, cacheSolve computes the inverse matrix and returns 
#                             #   the computed matrix

# cacheSolve(cachematrix)     # The second time, cacheSolve returns the cached inverse matrix it 
#                             #   it computed and stored the first time
