## The purpose of this R function is to calculate the inverse of a square matrix and cache it so that it can be used later on from the cache.
## function makeCacheMatrix creates a special "Matrix", and it contains the functions to 
## set the value of the Matrix
## get the value of the Matrix
## set the value of the inverse of the Matrix
## get the value of the inverse of the Matrix

### Test matrices:
## m1 <- matrix(c(1/4, -1/16, -1/2, 3/16), nrow = 2, ncol = 2)
## m1 <- matrix(c(1/2, -1/4, -1, -3/4), nrow = 2, ncol = 2)
# m1 <- matrix(vector, nrow, ncol)
# m1 <- matrix(c(1,3,2,.4,.2,4.3,.5,.6,8), nrow = 3, ncol = 3)
# m1 <- matrix(c(1,3,2,.4,.2,4.3,.5,.6,8,20,21,34,12,.1,0,0), nrow = 4, ncol = 4)
# myMatrix_object <- makeCacheMatrix(m1)
# myMatrix_object$get()
# cacheSolve(myMatrix_object)
# myMatrix_object$get_inv()
# myMatrix_object$set(c(1,3,2,.4,.2,4.3,.5,.6,8,20,21,34,12,.1,0,0,6,4,2,3,4,1,0,12,6), nrow = 5, ncol = 5)
## After set command, inv is cleared and cacheSolve need to be run for calculating new inv.

makeCacheMatrix <- function(x = matrix()) {
  m_inv <- NULL
  set <- function(vector,nrow,ncol) { ## assing a new Matrix.
    m1 <<- matrix(vector,nrow,ncol)
    m_inv <<- NULL ## clear cache.
  }
  get <- function() m1 # Prints the matrix. 
  set_inv <- function(matrix_i) m_inv <<- matrix_i ## this function is called by cacheSolve to insert the inverse of the matrix to cache.
  get_inv <- function() m_inv ## to call/retrieve the inverse from cache by other functions.
  list(set = set, 
       get = get, 
       get_inv = get_inv,
       set_inv = set_inv)
}


## The cacheSolve function calculates the inverse of the special "Matrix" created with the makeCacheMatrix function. However, 
##   it first checks to see if the mean has already been calculated. 
##   If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates 
##   the inverse of the matrix and sets the inverse matrix in the cache via the set_inv function. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m_inv <- x$get_inv()
  if(!is.null(m_inv)) { ## if Cache already has inverse, return the cache.
    message("getting cached data") 
    return(m_inv)
  } else { ## else, Calculate Inverse
    data <- x$get() ## Call the matrix from cache using makeCacheMatrix function
    m_inv <- solve(data, ...) 
    x$set_inv(m_inv) ## Call set_inv to save new inverse in to cache.
    m_inv
    
  }
}

        
