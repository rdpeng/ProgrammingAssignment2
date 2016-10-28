###Assignment 3 for Week 3
####

# Need to calculate two functions, namely makeCacheMatrix and cacheSolve of an inverse matrix 
# First function is to make makeCacheMatrix function, similar to makeVector function given

makeCacheMatrix <- function(x = matrix()) { #creates matrix object
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve #change name
  getInverse <- function() m                    #change name
  list(set = set, get = get,
       setInverse = setInverse, # set to inverse
       getInverse = getInverse) # set to inverse
}

#Second function is to make cacheSolved function of the inverse matrix, similar to cachemean function
  
cacheSolved <- function(x, ...) {
    m <- x$getInverse()    # set to inverse
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...) #use solve(x) function as requested in assigment notes
    x$setInverse(m)
    m
}
  
# Use to test function

####  make a matrix to put into function
testmatrix <- matrix(1,4,4)# 4 x4 matrix with the number 2

CacheMatrix <-makeCacheMatrix(testmatrix)
cacheSolved(CacheMatrix)

#seem to get error    ....Error in solve.default(data, ...) : 
  