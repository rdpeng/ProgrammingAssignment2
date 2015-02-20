## Assignment 2 should create 2 functions that are used to create a special object that stores a matrix vector 
##and cache's its INVERSE.


## The below function takes in a square matrix as an input.calculates the inverse of the  matrix using the solve function
## and assign the the inverse to the object m. The function creates a speacial vector, which is a list of functions to
## Sets/get the matrix And set and get the inverse of the matrix


makeCacheMatrix <- function(x = matrix())
  {
      m <- NULL
      set <- function(y) 
        {
          x <<- y
          m <<- NULL
        }
      get <- function() x
      ## calculating the inverse and assiging to m
      setinv <- function(solve) m <<- solve
      ## get the result, which is the inverse
      getinv <- function() m
      list(set = set, get = get,setinv = setinv, getinv = getinv)
  
  }


## The second part of tis assignment is a function to see if the inverse for the given matrix has already been calculated
## if it is then it gets the value from cache using getinv
## if not then calculates inverse and sets the value in cache using the setinv function.

cacheSolve <- function(x, ...) 
  {
      ## Return a matrix that is the inverse of 'x'
      m <- x$getinv()
      ## checking to see if the inverse is already in cache and retrieve it
      if(!is.null(m))
        {
          message("getting cached data")
          return(m)
        }
      data <- x$get()
      ## if no inverse found, then recompute the inverse and set in the cache
      m <- solve(data, ...)
      x$setinv(m)
      m
  }
