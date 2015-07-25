## This program caches the inverse of a matrix
## by using the function solve() to calculate 
## the inverse of a given matrix and uses R's 
## scoping rules to cache its results. This is 
## useful because solve() takes times to calculate
## inverse and hence the result with be cached
## for a second time calculation of the same matrix.

## Demo Program
## > newmatrix <- matrix(c(4, 3, 3, 2), nrow=2, ncol=2)
## > matrixcache <- makeCacheMatrix(newmatrix)
## > cacheSolve(matrixcache)
## Output: Inverse calculated from solve() function
## > cacheSolve(matrixcache)
## Output: Inverse stored in cache

## Creates a list of important functions represented
## as a matrix.

makeCacheMatrix <- function(x = matrix()) 
    {
      m <- NULL
  
      ## Sets the value of the matrix
  
      set <- function(y)
          {
              x <<- y
              m <<- NULL
          }
  
      ## Gets the value of the matrix
    
      get <- function() x
  
      ## Sets the value of the inverse of the matrix
  
      setsolve <- function(solve) m <<- solve
  
      ## Gets the value of the inverse of the matrix
  
      getsolve <- function() m
  
      ## Returns the list of functions
  
      list(set = set, get = get, 
           setsolve = setsolve, 
           getsolve = getsolve)
    }


## Once the makeCacheMatrix function is run on a matrix
## This function returns the inverse of the matrix
## freshly calculated the first time and from the cache
## the next time.

cacheSolve <- function(x, ...) 
    {
         ## Return a matrix that is the inverse of 'x'
         m <- x$getsolve()
         if(!is.null(m))
         ## Checks if the inverse is cached
              {
                  message("getting cached data")
                  return(m)
              }
  
         ## if the cache isn't filled
         data <- x$get()         #Get it
         m <- solve(data, ...)   #Solve it
         x$setsolve(m)           #Set it
         m                       #Return it
    }
