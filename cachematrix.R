## makeCacheMatrix creates a matrix based on user input, solves for the inverse of the matrix,
## and caches the inverse matrix into the new matrix, cacheMatrix.  No data is returned.

makeCacheMatrix <- function(x = matrix()) {
  cacheMatrix<<-solve(x)  ## Solves inverse of matrix and caches values to new matrix
}

##cacheSolve performs the same task as makeCacheMatrix then checks to determine if the
##resulting inverse matrix is the same as the previously cached matrix.  If so, it prints the
##new result.  If not, it prints the cached result.  

cacheSolve <- function(x, ...) {  ## Return a matrix that is the inverse of 'x'
  xsolved<-solve(x)  ##solves for inverse of x
  if(xsolved[1,1]==cacheMatrix[1,1]) { ##checks that cacheSolve result is the same as makeCacheMatrix result
    print(xsolved)    ##if so, prints cacheSolve result
  } else { 
    print(cacheMatrix) ##if not, prints cacheMatrix result 
  }
}

