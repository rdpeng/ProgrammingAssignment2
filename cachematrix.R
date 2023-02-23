## function cacheSolve calculates the inverse of a matrix if it was not already stored
## prequisites:
## input is of type "makeCacheMatrix"

## makeCacheMatrix
## goal: store inverse of matrix that has been calculated
## input: non-inverted matrix & inverted matrix
## output: functions + cached data in other environment than the calling environment

makeCacheMatrix <- function(x = matrix()) {
      invmatrix <<- NULL
      set <- function(y) { ## set the non-inverted matrix into cache
            x <<- y
            invmatrix <<- NULL
      }
      get <- function() { ## gets the non-inverted matrix if cached, returns NA if not cached
            x
      }
      setinvmatrix <- function(z) { ## stores inverse matrix calculated in calling function
            invmatrix <<- z
      }
      getinvmatrix <- function() { ## returns the inverse matrix if cached, returns NULL if not cached
            invmatrix
      }
      
      list(set = set, get = get, 
           setinvmatrix = setinvmatrix, getinvmatrix = getinvmatrix)
}


## cacheSolve
## goal: return inverse matrix to requestor
## input: matrix
## output: inverse of matrix
## logic:
      ## IF inverse matrix exists in cache then get inverse matrix
      ## ELSE calculate inverse matrix, store inverse matrix with function from makeCacheMatrix
      ## RETURN inverse matrix

cacheSolve <- function(x, ...) {
      print("start")
      invmatrix <- x$getinvmatrix()
      if(!is.null(invmatrix)){
            message("retrieve inverse matrix")
      }
      else { ## calculate the inverse matrix
            message("calculate inverse matrix with solve() and cache result")
            data1 <- x$get()
            invmatrix <- solve(data1)
            x$setinvmatrix(invmatrix)

      }
      ## Return a matrix that is the inverse of 'x'
      message("return inverse matrix")
      invmatrix
}

