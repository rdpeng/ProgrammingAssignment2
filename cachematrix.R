## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

  makeCacheMatrix <- function(x = matrix()) {  ## define the argument with default mode of "matrix"
    j <- NULL    ## initialize j as NULL; it will hold value of the matrix inverse 
    set <- function(y){  ##define the set function
      x <<- y           ## value of matrix in parent environment
      j <<- NULL         ## if there is a new matrix, reset j to NULL
    }
    get <- function()x    ##define get function 
    setInverse <- function(inverse) j <<- inverse ##assigns value of inverse to parent environment 
    getInverse <- function() j   ## gets the value of j 
    list(set = set, get = get, 
         setInverse = setInverse, 
         getInverse = getInverse)  ## you need this in order to refer to the functions with the $ operator
  }
  
  ## This function computes the inverse of the special "matrix" returned by the above makeCacheMatrix above
  ## if the inverse has already been calculated the cacheSolve will retrieve the inverse from the cache as long as the matrix is the same 
  cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    j <- x$getInverse()
    if(!is.null(j)){
      message("getting cached data")
      return(j)
    }
    mat <- x$get()
    j <- solve(mat,...)
    x$setInverse(j)
    j
  }



}



