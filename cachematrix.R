### makeCacheMatrix function takes in an inversible square matrix and returns a list of functions
##for getting ht einput matrix, seting the input matrix, getting and setting the inverse matrix 
#cacheSolve takes in the input matrix, gets the inverse if available in cache, computes inverse and returns the inverse matrix
# test function is used to test the two functions in order.

## Write a short comment describing this function
## MakeCacheMatrix takes an input matrix and creates a cache and returns functions for input and reverse matrix setting and retrieval

makeCacheMatrix <- function(x = matrix()) {
  #x: a square matrix
  # return: a list with set the matrix,get the matrix, set the inverse, get the inverse functions
  
  inv = NULL # for get funtion
  set = function(y) {
    # to assign to an environent other than current 
    x <<- y
    inv <<- NULL
  }
  get = function() x # ouputs the input matrix
  setinv = function(inverse) inv <<- inverse # sets inverse in cache
  getinv = function() inv # outputs inverse matrix
  list(set=set, get=get, setinv=setinv, getinv=getinv)

}


## Wreturns the inverse of teh input matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv = x$getinv()
  
  # if the inverse has already been calculated
  if (!is.null(inv)){
    # get it from the cache and skips the computation. 
    message("getting cached data")
    return(inv)
  }
  
  # otherwise, calculates the inverse 
  mat.data = x$get()
  inv = solve(mat.data)
  
  # sets the value of the inverse in the cache via the setinv function.
  x$setinv(inv)
  
  return(inv)
}

## test function can be run with the inut matrix as arguments to compute the inerse using the above two functions
test = function(matr){
  ## @matr: an invertible matrix
  
  temp = makeCacheMatrix(matr)
  
  cacheSolve(temp)

}

