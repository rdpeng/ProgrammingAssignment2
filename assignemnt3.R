makeCacheMatrix <- function(m = matrix()) {
  #m: a square matrix
  # return: a list with set the matrix,get the matrix, set the inverse, get the inverse functions
  
  inv = NULL # for get funtion
  set = function(y) {
    # to assign to an environent other than current 
    m <<- y
    inv <<- NULL
  }
  get = function() m # ouputs the input matrix
  setinv = function(inverse) inv <<- inverse # sets inverse in cache
  getinv = function() inv # outputs inverse matrix
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

cacheSolve <- function(x, ...) {
  ## @x: output of makeCacheMatrix()
  ## return: inverse of the original matrix input to makeCacheMatrix()
  
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

test = function(matr){
  ## @matr: an invertible matrix
  
  temp = makeCacheMatrix(matr)
  
  cacheSolve(temp)

}