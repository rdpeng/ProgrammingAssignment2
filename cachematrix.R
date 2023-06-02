## Put comments here that give an overall description of what your
## functions do

# comments are listed inside each function

makeCacheMatrix <- function(x = matrix()) {
  res <- NULL
  # to Set the matrix 
set <- function(y){
  x <<- y
  res <<- NULL
}
  # to ghet the value of the matrix 
get <- function() x
  # to set the inverse of the matrix
setsolve <- function(solve) res<- solve
  # to get the inverted value of the matrix
getsolve <- function() res
  # to return the list of the function
list(set = set, get = get,
     setsolve = setsolve,
     getsolve = getsolve)
}

}


## comments are inside the function
# function to solve the matrix or retun the cache
cacheSolve <- function(x, ...) {
        res <- x$getsolve()
  # if the cache contains the solved value, retun from the cache
  if(!is.na(res)){
    print("return Data from Cache")
    return(res)
  }else{
    # calculate the inverted matrix and Return a matrix that is the inverse of 'x'
    data <- x$get()
  res <- solve(data,...)
  x$setsolve()
  return(res)
  }     
}
