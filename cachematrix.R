## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix
## function
## 1) set : set org matrix to variable x
## 2) get : get org matrix from variable x
## 3) setinvm : set inverse matrix from variable invm(invm was calculated by cacheSolve)
## 4) getinvm : get inverse matrix from variable invm(invm was calculated by cacheSolve)

makeCacheMatrix <- function(x = matrix()) {
  invm <- NULL
  set <- function(orgm) x <<- orgm
  get <- function() x
  setinvm <- function(invx) invm <<- invx
  getinvm <- function() invm
  
  list(set=set,
       get=get,
       setinvm=setinvm,
       getinvm=getinvm)
}


## Write a short comment describing this function
## cachesolve
## if matrix x is null, make inverse matrix and save it to cache in object x
## if not, get calculated inverse matrix from object x
cacheSolve <- function(x, ...) {
  invm <- x$getinvm()
  if(!is.null(invm)){
    message("get inverse matrix from cached data :)")
    return(invm)
  }
  
  mat <- x$get()
  invm <- solve(mat)
  x$setinvm(invm)
  ## Return a matrix that is the inverse of 'x'
  invm
}
