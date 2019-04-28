## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix(c(1:9), 3,3)) {
  mtx <<- x
  invx <<- matrix(,3,3)
  detm <- det(x)
  if(detm <= 0) {
    message("NC")
    
  }
  else {
    invx <<- solve(x)
  }
  
}


## Write a short comment describing this function

cacheSolve <- function(x=mtx, inv=invx) {
        ## Return a matrix that is the inverse of 'x'
  invxx <<- solve(inv)
  
  if (all.equal(x, invxx)) { message("matched")}
  else { message("different")} 
}
