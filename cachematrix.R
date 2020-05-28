## Put comments here that give an overall description of what your
## functions do


## Write a short comment describing this function
#makeCacheMatrix: This function creates a special “matrix” object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse_matrix<-NULL
  set<- funtion(y){
    x<<-y
    inverse_matrix<<-NULL
  }
  get<-funtion() x
  setinverse <-function(inverse) inverse_matrix<<-inverse
  getinverse <-function() inverse_matrix
  list(set = set,get= get,setinverse = setinverse,
       getinverse=getinverse)

}


## Write a short comment describing this function
#This function computes the inverse of the special “matrix” returned by makeCacheMatrix above.
#If the inverse has already been calculated (and the matrix has not changed), 
#then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse_matrix <-x$getinverse()
  if(!is.null(inverse_matrix)){
    message("getting cached data")
    return(i)
  }
  data <-x$get()
  inverse<-solve(data, ...)
  x$setinverse(inverse_matrix)
  inverse_matrix
}
