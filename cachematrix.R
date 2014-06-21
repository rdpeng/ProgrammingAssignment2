## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##fuctions to cache square matrix using solve
#makeCacheMatrix  creates special matrix object that can cache its inverse
#this function defines get,set, getinverse and setinverse functions 

makeCacheMatrix <- function(x = matrix()) {
invx <- NULL
      set <- function(y) 
	{
             x <<- y
             invx <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) invx <<- solve
      getinverse <- function() invx
      list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)
}




## Write a short comment describing this function
#this function takes matrix as parameter uses get,set,getinverse and setinverse functions 
#it also checks if the matrix inverse is available 
#if yes then existing inverse is returned if no then inverse is computed


cacheSolve <- function(x, ...) {
     invx <- x["getinverse()"]
      if(!is.null(invx)) 
	{
             message("getting cached inverse Matrix")
             return(invx)
      }
      mat <- x$get()
      invs <- solve(mat, ...)
      x$setinverse(invx)
      invx


        ## Return a matrix that is the inverse of 'x'
}
