## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This first function, makeCacheMatrix, creates a list of 4 variables to do the following:
 # set the value of the matrix
 # get the value of the matrix
 # set the inverse of the matrix
 # get the inverse of the matrix 
 # This function also allows you to set a matrix and do its inversion. It caches the result (via the "<<-" operator) 
 # in the global environment for use in the upcoming cacheSolve function.  

makeCacheMatrix <- function(x = matrix()) {
   m <- NULL  #matrix inversion results are stored in this variable
   set <- function(y) {
     print("now setting the result to a global environment variable, i.e. via x, which is a free variable")
     x <<- y  
     m <<- NULL #resetting the results
   }
   
   # get the original matrix
   get <- function() x
 
   setinverse <- function(solve) m <<- solve 
   print("set inverse - building cache")
   
   getinverse <- function() m 
   print("get inverse - if cache available")
   
   list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)  
}


## Write a short comment describing this function
## This second function, cacheSolve, calculates the inverse of a computable matrix. However matrix inversion is a costly process.
 # This will first check if the result is already available in the makeCacheMatrix and if so, it directly gets that result.If not 
 # this will then solve the matrix utilizing the solve() function in R. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
   # To check if the matrix inversion results are available in the prior call and if so, return those results
   if(!is.null(m)) {
     message("getting cached data")
     return(m)
   }
   data <- x$get()
   print("getting ready to solve the matrix now")
   m <- solve(data, ...)   
   x$setinverse(m)
   m    
}
