## The two functions formulated below let a user cache the inverse of a matrix, and retrive it
## for a later use, instead of recomputing it every time

## The function named 'makeCacheMatrix' takes argument as the matrix whose inverse is to be cached and returns a list of three elements A. the matrix, B. function to set the 
#inverse of the matrix,and C. a function to read the inverse 
# One should create an object of this function type and cache the inverse of the matrix one is working with

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL  # initialize variable 'inv' to store the value of inverse of a matrix
    mat<-matrix()	# initialize variable 'mat' to store value of matrix
      mat<-x
        setinverse<-function(y=matrix())   # constructor function to be used to cache the value of inverse
        {
          inv<<-y
          }
        getinverse<- function()  # constructor function to get the value of inverse later, after it is cached
          {
            inv
              }
            list(matrix=mat,setinv=setinverse,getinv=getinverse) # function returns a list
                }


## The second function named 'cacheSolve' takes an objects of makeCacheMatrix as an argument. 
# It checks for a cached inverse,and if a non-null inverse it cached it returns it with a comments, else
# it computes th4e inverse , caches it and returns the inverse

cacheSolve <- function(x, ...) {
  inverse<-x$getinv()  # Reference value of inverse of matrix and save in variable called 'inverse'
    if(is.null(inverse))			# Test if 'inverse'contains a cached value. If it does not, then reference the value of matrix, calculate its inverse and return it
      {						# Else return the cached value of inverse with a message
        inverse<- solve(x$matrix)
          x$setinv()<-inverse
            return(inverse)
              }
              else
                {
                  message("getting cached inverse of the matrix")
                  return(inverse)
                    }
        ## Return a matrix that is the inverse of 'x'
}
