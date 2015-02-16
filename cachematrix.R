
## Put comments here that give an overall description of what your functions do

# The matrix that needs to be inversed, first has to go through makeCacheMatrix, in order to make it compatible with the get/set functions. It also creates an empty cache m (NULL). 
# The second function will inverse the matrix, but takes its cache (if available) when called in cacheSolve, instead of always recalculating the inverse. It can call the cache after it has been calculated the first time, and as long as the underlying values have not changed. 

## Write a short comment describing this function

# Please follow the explanations next to each line of code.

makeCacheMatrix <- function(x = matrix()) {                         #1.1 naming function, mandating input argument x being a matrix
m <- NULL                                                           #1.2 setting cached matrix to NULL (empty)
  set <- function(y) {                                              #1.3 creating first sub-function, which allows for changing the underlying matrix x (basically not called by both functions, but to be used with other functions, loops, etc.)
    x <<- y                                                         #1.3.1 this part resets the input of the makeCacheMatrix function x to y, which is the input for the set function
    m <<- NULL                                                      #1.3.2 because the input x changes, the cached inverse matrix is no longer valid and needs to be recalculated, hence m is reset to NULL (the second function will notice this with if(!is.null(m)), and will continue by re-calculating the inverse) 
  }
  get <- function() x                                               #1.4 the second sub-function, which simply prints the (non-inversed) input matrix x
  setmatrix <- function(matrix = matrix()) m <<- matrix             #1.5 the third sub-function, which is used to update the cached matrix by the second function if a new inversed matrix needs to be calculated
  getmatrix <- function() m                                         #1.6 the fourth sub-function, which is used by the second function to check whether or not there already is a cached inverse matrix (if yes, print it and show the message "getting cached inverse matrix", if not, calculate it and assign as the cache inverse matrix)
  list(set = set, get = get,                                        #1.7 this part simply puts all the subfunctions in a list, and names them accordingly
       setmatrix = setmatrix,
       getmatrix = getmatrix)

}

## Write a short comment describing this function

# Please follow the explanations next to each line of code.

cacheSolve <- function(x, ...) {                                    #2.1 returns a matrix that is the inverse of 'x'. Calculates it the first time, takes the cache from then on unless the underlying matrix changes. 
m <- x$getmatrix()                                                  #2.2 assigns m the value of the cached matrix (which can be the inverse matrix if it has been calculated before, or it will be NULL [#1.2])
  if(!is.null(m)) {                                                 #2.3 checks whether or not the value of m is NOT NULL (available), 
    message("getting cached inverse matrix")                        #2.3.1 and if so prints the message "getting chaced inverse matrix", 
    return(m)                                                       #2.3.2 and finally also prints the cached matrix m
  }                     
  data <- x$get()                                                   #2.4 when m is NULL however (not available, not calculated before), it assigns the value of the input matrix x to an object called 'data' (just a placeholder)
  m <- solve(data, ...)                                             #2.5 afterwards it calculates the inverse of x, assigns it to 'm' (in its local context, not using <<-) 
  x$setmatrix(m)                                                    #2.6 it now sets the cached mean to be the inverse of x, using the setmatrix function (see #1.6), which overwrites m in #1.2 using <<-
  m                                                                 #2.7 it prints m to the console
}
