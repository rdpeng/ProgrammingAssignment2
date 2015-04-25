## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inverse_of_matrix <- NULL
  set_inverse <- function(mat) {
    x <<- mat
    inverse_of_matrix<<- NULL
  }
  get_mat <- function() x
  set_inverse_mat <- function(inverse_of_matrix) inverse_of_matrix <<- inverse_of_matrix
  get_inverse_mat <- function() inverse_of_matrix
  list(set= set_inverse, get= get_mat,
       setinverse = set_inverse_mat,
       getinverse = get_inverse_mat)

}


## Write a short comment describing this function
## cacheSolve function will check for the current matrix inverse in the environment, if the inverse is previously calculated then it will return the inverse from cached data otherwise calculate it
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse_of_matrix<- x$getinverse()
  if(!is.null(inverse_of_matrix)) {
    message("Getting the inverse of matrix from the cached data")
    return(inverse_of_matrix)
  }
  data_mat <- x$get()
  inverse_of_matrix <- solve(data_mat, ...)
  x$setinverse(inverse_of_matrix)
  inverse_of_matrix

}

##Giving example of running the code with sample matrix
##Creating a sample matrix
# > x<-matrix(1:4,c(2,2))
##making cache matrix
# > makeCache_matrix<-makeCacheMatrix(x)
# > makeCache_matrix$get
# function() x
# <environment: 0x000000000a5c6238>
#   > makeCache_matrix$get()
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4
##calculating the inverse of the matrix for the first time
# > cacheSolve(makeCache_matrix)
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
##Again calculating the inverse of the same matrix
# > cacheSolve(makeCache_matrix)
# getting the inverse of matrix from the cached data
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > 
