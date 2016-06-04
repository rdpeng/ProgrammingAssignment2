#REQUIRMENTS
#Must be a square matrix
#Each row cannot be multipules of eachother

#This Function
#sets a matrix
#gets a matrix
#sets the inverse
#gets the inverse

makeCacheMatrix <- function(x = matrix()) {
  
  m = NULL
  set = function(y) {
    x <<- y
    m <<- NULL
  }
  get = function() x
  set_matrix = function(solve) m <<- mean
  get_matrix = function() m
  list(set = set, get = get,
       set_matrix = set_matrix,
       get_matrix = get_matrix)
  
  
}


#This function
#First checks to see if the matrix was already solved for the inverse
#Then solves the invers
#outputs the inverse

cacheSolve <- function(x, ...) {
  
  m = x$get_matrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  matrix = x$get()
  m = solve(matrix, ...)
  x$set_matrix(m)
  m
  
}



#Example_1
v = 9:12
m= matrix(v,2,2)
m
cacheSolve(makeCacheMatrix(m))
#Example_2

v = cbind (c(9, 10), c(11, 12))
m = makeCacheMatrix(v)          
m$get()
cacheSolve(m)
#

