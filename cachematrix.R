
#makeCacheMatrix transforms a matrix into a list to store additional information 
#input: makeCacheMatrix  takes as input a matrix. 
#output: the output is a list. 
#                       the list includes functions that allows to set and get the initial matrix. 
#                       the list includes functions that allows to to set and get the inverse of a matrix
#remark: this function does not calculate the inverse matrix when it creates the list.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL#declare m otherwise the compiler will not find later on as it is not declared
  
  #define the functions that are encompassed in the list
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x#return the given value
  setinverse <- function(solve) m <<- solve#set the function used
  getinverse <- function() m
  list(set = set, get = get,
       setsolve = setinverse,
       getsolve = getinverse)
  
}


#Cachesolve return the inverse of a matrix
#input:a list created by the function makecachematrix as it needs an inital input matrix, a placholder for a second matrix (inverse matrix) and the function (to compute the inverse)
#process: check if an inverse matrix has already been computed. 
#     if yes, just return the inverse matrix. 
#     If no, compute the inverse matrix and return it
#output:the inverse matrix as an array.
#remark: the function crashes if the matrix is not invertible
cacheSolve <- function(x, ...) {
        
  m <- x$getsolve()                 #load the placeholder for the inverse matrix
  if(!is.null(m)) {                 #check if there is already an inverse Matrix to avoid repeating the same computation
    message("getting cached data")
    return(m)                       #return the inverse matrix previously computed
  }
  data <- x$get()
  m <- solve(data, ...)             #compute the inverse matrix as it is NULL beforehand
  x$setsolve(m)                     #save the inverse matrix in the list. this line is necessary  to avoid re-computing the inverse matrix
  m                                 #return the inverse matrix
}
