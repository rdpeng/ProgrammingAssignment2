##Caching the Inverse of a Matrix

######Example of using the function##########
#x<-matrix(1:4,2,2) #Original Matrix creation
#xin<-makeCacheMatrix(x) #Creates a special "Matrix" xin returns  list of 4 functions
#xin$get() #Return the original Matrix
#cacheSolve(xin) #Compute and return the inverse of the original matrix
#cacheSolve(xin) #Inverse already computed; show the message "getting cached data" and return the cached inverse matrix

##In this example we use the <<- operator which assign a value to an object in an environment that is different from the current environment. 
##Below are two functions that are used to create a special object that stores a numeric Matrix and cache's its inverse

##The first function, makeCacheMatrix creates a special "Matrix", which is really a list containing a function to
##1) set the value of the matrix
##2) get the value of the matrix
##3) set the value of the inverse of the matrix
##4) get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  myinv <- NULL# myinv object will store the cached inverse of the matrix
  set <- function(y) { #Set the original Matrix
    x <<- y
    myinv <<- NULL
  }
  get <- function() x #Get the original Matrix
  setinvrse <- function(invrse) myinv <<- invrse #Set the inverse Matrix
  getinvrse <- function() myinv #Get the inverse Matrix
  #Return the list containing the functions
  list(set = set, get = get,setinvrse = setinvrse,getinvrse = getinvrse)    
}


##The following function, cacheSolve, calculates the inverse of the special "Matrix" created with the above function. 
##However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation. 
##Otherwise, it calculates the inverse of the Matrix and sets the value of the inverse in the cache via the setinvrse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  myinv <- x$getinvrse()
  ##Checks to see if the inverse has already been calculated, if yes, return it
  if(!is.null(myinv)) {
    message("getting cached data")
    return(myinv)
  }
  ##It calculates the inverse of the Matrix
  data <- x$get()
  myinv <- solve(data, ...)
  ##Cache the inverse matrix
  x$setinvrse(myinv)
  ##Return the computed inverse matrix
  myinv
}
