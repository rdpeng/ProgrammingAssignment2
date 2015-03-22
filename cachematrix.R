## Put comments here that give an overall description of what your
## functions do
##
## makeCacheMatrix() is a function that creates a data object 
##    that chaches the calculated inverse of an matrix
##    the data object has 4 methods: set, get, setinverse, getinverse
##    that allow accessing and setting the values of the data object
##
## cacheSolve() is a function that calculates the inverse of a matrix
##    unless the inverse of the matrix already exists
##    it uses the functions provided by makeCacheatrix to check  
##    (1) if the inverse already exists in order to avoid re-computation of the inverse
##    (2) to chache the freshly calculated inverse for later re-use 


makeCacheMatrix <- function(computed_inverse = matrix()) {
## creates a data object that chaches the calculated inverse of an matrix
## creates 4 data object 4 methods: set, get, setinverse, getinverse
##    that allow accessing and setting the values of the data object     
     
     
     ## initialize the Chache
     stored_inverse_matrix <- NULL

     ## Define the function "set" for the makeCacheMatrix parent environment
     set <- function(y) {
          ## Set computed_inverse for the function enviromnent to 'y'
          computed_inverse <<- y
          ## Set stored_inverse_matrix for the makeCacheMatrix environment to NULL
          stored_inverse_matrix <<- NULL
     }
     
     ## Define the function "get" for the makeCacheMatrix parent environment
     get <- function() {
          return (computed_inverse)
     }
     
     ## Define the function "getinverse" for the makeCacheMatrix parent environment
     setinverse <- function(computed_inverse) {
          ## take the value of computed_inverse and assigned it to the chache stored_inverse_matrix 
          stored_inverse_matrix <<- computed_inverse
     }
     
     
     ## Define the function "setinverse" for the makeCacheMatrix parent environment
     getinverse <- function() {
          return(stored_inverse_matrix)
     }
     
     ## "objectvariable" are variables whose value is a list of functions.
     ##  We access the functions stored in the list by using the names of the list objects
     ##  not the names of the functions. 
     ##  this ist is a means of making the methods explicite to the global environment.
     
     list(set = set, 
          get = get,
          setinverse = setinverse,
          getinverse = getinverse)
     
}

cacheSolve <- function(data_matrix, ...) {
## cacheSolve() is a function that calculates the inverse of a matrix
## if the inverse of the matrix already exists it takes the value from the cache
## it uses the functions provided by makeCacheatrix   
## (1) to to check if the inverse already exists in order to avoid re-computation of the inverse
## (2) to chache the freshly calculated inverse for later re-use 
     
     ## sets the local inverse equal to the inverse stored by makeCacheMatrix
     local_inverse <- data_matrix$getinverse()
     
     if(!is.null(local_inverse)) {
          ## if inverse already exist (eqivalent to NOT null)
          ##   print message
          message("getting cached data")
          ##   return stored inverse
          return(local_inverse)
          ##   exist function
     }
     ## if inverse does NOT exist
     ##    get the data from data_matrix object and assign to variable: data
     data <- data_matrix$get()     
     
     ## calculate the inverse of data_matrix
     local_inverse <- solve(data, ...)
     
     ## assigned the newly calculated inverse to the chached inverse
     data_matrix$setinverse(local_inverse)
     
     ## returns the calculated inverse
     return(local_inverse)
}
