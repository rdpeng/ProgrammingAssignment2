# Assumption - The matrix you supply as Input is always inversible 
##  This function creates a special "matrix" object that can cache its inverse.
##Illustrates lexical scoping
## This has two functions MakeCacheMatrix and Cachesolve


#This function makes a matrix object given a input Matrix that can be inversed
makeCacheMatrix <- function(x = matrix()) {
# Initialise matrix i for inverse to NULL
  i <- matrix()
  
  #Set the input argument matrix y into Matrix x 
  set <- function(y) {
    
    #The below line is special as it assigns matrix input y into variable x defined in the parent environment 
    x <<- y
    
    #Initialise matrix inverse variable i to NULL of the parent
    i <<- matrix()
  }
  
  #Function get returns the value of  matrix defined in the parent 
  get <- function() x
  
  #set inverse function returns inverse of input matrix input to parent environment variable matrix i
  #This uses solve function to return inverse of a matrix
  
  setinverse <- function(input) i <<- input
  
  #Get value of matrix variable i from parent 
  getinverse <- function() i
  
  #makes each function as an element of list to return back to parent environment
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## INput- A matrix x and the output is an inverse of Matrix x
# It returns inverse of input matrix x if present in the cache else computes inverse and returns to console 

cacheSolve <- function(x, ...) {
## Check to see if you can return the cached value using the matrix object getinverse function
  
  i <- x$getinverse()
  #If  matrix i is not  NULL ( it does not have NA value),  then return the cached data printing the same to console
  
  if(!all(is.na(i))) {
    message("getting cached data")
    return(i)
  }
  
  #if cache is null, then calculate the inverse here using solve, assign the inverse to the matrix object and return to console
  data <- x$get()
    i <- solve(data, ...)
  x$setinverse(i)
  i
  
  
}

#Output trials give the following in console to check the output
#m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
#n1 <- matrix(c(6,2,8,4), nrow = 2, ncol = 2)
#n2 <- matrix(c(5/8, -1/8, -7/8, 3/8), nrow = 2, ncol = 2)
#myMatrix_object <- makeCacheMatrix(m1)
#cacheSolve(myMatrix_object) --> should compute the first time
#cacheSolve(myMatrix_object) --> should return cached value
#change value of myMatrixObject using below 
#myMatrix_object <- makeCacheMatrix(n2)
#cacheSolve(myMatrix_object) --> should compute the first time
#cacheSolve(myMatrix_object) --> should return cached value
#Below shows snapshot of output you should get
#-----------------------------------------------
#> myMatrix_object <- makeCacheMatrix(m1)
#> cacheSolve(myMatrix_object)
#[,1] [,2]
#[1,]    6    8
#[2,]    2    4
#> cacheSolve(myMatrix_object)
#getting cached data
#[,1] [,2]
#[1,]    6    8
#[2,]    2    4
#> myMatrix_object <- makeCacheMatrix(n2)
#> cacheSolve(myMatrix_object)
#[,1] [,2]
#[1,]    3    7
#[2,]    1    5
#> cacheSolve(myMatrix_object)
#getting cached data
#[,1] [,2]
#[1,]    3    7
#[2,]    1    5
#> 
#--------------------------------------------------