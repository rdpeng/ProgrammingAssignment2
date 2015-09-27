# makeCacheMatrix
# Checks if the matrix is square by calling isSquare function that 
# returns TRUE if row and column count is equal otherwise return false
# Creates a "matrix" object that can cache its inverse.
# The object does not calculate the inverse, just saves it inside.
# Saves the matrix to variable x and its inverse to variable s in scope.
# Returned object (actually it's a list) contains methods:
# isSquare: Checks if the matrix is square or not
# setMatrix: sets matrix and resets cached inverse 's'
# getMatrix: returns matrix
# setSolve: saves solve (inverse matrix) value
# getSolve: returns cached inverse value


makeCacheMatrix <- function(x = matrix()) {
        isSquare <- function(x){
          r <- nrow(x)
          c <- ncol(x)
          if(r==c)
          {
              return(TRUE)
          }
          return(FALSE)
        }
        
        if(isSquare(x)){
            s <- NULL
            setMatrix <- function(y) {
                x <<- y
                s <<- NULL
            }
            
            getMatrix <- function(){
                x
            }
            
            setSolve <- function(solve){
                s <<- solve
            }
            getSolve <- function(){
                s
            }
            
            return(list(setMatrix = setMatrix, getMatrix = getMatrix, setSolve = setSolve,  getSolve = getSolve))
        }
        else{
            print("Please use a square Matrix to inverse")
            return(NULL)
        }
}

# Function to get the inversed matrix from an object created by makeCacheMatrix.
# Takes the object of that type as an argument 'x', checks if the object x is null 
# If the object is null, then can not continue the execution. 
# If object is not null then checks if the inverse value is already
# cached, and if it is returns the cached value; if not, then this function gets the matrix
# saved in 'x' and calculates the inverse saves it into 'x' cache using method 'setSolve'
# and returns the result.

## Following is an example of the inputs and error check
# Create matrix => mat <- matrix(rnorm(12), ncol = 4, byrow = TRUE)
# Print matrix
# mat
#           [,1]     [,2]       [,3]        [,4]
# [1,] 2.0809960 2.674179 -0.2314187 -0.05308414
# [2,] 1.6601580 1.259350  1.1878956 -0.36031184
# [3,] 0.3446623 1.115438  0.7633296 -0.26804722
# Call makeCacheMatrix to cache matrix and save the result => o<- makeCacheMatrix(mat)
# Error return and object 'o' should be NULL => "Please use a square Matrix to inverse" 
# Error return if passed 'o' to cacheSolve => cacheSolve(o) 
# Output => "Object passed is null, can not continue execution" 

## Following is an example of the inputs and successful execution
# Create matrix => mat <- matrix(rnorm(16), ncol = 4, byrow = TRUE)
# Print matrix  
# mat
#              [,1]       [,2]        [,3]        [,4]
# [1,]  0.003599667 -0.3691884  1.05594533 -1.23800509
# [2,]  0.586460686  0.3936409  0.67400355  0.59933335
# [3,] -0.411846852  1.1159559 -0.67984411 -0.07055773
# [4,]  0.729187659  0.6457430  0.05107296  1.05447460
# Call makeCacheMatrix to cache matrix and save the result => o<- makeCacheMatrix(mat)
# Pass 'o' to cacheSolve => cacheSolve(o) 
# Output => 
#            [,1]        [,2]       [,3]       [,4]
# [1,]  0.6356798 -0.30772648  0.2708378 0.18189568
# [2,]  0.1529704  0.95132784  0.8451903 0.04097488
# [3,] -0.4460386 -0.04734361 -0.7896488 0.24519206
# [4,]  0.1097719 -0.54826620 -1.3297075 0.11004969

# 
cacheSolve <- function(x, ...) {
        if(is.null(x)){
          print("Object passed is null, can not continue execution")
          return(NULL)
        }
        ## Return a matrix that is the inverse of 'x'
        s <- x$getSolve()
        if(!is.null(s)) {
          message("Getting cached data for inverse of matrix")
          return(s)
        }
        data <- x$getMatrix()
        s <- solve(data, ...)
        x$setSolve(s)
        print(s)
}
