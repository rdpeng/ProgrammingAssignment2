
#########################################################

## Week 3 Programming Assignment
## Caching the Inverse of a Matrix

## The purpose of this program is to improve the efficiency
## of taking the inverse of a matrix by first checking if the
## inverse already exists in memory

##########################################################


## This function creates a list containing four separate
## functions that will be used to store and return the 
## inverted matrix. 

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL #Setting inverse of the matrix to NULL
  
  ## The set function sets the input matrix to
  ## the variable x and sets the inverse matrix i equal to 
  ## missing. 
  set <- function(y) {
      x <<- y
      i <<- NULL
  }
  
  # The get function returns the input matrix
  get <- function () x
  
  ## The setinverse function sets the input inverse function
  ## to the variable i. 
  setinverse <- function(inverse) i <<- inverse
  
  ## The getinverse function returns the inverse matrix i
  getinverse <- function () i
  
  ## Creating a list of the four functions. 
  list(set = set, 
       get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function checks if the inverted matrix already exists.
## If it does exist then the program returns the invert. 
## If it does not exist then the inverted matrix is calculated.

cacheSolve <- function(x, ...) {
  
    ## Getting the inverse matrix using the getinverse function
    ## from within the makeCacheMatrix function. 
    i <- x$getinverse()
    
    ## If the inverse matrix returned is not empty then 
    ## return message and return the inverted matrix rather
    ## then recreating it. 
    if(!is.null(i)){
        message("getting cached data")
        return(i)
    }
    
    ## Otherwise, put the input matrix in the variable data
    data <- x$get()
    
    ## Find the inverse of the matrix and store in variable i
    i <- solve(data)
  
    ## Assign the inverted matrix to the variable in memory
    x$setinverse(i)
    
    ## Return the inverted matrix
    i
}


## Example running the code. 

## Creating a matrix.
input_matrix <- matrix(1:4, 2, 2)

## Running the first program on the created matrix.  Nothing is returned.
a <- makeCacheMatrix(input_matrix)

## Running second program with the list of the four functions created from 
## the first function as the input. 
cacheSolve(a)

## The first time the program is run, it returns the inverted matrix:
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

## Now run the program again. 
cacheSolve(a)

## Now it returns the message and the cached inverse of the matrix.
## getting cached data
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5












