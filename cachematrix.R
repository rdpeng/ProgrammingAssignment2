## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) { #create a matrix function
  inverse <- NULL #create a variable to store the inverse result
  setmatrix<- function(y) { #set the value of the matrix
    x <<- y
    inverse <<- NULL  #set the variable to null
  }
  getmatrix <- function() x  #get the value of the matrix
  setinverse <- function(solve) inverse <<- solve #set the value of the inverse
  getinverse <- function() inverse #get the value of the inverse
  #the following is a list
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) { #if the inverse is not null
    message("getting invertible data")
    return(inverse) #return the calculated inverse
  }
  data <- x$getmatrix() #or else, get the matrix
  inverse <- solve(data, ...) #calculate the inverse
  x$setmatrix(inverse)
  inverse #return the inverse from the cache
}
