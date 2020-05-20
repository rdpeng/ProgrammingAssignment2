## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) { 
  #first, we set the value for the matrix
        j <- NULL 
        set <- function(y) { 
              
        }
        get <- function() x #Get the value of the Matrix
        setinverse <- function(inverse) j <<- inverse #Then set the value for the inverse matrix
        getinverse <- function() j #Get the value of the inverse matrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  #Get the value of the invertable matrix from the previous function
  i <- x$getinverse()
  if (!is.null(i)) { #if the inverse matrix is not NULL 
          message("Getting cached matrix") #Then return the message "Getting cached matrix"
          return(i)
  }
  #If the value of the ivertible matrix is NULL
  data <- x$get() #Retrives the original matrix data
  i <- solve(data, ...) #Uses the solve function to inverse the matrix
  x$setinverse(i) #Set's the invertible matrix
  i #returns the invertible matrix.
}
        ## Return a matrix that is the inverse of 'x'

###Test###
B <- matrix(c(1,2,3,4),2,2)

B1 <- makeCacheMatrix(B)
cacheSolve(B1) 
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
