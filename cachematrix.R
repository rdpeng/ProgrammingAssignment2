## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# the makeCacheMatrix stores and retreives a matrix and its inverse
## 1.  set the value of a matrix and delete any pre-existing inverse matrix
## 2.  get the value of a matrix
## 3.  set the inverse of the matrix
## 4.  get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        -
                inverse<-NULL 
        
        set <- function(y) {                   # creates the matrix
                x <<- y
                inverse <<- NULL               # when setting a new matrix, delete existing inverse matrix
        }
        
        get<-function() x                       #retrieves the matrix
        
        setinverse<-function(calculatedinverse) { #set the inverse
                inverse<<-calculatedinverse
        }
        getinverse<- function() inverse         #retrieves the inverse
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



## Write a short comment describing this function



## The cacheSolve Function assumes that the matrix 'x' is inversible
## 1. If matrix x is stored and unchanged - 
##    check if existing inverse matrix is stored in makeChacheMatrix
## 2. If no inverse matrix is cached in makeChacheMatrix - 
##    the compute the inverse and set it in makeChacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse<-x$getinverse() #retrieves inverse value from makeCacheMatrix
        
        if(!is.null(inverse)) { #if inverse has a value, return the inverse
                message("getting cached data")
                return(inverse)
        }
        data <- x$get() 
        #else, the function retrieves the matrix from makeCacheMatrix, 
        #and compute the inverse
        inverse <- solve(data) #calculates the inverse
        
        x$setinverse(inverse)
        
        return(inverse)
        
}
