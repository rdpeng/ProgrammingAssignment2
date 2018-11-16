## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#take the matrix as an input 
makeCacheMatrix <- function(x = matrix()) {
        inverse_matrix <- NULL
        
#set the values of the matrix
        set_matrix <- function(y) {
                x <<- y
                inverse_matrix <<- NULL
        }
        
        get_matrix <- function() x                                    #get the value of the Matrix
        set_inverse <- function(inverse) inverse_matrix <<- inverse   #set the value of the invertible matrix
        get_inverse <- function() inverse_matrix                      #get the value of the invertible matrix
      
        list(set_matrix = set_matrix, get_matrix = get_matrix,
             set_inverse = set_inverse, get_inverse = get_inverse)
        
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        inverse_matrix <- x$get_inverse()
        if(!is.null(inverse_matrix)) {                       #if inverse matrix is not NULL
                message("Getting Cached Invertible Matrix")  #Type message: Getting Cached Invertible Matrix 
                return(inverse_matrix)                       #return the invertible matrix
        }
        
        #if value of the invertible matrix is NULL then  
        MatrixData <- x$get_matrix()                     #get the original Matrix Data 
        inverse_matrix <- solve(MatrixData, ...)             #use solve function to inverse the matrix
        x$set_inverse(inverse_matrix)                         #set the invertible matrix 
        return(inverse_matrix)                               #return the invertible matrix
        ## Return a matrix that is the inverse of 'x'
}
