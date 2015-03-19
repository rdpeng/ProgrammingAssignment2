## Assignment 2 of R
## makeCacheMatrix function creates a matrix 
## object that can cache its inverse.

## Following functions are called to perform describedd functions:

## 1. set_valve_matrix: set the value of the matrix
## 2. get_value_matrix: get the value of the matrix
## 3. set_inverse_matrix: set the inverse of the matrix
## 4. get_inverse_matrix: get the inverse of the matrix

makeCacheMatrix <- function(matrix_value = matrix()) {
        inverse_matrix<-NULL
        
        set_matrix_value<-function(y){
                matrix_value <<- y
                inverse_matrix <- NULL
        }
        
        get_matrix<-function(){
                matrix_value
        }
        
        set_inverse_matrix<-function(z){
                inverse_matrix<<-z
        }
        
        get_inverse_matrix<-function(){
                inverse_matrix       
        }
        
        
        list(set_matrix_value = set_matrix_value, get_matrix = get_matrix, 
             set_inverse_matrix = set_inverse_matrix, 
             get_inverse_matrix= get_inverse_matrix)
}


## This function computes the inverse of the matrix returned by 
## makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then cacheSolve should retrieve
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$get_inverse_matrix()
        if(!is.null(inv)) {
                message("getting cached data.")
                return(inv)
        }
        data <- x$get_matrix()
        inv <- solve(data)
        x$set_inverse_matrix(inv)
        inv
}

## Test
## m <- rbind(c(1, 2), c(3, 4))
## matrixEG <- makeCacheMatrix(m)
## matrixEG$get_matrix()

## cacheSolve(matrixEG)