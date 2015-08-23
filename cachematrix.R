## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(matrix_) {
        inverse_matrix <- NULL
        set_matrix <- function(new_matrix){
                matrix_ <<- new_matrix
                inverse_matrix <<- NULL
        }
        get_matrix <- function() matrix_ 
        set_inverse_matrix <- function(new_inverse_matrix) inverse_matrix <<- new_inverse_matrix        
        get_inverse_matrix <- function() inverse_matrix  
        list(set_matrix=set_matrix,get_matrix=get_matrix,
              set_inverse_matrix=set_inverse_matrix,get_inverse_matrix=get_inverse_matrix)
}



## Write a short comment describing this function

cacheSolve <- function(special_matrix){
        print(special_matrix)
        inverse_matrix <- special_matrix$get_inverse_matrix()
       #return(inverse_matrix)
        if(!is.null(inverse_matrix)) {
                message("getting cached data")
                return(inverse_matrix)
        }
        data <- special_matrix$get_matrix()
        inverse_matrix <- solve(data)
        special_matrix$set_inverse_matrix(inverse_matrix)
        inverse_matrix
}  
