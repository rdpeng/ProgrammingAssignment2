## Put comments here that give an overall description of what your
## functions do

## following the given example and hint of the makeVector function, I replicate its logic
## to construct the matrix object.

## Write a short comment describing this function

#to save computer power I assign the matrix x, values from other existing environment.

makeCacheMatrix <- function(x = matrix()) 
{
        matrix_inverse <- NULL  
        set <- function(y) 
        {
                x <<- y
                matrix_inverse <<- NULL
        }
 take <- function() x   
    setinv <- function(inverse) matrix_inverse <<- inverse
    takeinv <- function() matrix_inverse
    list(set = set, take = take, setinv = setinv, takeinv = takeinv)
}


## Write a short comment describing this function

## as specified in the example cachemean

cacheSolve <- function(x, ...) 
{
     matrix_inverse <- x$takeinv()   #bring the inverse from the previous cached matrix
    if (!is.null(matrix_inverse))   #if exist, display it
    {       
        message("getting cached data")
        return(matrix_inverse)
    }
    mat <- x$take()
    matrix_inverse <- solve(mat, ...) # else calculate the inverse with the solve function
    x$setinv(matrix_inverse)
    matrix_inverse
}

