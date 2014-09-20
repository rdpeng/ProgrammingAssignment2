
## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
        # 'inv' is the property of inversion
        
        inv <- NULL
        
        # Here we are setting the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        # Here we are getting the matrix
        get <- function() {
                x
        }
        
        # Here we are setting the inverse of our matrix
        setInverseMatrix <- function() {
                inv <<- inverse
        }
        
        # Here we are getting the inverse of our matrix
        getInverseMatrix <- function() {
                # Here the property of inversion is recalled
                inv
        }
        # with the list function I want to return the list of
        # the computations made before
        list(set = set, get = get,
             setInverseMatrix = setInverseMatrix,
             getInverseMatrix = getInverseMatrix)
        
        
}

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        # Here 'inv' is the matrix inverse of the first one, 'x'
        inv <- x$getInverseMatrix()
        
        # If 'inv' is not equal to NULL the code return the inverse of the matrix
        # is.null returns TRUE if its argument is NULL and FALSE otherwise
        # In other words if we already have the inverse, it returns the inverse.
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        # Recalling the initial matrix
        data <- x$get()
        # calculating the inverse of a matrix using 'solve' function 
        # it is important to remember to use %*% for matrix multiplications
        # only * returns element by element multiplications
        inv <- solve(data, ...) %*% data
        
        # setting the invers of 'inv'
        x$setInverseMatrix(inv)
        
        # return 'inv'
        inv
}
