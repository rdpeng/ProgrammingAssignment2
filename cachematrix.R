# Function saves a matrix and store it twice, as matrix and invers of himself.
# We reach the maingoal with a check after input of an new matrix, 
#if old and new are similar and we don´t need to computed it again.


#Kind of Frontend: We name our function and assign working commands
#`gatevar` is the variable to communicate with our second function, our `backend` 


makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
        set <- function(y) 
        {
                x <<- y
                inv <<- NULL
        }
        
        get <- function() 
        {
                x
        }
        
        setInverse <- function(gatevar)
        {
                inv <<- gatevar
        }
        
        getInverse <- function() 
        {
        inv
        }
        
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}



# Our ´backend´. We check, if there are already a similar matrix.
# If it´s not, we creates the invers of our given matrix

cacheSolve <- function(x, ...) {

        inv <- x$getInverse()
        
        if (!is.null(inv)) 
        {
                message("getting cached data")
                return(inv)
        }
        
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
