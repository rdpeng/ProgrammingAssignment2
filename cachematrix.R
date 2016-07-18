## These two functions allow user to create a set of functions 
##for a matrix to ultimately allow caching its inverse

## This function will create a list of functions that can be 
##executed against an input matrix to set and return the matrix
##and also set and return the inverse of that matrix

#I am adding a change!!

makeCacheMatrix <- function(x = matrix()) {             
        inv <- NULL                                     ##inv is where the cached value of the inv will be stored, initialized to NULL
        set <- function(y) {                            ##set function allows user to set the value of the matrix they are creating
                x <<- y
                inv <<- NULL
        }
        get <- function() x                             ##get function returns the matrix that was created
        setinverse <- function(solve) inv <<- solve     ##setinverse function allows user to set the value of the matrix's inverse 
                                                        ##by calling the solve function -- the inverse is now cached in the inv variable
        getinverse <- function() inv                    ##getinverse returns the value of inv (the cached inverse)
        
        list(set = set,                                 ##the final result of the makeCacheMatrix is a list of all the above functions
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function will first attempt to pull the cached inverse of the matrix and return it
## If none is found, then it will calculate the inverse of the matrix and return it

cacheSolve <- function(mat, ...) {
        inv <- mat$getinverse()                         ##this line will attempt to pull the cached value of the inverse of the matrix
        if(!is.null(inv)) {                             ##if that result is NOT null, then that value will be returned
                message("getting cached data")
                return(inv)
        }
        else {                                          
        data <- mat$get()                               ##if the result IS null, then this line will get the matrix
        inv <- solve(data, ...)                         ##this line will calculate the inverse of the matrix
        mat$setinverse(inv)                             ##and this line will set cache the inverse of the matrix for future us
        inv                                             ##and finally this line will return the calculated inverse
        }
        
}
