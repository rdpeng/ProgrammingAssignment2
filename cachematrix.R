## These function creates a function that save the value of a matrix in cache 
## then get the inverse of the matrix and save again in the cache

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {  #creates the funtion makeCacheMatrix
        m <- NULL   #clear the value of m
        set <- function(y) { #creates the set function arguments y
                x <<- y  # gives x the value of y and reset m
                m <<- NULL
        }
        get <- function() x   # retrive the vector x
        setMi <- function(solve) m <<- solve  #sets the inverse of matirx setMi to m in the solve function
        getMi <- function() m  #return the inverse of the matrix m
        list(set = set, get = get,
             setMi = setMi,
             getMi = getMi)   #retur a vector 
}


## CacheSolve returns the inverse of a matrix, but first checks if already have been calculated the inverse,   
## then retrieve the inverse from the cache. if not had been calculated then it calculates the inverse and save in the cache. 

cacheSolve <- function(x, ...) { 
        m <- x$getMi()   # get the inversed matrix from object x
# it will be null if uncalculated, remember the first line "m <- NULL" in the previous function
        if(!is.null(m)) {  # if the inversion result is there message getting cached data
	        message("getting cached data")
                return(m) # return the calculated inversion
        }
        data <- x$get()  # if not, we do x$get to get the matrix object
        m <- solve(data, ...) # take the inverse of the matrix x
        x$setMi(m) # then set at the cache
        m # returns m
}
