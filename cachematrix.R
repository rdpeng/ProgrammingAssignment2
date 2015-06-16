## Together, these functions can find the inverse of a matrix,
## and cache it. Once it is cached, if we try to calculate the 
## inverse again, it retrieves the inverse from the cache 
## instead of calculating it again.

## makeCacheMatrix returns a list of functions. set allows us 
## to define the matrix which we want to know the inverse of, get
## retrieves that matrix, setinv stores the inverse, and getinv
## returns the inverse (if it has been calculated, otherwise it
## returns NULL).

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}



## cacheSolve calls on makeCacheMatrix. If the inverse has 
## already been calculated and cached, it returns it. Otherwise, 
## the function calculates it, caches it, and returns it.

cacheSolve <- function(x = matrix(), ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinv(m)
        return(m)
}

## Example code
make <- makeCacheMatrix()
make$set(matrix(c(1,1,2,1),2,2)) # calculates the inverse (hasn't been cached)
make$set(matrix(c(1,1,2,1),2,2)) # retrieves inverse from cache
