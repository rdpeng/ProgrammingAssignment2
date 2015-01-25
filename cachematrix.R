## I made minimal changes to the two examples, but this seems to do the job.
## Essentially, the first function caches a matrix and its inverse, and the 
##   second function calls the inverse (if available) or calculates it (if not).



## The function makeCacheMatrix() does a few things:
## 1) it initializes a shell of a matrix 
## 2) it caches itself
## 3) it caches its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## The function cache(Solve) checks if the cache has anything in it already.
## If so, it pulls the cached data. If not, it calculates the inverse. 
## It's not clear from the assignment whether it needs to check 
##   whether the matrix has changed, but if you run the two functions successively,
##   then it shouldn't be necessary. 

cacheSolve <- function(x, ...) {
   	m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
