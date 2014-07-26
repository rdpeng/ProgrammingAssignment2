## I've been doing fairly well on the quizes and past assignments but this
## one just through me for a loop. So, just hiping to get some points
## for github submission....

## Original Matrix

makeCacheMatrix <-- function(x = matrix(c(1,2,3,4), nrow=2, ncol=2)) {
  m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)

}

## Cached Matric

cacheSolve <- function(x, ...) {
                m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}
