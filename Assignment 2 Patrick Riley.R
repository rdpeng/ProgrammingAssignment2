
mymtx<-matrix(1:4,2,2)

mymtx

mymtx2<-solve(mymtx)
mymtx2

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmtx <- function(solve) m <<- solve
        getmtx <- function() m
        list(set = set, get = get,
             setmtx = setmtx,
             getmtx = getmtx)
}

textMatrix<-makeCacheMatrix(mymtx)

cachesolve <- function(x, ...) {
        m <- x$getmtx()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmtx(m)
        m
}

cachesolve(textMatrix)

