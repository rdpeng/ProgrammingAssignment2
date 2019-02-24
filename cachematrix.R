## In here the separate object is created to store the matrix and will cache its inverse

makeCacheMatrix <- function(x = matrix())
 {
    rev <- NULL
    ct <- function(y)
    {
        x <<- y
        rev <<- NULL
    }
    gt <- function() x
    ctinv <- function(reverse) rev <<- reverse
    gtinv <- function() rev
    list(ct = ct,
	 gt = gt,
	 ctinv = ctinv,
	 gtinv = gtinv)
}


## This function will inverse the matrix that is assigned.
## If inverse is already done it will retrieve from the cache.

cacheSolve <- function(x, ...) 
{
    rev <- x$gtinv()
    if(!is.null(rev)) 
    {
        return(rev)
    }
    store <- x$gt()
    rev <- solve(store, ...)
    x$ctinv(rev)
    rev
}
