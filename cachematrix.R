## In here the separate object is created to store the matrix and will cache its inverse

makeCacheMatrix <- function(x = matrix())
 {
    rev <- NULL		## to hold the matrix inverse value I assigned NULL value to rev
    ct <- function(y)    ## Creating new function
    {
        x <<- y		
        rev <<- NULL	## Any other new matrix is there then this will reset the rev to NULL
    }
    gt <- function() x
    ctinv <- function(reverse) rev <<- reverse
    gtinv <- function() rev	## Wherever the rev value is called it will get the value of it
    list(ct = ct,
	 gt = gt,
	 ctinv = ctinv,
	 gtinv = gtinv)
}


## This function will inverse the matrix that is assigned.
## If inverse is already done it will retrieve from the cache.

cacheSolve <- function(x, ...) 
{
    rev <- x$gtinv()		## will get the rev value from the makecachematrix function
    if(!is.null(rev)) 		##  Checking if the rev is not NULL
    {
        return(rev)		## returns the rev value
    }
    store <- x$gt()		
    rev <- solve(store, ...)	## this will set the inverse
    x$ctinv(rev)
    rev
}
