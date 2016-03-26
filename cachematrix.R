makeCacheMatrix <- function(x = matrix()) {
inversematrix <- NULL
set <- function(y)
{
x <<- y
inversematrix <<- NULL
}
get <- function() x
setIM <- function(inversematrix) inv <<- inversematrix
getIM <- function() inversematrix
list(set = set,get = get,setIM = setIM,getIM = getIM)
}

cacheSolve <- function(x, ...) {
   inversematrix <- x$getInverse()
        if (!is.null(inversematrix)) {
                message("Cached Values")
                return(inversematrix)
        }
        matrix <- x$get()
        inversematrix <- solve(matrix, ...)
        x$setInverse(inversematrix)
        inversematrix
}
