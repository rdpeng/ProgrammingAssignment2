

## There are 2 functions makeCacheMatrix , cacheSolve
## makeCacheMatric consists of set, get , setinverse , getinversse




makeCacheMatrix <- function(x = matrix())
{
        inv <- NULL        # initializing inv as Null
        
        set <- function(y){
                          x   <<- y
                          inv <<- NULL
        }
        get <- function(){x}
        setInverse <- function(inverse) { inv <<- inverse}
        getInverse <- function()  {inv}
        list( set=set , get=get , setInverse=setInverse , getInverse=getInverse)
        
}


cacheSolve <- function(x, ... )
{
        inv <- x$getInverse()
        if (!is.null(inv)){
                message("your data is being cached")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ... )
        x$setInverse(inv)
        inv                ## The last Expression gets printed
}
