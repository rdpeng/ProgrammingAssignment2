#creation of the function that will evaluate if the matrix already exist with its inverse
makeCacheMatrix <- function(x = matrix())
{
        mymatrix <- NULL
        set <- function(y)
{
                        x <<- y #reassignation of "x"
                        mymatrix <<- NULL
         }
        get <- function() x #return the matrix specified by the user
        setinv <- function(inv) mymatrix <<- inv #will be the inverse matrix of the specified by the user
        getinv <- function() mymatrix #if the matrix already has its inverse calculated, this will be return when the function cacheSolve runs
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv) #the components of the list


}

#this function will calculate te inverse matrix, and will save it in memory
cacheSolve <- function(x, ...)
{
        mymatrix <- x$getinv() #takes the value of the matrix created in the function makeCacheMatrix
        if(!is.null(mymatrix)) #if the inverse matrix has been calculated before,it will return that matrix
        {
                message("getting cached data")
                return(mymatrix)
        }
        data <- x$get() #if the inverse matrix hasn't been calculated, it will be
        mymatrix <- solve(data, ...)
        x$setinv(mymatrix)
        mymatrix #it will take the inverse matrix calculated
