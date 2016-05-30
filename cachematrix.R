## The First function named makeCacheMatrix function gets and sets the values of Inverse and parameters
## The second function actually calculates the value and stores in cache using MakeCacheMatrix function

## This function is used to store the inverse matrix values in Cache.
makeCacheMatrix <- function(x = matrix())
{
        # Two set function will set the values
        # first set takes the parent function parameter 
        # and caches it so no need to pass parameter twice
        # second set function takes the value and stores for
        # later usage
        
        #cached value variable which will be used to hold the Cached value
        # initialized to NULL
        cached_value <- NULL 
        
        #setPara function will set the global variable value to NULL
        # when the set function is called
        # in addition to that it will store the parent function
        # passed value into the x cached value
        # so when you set new value to matrix it will empty the cached variable
        setPara <- function(y)
        {
                x <<- y
                cached_value <<- NULL
        }

        # getPara function returns the parameter and passed value as it is
        getPara <- function() 
        {
                x
        }
        
                
        # setInverse function just sets the parameter value to cached_value variable
        setInverse <- function(Inversevalue)
        {
                cached_value <<- Inversevalue
        }
                
        
        
        # getInverse function will return the cached value
        getInverse <- function()
        {
                cached_value
        }
        list(set = setPara, get = getPara,
             setInverse = setInverse,
             getInverse = getInverse)
        
}


## This function checks the value and stores in the cache.
cacheSolve <- function(x, ...)
{
        # First try to access the cached value of inverse
        inv <- x$getInverse()
        # check if the value is not null
        if(!is.null(inv))
        {
                # use the cached 
                message("using cached data")
                return(inv)
        }
        else
        {
                # if the value has changed and no cached value
                # use the newly calculated value 
                # and also set the calculated cached value
                calc <- x$get()
                inv <- solve(calc)
                x$setInverse(inv)
        }        
        inv
}