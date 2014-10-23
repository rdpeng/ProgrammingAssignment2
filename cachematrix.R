## this two functions calculate the inverse of a given matrix. it works little faster than the contemporary ways as it caches the values if you have calculated it earlier. as gives faster results by looking in to the cache values
##creates cache and store the value and gives faster results on repeted calculations

## this function converts the given matrix values into the list of values and assign it to the set ,get,setsolve, and getsolve in the list format

makeCacheMatrix <- function(x = matrix())
{
        m<-NULL
        set<-function(y)
         {
             x <<-y
             m <<-NULL
         }
        get<-function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set=set, get=get,
                       setsolve=setsolve,
                       getsolve=getsolve)

}


## this function checks if the inverse values af the given matrix have previously been calculated if yes ,it returns the previous value else it calculate the inverse show the result as output and store it. 
cacheSolve <- function(x, ...) 
{
         m <- x$getsolve()
         if(!is.null(m))
         {
               message("getting cached data")
               return(m)
           }
         matrix <- x$get()
         m <- solve(matrix, ...)
         x$setsolve(m)
         m
     

        ## Return a matrix that is the inverse of 'x'
}
