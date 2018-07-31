## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# m is used to store the result after processing. 
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                 x <<- y
                m <<- NULL
         # "<<-" will search the global symbol for assignment, # so this function will assign the value of y to the input variable x of makeCacheMatrix, 
         # instead of creating a new symbol x inside the set function, set m to NULL. 
         }
         # constant value function, return x, that is, the data that needs to be processed. 
         get <- function() x
        # Assign the value of inverse to m, which is the result after the cache processing. 
         setinverse <- function(solve) m <<-solve 
         # constant value function, return x, that is, the data that needs to be processed. 
         getinverse<- function() m
          # Function makeCacheMatrix returns a list, in which a total of four elements, 
           # each element is a function defined earlier. 
         list(set = set, get = get,
         setinverse = setinverse,
         getinverse= getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
       # x returns a makeCacheMatrix, 
       # x$getinverse is to call the getinverse function in makeCacheMatrix, 
       # is to first query whether there is a inverse before. 
       m <- x$getinverse()
        if(!is.null(m)) {
        # m is not NULL, indicating that the inverse has been calculated before, 
        # directly returns the result.       
                message("getting cached data")
                return(m)
        }
        # Function to perform this step if there is no explanation before the calculated inverse. 
        # Call inside x GET function to bring up the data to be processed from x. 
        data <- x$get()
        # Calculate the inverse. 
        m <- solve(data, ...)
        # call inside x setinverse function, the result obtained is also stored to the inside to x; 
        # next time call cacheSolve, x has been cached results, 
        # cacheSolve function before The direct return of the if query does not need to be calculated. 
        x$setinverse(m)
         # returns the result. 
        m
        
}
