# Creating makeCacheMatrix function to set and get the value of vector
#Also, to set and get the mean of vector

makeCacheMatrix <- function(x = matrix()) {
        invData <- NULL
        set <- function(y){
                x <<- y
                invData <<- NULL
        }
        get <- function() x
        setInv <- function(mean) invData <<- mean
        getInv <- function() invData
        list(set = set,
             get = get,
             setInv = setInv, 
             getInv = getInv)
}

# The below function help to compute inverse matrix retuned by makeCacheMatric
# Actually this function check whether the mean is already is already created or not
#If it find out mean is already created, it skips the computation and provide the output.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invData <- x$getInv()
            if(!is.null(invData)){
                  message("getting cached data")
                  return(invData)
            }
        data <- x$get()
        invData <- solve(data,...)
        x$setInv(invData)
        invData     
}
---------------------------------------------------------------------------------------------
#Testing my function

#Creating 2*2 matrix and assigning it to the makeCacheMatrix
> MyData <- makeCacheMatrix(matrix(c(1,2,3,4),2,2))

#Calling cacheSolve function for the first time
#Function is computing the value and returning the inverse matrix first time

> cacheSolve(MyData)
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5

#Calling the cacheSolve function again and this time it returning the value from the cache
#with the message, instead of computing again

> cacheSolve(MyData)
getting cached data
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5