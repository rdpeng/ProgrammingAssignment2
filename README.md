Assignment Instructions
_________________________________________________
### Introduction

This second programming assignment will require you to write an R
function that is able to cache potentially time-consuming computations. For
example, taking the mean of a numeric vector is typically a fast
operation. However, for a very long vector, it may take too long to
compute the mean, especially if it has to be computed repeatedly (e.g.
in a loop). If the contents of a vector are not changing, it may make
sense to cache the value of the mean so that when we need it again, it
can be looked up in the cache rather than recomputed. In this
Programming Assignment you will take advantage of the scoping rules of the R
language and how they can be manipulated to preserve state inside of an
R object.

### Example: Caching the Mean of a Vector

In this example we introduce the `<<-` operator which can be used to
assign a value to an object in an environment that is different from the
current environment. Below are two functions that are used to create a
special object that stores a numeric vector and caches its mean.

The first function, `makeVector` creates a special "vector", which is
really a list containing a function to

1.  set the value of the vector
2.  get the value of the vector
3.  set the value of the mean
4.  get the value of the mean

__________________________________________________________________________________________________
# Creating makeCacheMatrix function to set and get the value of vector.
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
#If it find out mean is already created, it skips the computation and provide the output

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