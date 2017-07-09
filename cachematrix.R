## normal example
myMatrix <- matrix(c(1,3,2,5,1,0,0,8,5), nrow=3, ncol=3)
myMatrix
invmat <- solve(myMatrix)
unimat <- myMatrix %*% invmat
unimat ## results in matrix 
##	1	0	0	
##	0	1	0
##	0	0	1 


##########################
## Coursera Data Specialisation / course 2 R-programming / Week 3 / cacheMatrix.R)
## Two functions to fasten the calculation of repeated inverse matrix calculations
## The idea is to read any secondary inverse from cache instead of calculate it again.

## First function creates a list(set, get, setsolve, getsolve)
## to know if the inverse is already calculated
## The scope is the environment where the inverse is calculated
## brought to the calling environment by <<-
## The examples makeVector were acually very useful :-)
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## Second function returns the value from cache unless it does not yet exist
## the function requires on object of the tyoe makeCacheMatrix, i.e. a list with set, get, setsolve, getsolve
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}

## we may test the functions by the following example 
myMatrix <- matrix(rnorm(1000000), nrow=1000, ncol=1000) * 10
myList <- makeCacheMatrix(myMatrix)
	a <-  Sys.time()
first <- cacheSolve(myList)
	b <-  Sys.time()
second <- cacheSolve(myList)
	c <-  Sys.time()
	b-a ; c-b
#uni <- myLlist %*% first

myMatrix2 <- matrix(rnorm(1000000), nrow=1000, ncol=1000) * 10
myList2 <- makeCacheMatrix(myMatrix2)
system.time(cacheSolve(myList2)) ## system.time calculates cpu time for trunnig the funclion calles
system.time(cacheSolve(myList2))
## and back to myList
third <- cacheSolve(myList)
## any new assignment of myList2 (even if it has the same myMatrix2 as input) will reset m to NULL
# myList2 <- makeCacheMatrix(myMatrix2)
# first <- cacheSolve(myList2)
