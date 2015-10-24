## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function accepts a square invertible matrix as a parameter
## It sets the value of m in the local enviroment to NULL, 
## it sets the value of m in the global enivorment to NULL initially
## It creates functions to firstly, get the value of the matrix assigned to x
## create a function setimat based on the solve() function, m is assigned the value of the solve function
## then it creates a function getimat using m from the local enviroment first if it exist there
## otherwise it will use m from the global enviroment.
## List then sets all functions in the local enviroment to their intended function as previously defined
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL   
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setimat <- function(solve) m <<- solve
        getimat <- function() m
        list(set = set, get = get, 
             setimat = setimat,
             getimat = getimat)
}

## Write a short comment describing this function
## Using the invertible square matrix passed to the makeCacheMatrix above 
## cacheSolve assigns to m what it find from getimat, if it is not null then 
## it gets the inverse matrix previously assigned and returns it, otherwise if it is null
## then it passes the information assigned to x (the invertible square matrix) through get() 
## and store it in data, it then replaces the null value in m with 
## the inverted value of the matrix assigned initially to x, it then uses the setimat function in
## makeCacheMatrix to store the updated value of m and then prints the new value of m just computed
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getimat()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setimat(m)
        m
}
