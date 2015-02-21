## makeCacheMatrix is a function that creates a matrix that can cache its own inverse as an object
## cacheSolve either retreives the cached inverse of the matrix from makeCacheMatrix or calculates the inverse if it is not cached 

##  Creates a special object that is a matrix containing functions to 
## 1) Set value of matrix
## 2) get the value of the matrix
## 3) set the value of the inverse of the matri
## 4)get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

}


## Checks to see if the inverse of the matrix defined by the above function has already been calculate
##If it has been calculated the function just retrieves the inverse from the cache
## If the inverse has not been calculated, the function calculates the inverse of the special matrix defined above using solve
## and sets the value of the invers in the cache using the setInverse function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
