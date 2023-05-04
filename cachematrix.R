
rm(list = ls())
## makecachematrix function takes a matrix as an argument,initially set the inverse 
# as null and saves whatever value that is returned by cacheSolve function
##through the set inverse function 



makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function()x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get, setinverse = setinverse, 
         getinverse = getinverse)

}


## This function gets the inverse of the matrix if it is cached through
## the getinverse function but if it is null, it solves for it and 
## cache the value in the makeCacheMatrix function through the setinverse function

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)){
        message("getting data")
        return(inv)
    }
    matrix_x <- x$get()
    inv <- solve(matrix_x, ...)
    x$setinverse(inv)
}
        ## Return a matrix that is the inverse of 'x'





####testing the function#####
get <- function() x
rm(list = ls())
get(x)
str(get)
get(3)
get()
get()5
x<- NULL
function()x
my_matrix$getinverse()
my_matrix$setinverse(25)
my_matrix$getinverse()

my_matrix<-cachematrix(matrix(1:4,2,2))
my_matrix$get()
my_matrix$set()

T_m$setinverse(5)

T_m<-matrix(3:11,3,3)
T_mcached <- cachematrix(T_m)
T_m$get()
matrix(1:9,3,3)
T_mcached$get()
my_matrix$set(matrix(1:4,2,2))
my_matrix$getinverse()
matrix_inverse(my_matrix)
my_matrix$get()
