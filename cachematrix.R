## This function reads a matrix 'x' and returns a list of four functions:
## to declare (set) the matrix 'x'  set()
## to retrieve the (get) matrix 'x'     get()
## to declare (set) the inverse of 'x', 'inv'  setinv()
## to retrieve (get) the inverse of 'x'   getinv()

makeCacheMatrix <- function(x = matrix()) {
 inv <- NULL
     set <- function(y) {
          x <<- y
          inv <<- NULL
     }
     get <- function() x
     setinv <- function(Inverse) inv <<- Inverse
     getinv <- function() inv
     list(set = set, inv = inv,setinv = setinv, getinv = getinv)
}


## This function reads the output of makeCacheMatrix ('listx')
## and checks (by getinv()) to see if an inverse has already been calculated and stored 
## if not, it creates the inverse with the function solve(x) and stores it with setinv(inv)

cacheSolve <- function(listx, ...) {
inv = listx$getinv()
    if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
     }
     Matrix <- listx$get()
     inv <- solve(Matrix, ...)
     listx$setinv(inv)
     print(inv)
     
     Test <- x %*% inv
}
