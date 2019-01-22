##  Together makeCacheMatrix and cacheSolve take in a matrix, 
##  create and inverse matrix solve the inverse matrix and
##  cache the solution for future use.

## SAMPLE 1:
"
##  a<-makeCacheMatrix(matrix(1:4, nrow=2))

> cacheSolve(a)
[,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5

> cacheSolve(a)
hey! getting cached data
[,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
"


## SAMPLE 2:
"
b<-makeCacheMatrix(matrix(4:7, nrow=2))

> cacheSolve(b)
[,1] [,2]
[1,] -3.5    3
[2,]  2.5   -2

> cacheSolve(b)
hey! getting cached data
[,1] [,2]
[1,] -3.5    3
[2,]  2.5   -2
"

## SAMPLE 3:
"
hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
h4 <- makeCacheMatrix(hilbert(4))

> cacheSolve(h4)
[,1]  [,2]  [,3]  [,4]
[1,]   16  -120   240  -140
[2,] -120  1200 -2700  1680
[3,]  240 -2700  6480 -4200
[4,] -140  1680 -4200  2800

> cacheSolve(h4)
hey! getting cached data
[,1]  [,2]  [,3]  [,4]
[1,]   16  -120   240  -140
[2,] -120  1200 -2700  1680
[3,]  240 -2700  6480 -4200
[4,] -140  1680 -4200  2800
"

## makeCacheMatrix takes in a matrix and return 4 functions: set, get, setinv and getinv
"   set: sets the value of the matrix
    get: returns the value of the matrix
    setInv: sets the value of the inverse matrix
    getInv: gets the value of the inverse matrix
"
makeCacheMatrix <- function(x = matrix()) {
    Inv <- NULL
    set <- function(y){
        x<<-y
        Inv<<-NULL
    }
    get <- function() x
    setInv <- function(solve) Inv <<-solve
    getInv <- function() Inv
    
    list(set=set,
         get=get,
         setInv=setInv,
         getInv=getInv)

}


## this function looks to see if function is in the cache and returns it
## otherwise calculates it
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        Inv <- x$getInv()
        if(!is.null(Inv)){
            message("hey! getting cached data")
            return(Inv)
        }
        dat <- x$get()
        Inv <- solve(dat,...)
        x$setInv(Inv)
        Inv
}
