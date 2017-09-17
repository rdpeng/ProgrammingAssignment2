## The function makeCacheMatrix creates a matrix for compute the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        inmtx <- NULL
        ## Set the value of the matrix
        set <- function(y) {
                x <<- y
                inmtx <<- NULL
        }
        ## Get the value of the matrix
        get <- function() x
        ## Set the value of the inverse
        setinmtx <- function(inverse) inmtx <<- inverse
        ## Get the value of the inverse
        getinmtx <- function() inmtx
        list(set = set, get = get,
             setinmtx = setinmtx,
             getinmtx = getinmtx)
}



##The second function check the data, if the data already exists, it wont compute it, else it will compute it
cacheinverse <- function(x, ...) {
        inmtx <- x$getinmtx()
        if(!is.null(inmtx)) {
                message("getting cached data")
                return(inmtx)
        }
        data <- x$get()
        inmtx <- solve(data, ...)
        x$setinmtx(inmtx)
        inmtx
}
##Test Function

a<-matrix(c(3,4,5,5,3,2,3,5,6),3,3)

det(a)!=0
[1] TRUE

mat<-makeCacheMatrix(a)

mat$get()
     [,1] [,2] [,3]
[1,]    2    5    8
[2,]    3    6    9
[3,]    4    7   10

cacheinverse(mat)
       [,1]   [,2]   [,3]
[1,]  1.000 -3.000  2.000
[2,]  0.125  0.375 -0.375
[3,] -0.875  2.375 -1.375
