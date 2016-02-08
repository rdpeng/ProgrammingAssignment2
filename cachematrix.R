## Put comments here that give an overall description of what your
## functions do

## a special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

}


## computes the inverse of the special matrix returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}

my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
> my_matrix$get()
       [,1] [,2]
[1,]    1    3
[2,]    2    4
> cacheSolve(my_matrix)
      [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5