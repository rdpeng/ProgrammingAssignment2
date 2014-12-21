#==============================================================================
#            Caching a Matrix and its Inverse in a Function Object
#------------------------------------------------------------------------------
# This code has been written as part of Programming Assignment in partial 
# fulfillment of the online course called 'R Programming' at 
# https://class.coursera.org/rprog-016
# Author : Alok Swain
# Date   : Dec 20th 2014
# 
#------------------------------------------------------------------------------
# In R Programming language and Linear Algebra, Matrix Inversion can be a costly 
# computation and there is some benefit to caching the inverse of a matrix rather 
# than repeatedly computing it if the matrix doesn't change. Below there are two 
# functions that cache the inverse of a matrix using function closures.
#==============================================================================

#==============================================================================
# makeCacheMatrix(): This function creates a special "matrix" object that can 
#                    cache the matrix along with its inverse.
#       x (Input)  : An invertible square matrix of 2 dimensions to store.             
#                    This is an optional argument. If nothing is passed then
#                    an empty matrix object is stored.
#         (Output) : It returns a list of functions 
#                    get() - It fetches the matrix x that was passed as an 
#                            argument to the function object when it was called
#                            initially and stored locally in this function.
#                            If no matrix was called during the initial function 
#                            creation then an empty matrix is returned.
#                    set() - It allows to store a new matrix to replace the existing
#                            matrix if any stored in this function object. 
#                    getinverse() - It allows to get the inverse of the matrix object
#                            passed during the function object creation.
#                    setinverse() - It allows to replace the inverse of the matrix object
#                            stored with a new inverse.
#------------------------------------------------------------------------------
makeCacheMatrix <- function(x = matrix()) {
            inv <- NULL #inv <- matrix()
            set <- function(y) {
                    x <<- y
                    inv <<- NULL #inv <- matrix()
            }
            get <- function() { x }
            setinverse <- function(arg_inverse) { inv <<- arg_inverse }
            getinverse <- function() { inv }
            list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}
#==============================================================================


#==============================================================================
# cacheSolve(): This function computes the inverse of the matrix stored inside
#               special "matrix" function object returned by `makeCacheMatrix` 
#               above. 
#               If the inverse has already been calculated (and the matrix has 
#               not changed), then it should retrieve the inverse from the cache.
#   x (Input) : A special "matrix" function object returned by `makeCacheMatrix`
#    (Output) : Return a matrix that is the inverse of the matrix stored
#               in function object 'x'.
#------------------------------------------------------------------------------
cacheSolve <- function(x, ...) {
            m <- x$getinverse()
            if(!is.null(m)) {
                    message("getting cached data")
                    return(m)
            }
            data <- x$get()
            m <- solve(data, ...)
            x$setinverse(m)
            m
}
#==============================================================================



#==============================================================================
# Testing
# Test code to reproduce the output from the code above to check its validity.
#------------------------------------------------------------------------------
# a) Source this file.
#
#> 
#> source("cachematrix.R")
#> 
#------------------------------------------------------------------------------
# b) Set seed to some arbitary value and create a 3x3 random matrix
#
#>
#> set.seed(10)
#> xmat <- matrix(rnorm(1:9), 3, 3)
#> xmat
#            [,1]       [,2]      [,3]
#[1,]  0.01874617 -0.5991677 -1.208076
#[2,] -0.18425254  0.2945451 -0.363676
#[3,] -1.37133055  0.3897943 -1.626673
#> 
#------------------------------------------------------------------------------
# c) Pass the matrix above to create and store it in the special matrix function 
#    object.
#
#>
#> xspmat <- makeCacheMatrix(xmat)
#>
#------------------------------------------------------------------------------
# d) Pass it to function cacheSolve() to calculate the inverse matrix.
#
#>
#> inverse <- cacheSolve(xspmat)
#> inverse
#           [,1]      [,2]       [,3]
#[1,]  0.6404624  2.744233 -1.0891800
#[2,] -0.3777843  3.202915 -0.4355093
#[3,] -0.6304550 -1.545961  0.1990977
#> 
#------------------------------------------------------------------------------
# e) Call function cacheSolve() to calculate the inverse matrix again.
#    Check that it is getting the cached value and the matrix returned is the 
#    same as the one above.
#>
#> inverse1 <- cacheSolve(xspmat)
#getting cached data
#> inverse == inverse1
#     [,1] [,2] [,3]
#[1,] TRUE TRUE TRUE
#[2,] TRUE TRUE TRUE
#[3,] TRUE TRUE TRUE
#> 
#------------------------------------------------------------------------------
# f) Check to see that the inverse returned is valid by muliplying it to the 
#    original matrix and check if we get an identity matrix.
#
#> 
#> round(xmat %*% inverse)
#     [,1] [,2] [,3]
#[1,]    1    0    0
#[2,]    0    1    0
#[3,]    0    0    1
#> 
#------------------------------------------------------------------------------
# g) Change the matrix stored in the function object to a random 4x4 matrix.
#    Invert it and then check to see if we got the correct inverse.
#
#> 
#> xspmat$set(matrix(rnorm(16), 4, 4))
#> inverse <- cacheSolve(xspmat)
#> round(xspmat$get() %*% inverse)
#     [,1] [,2] [,3] [,4]
#[1,]    1    0    0    0
#[2,]    0    1    0    0
#[3,]    0    0    1    0
#[4,]    0    0    0    1
#> 
#------------------------------------------------------------------------------
# h) We do. Yay! Bravo. Done.
#==============================================================================
