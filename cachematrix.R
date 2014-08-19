#' Matrix inversion is usually a costly computation and their may be some
#' benefit to caching the inverse of a matrix rather than compute it
#' repeatedly (there are also alternatives to matrix inversion that we
#' will not discuss here). Your assignment is to write a pair of functions
#' that cache the inverse of a matrix.
#'
#' @name cachematrix
#' @author Daniel Robert Couture
#' @date 2014-08-19
#'
NULL

#' This function creates a special "matrix" object that can cache its inverse.
#'
#' @param x A matrix that will have its inverse calculated
#' @return An instance with setters and getters for matrix inverse property
makeCacheMatrix <- function(x = matrix()) {

}


#' This function computes the inverse of the special
#' "matrix" returned by `makeCacheMatrix`. If the inverse has
#' already been calculated (and the matrix has not changed), then
#' `cacheSolve` should retrieve the inverse from the cache.
#'
#' @param x An instance of the makeCacheMatrix
#' @param ... Optional additional parameters for `solve(x, ...)`
#' @return The inverse of `x` either from calculated or cached
cacheSolve <- function(x, ...) {

}
