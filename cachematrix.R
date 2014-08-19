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

#' Create a vector class that contains caching for matrix inverses
#'
#' @param x A matrix that will have its inverse calculated
makeCacheMatrix <- function(x = matrix()) {

}


#' Pull a file down from the interwebs if it hasn't been already
#'
#' @param url Path to the source data
#' @param destfil Relative or absolute system path to where the data
#'                is to be saved
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
}
