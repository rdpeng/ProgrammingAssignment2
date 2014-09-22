##RProg - 007 Programming Assignment 2 September, 2014
## Student Name: Subhasis Datta sxdatta@gmail.com Student ID: 2955599
## Put comments here that give an overall description of what your 
## functions do 
## Write a short comment describing this function 

makeCacheMatrix <- function(x = matrix()) { 
  ##  create a null Inverse matrix
  Inv_matrix <- NULL
  set_matrix <- function (y) {
  ## create the set matrix function and set the matrix
    x <<- y
    inv_matrix <- NULL
  }
  get_matrix <- function () x
    ## Get the matrix function
  SetInverse <- function(inverse)  Inv_matrix <<- inverse
  ## Create the Inverse matrix
  GetInverse <- function() Inv_matrix
  list(set=set_matrix, get=get_matrix, SetInverse = SetInverse, GetInverse = GetInverse)
  ## Set all the values in the Inverse matrix using the functions created
}
