## Put comments here that give an overall description of what your
## functions do

##Peer-Graded Assignment
##Programming Assignment 2 : Lexical Scopping
##author - Chinmoy Das
##email-id - chinmoy14.9@gmail.com
##github profile - https://github.com/chinmoy149

## Write a short comment describing this function
##This is the function that creates a matrix object that can cache its inverse.
makeCacheMatrix <- function (x = matrix ())
{
	inv <- NULL
	set <- function (s)
	{
		##set value to x, if the matrix is modified
		x <<- s
		inv <- NULL
		##reset the inverse of matrix x
	}
	show <- function ()
	{
		##return the current value of matrix
		x
	}
	setinv <- function (inverse)
	{
		inv <<- inverse
		##store the inverse of matrix - x
	}
	getinv <- function ()
	{
		##returns the inverse of matrix - x
		inv
	}
	matrix (c (set, show, setinv, getinv), 2, 2)
}

## Write a short comment describing this function
##computes the inverse of the matrix returned by `makeCacheMatrix`,
##and if the inv is already computed, then it returns it
cacheSolve <- function (x, ...)
{
	l <- c (x[1,], x[2,])
	##store matrix as list, so as to call the functions
	names (l) <- c ("set", "setinv", "show", "getinv")
	
	inv <- l$getinv ()
	if (!is.null (inv)) {
		print ("Inverse already cached")
		return (inv)
		##exit the function
	}
	
	mat <- l$show ()
	##store the matrix in mat
	n <- solve (mat, ...)
	l$setinv (n)
	return (n)
}