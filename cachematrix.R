## In this assignment, we are creating a list that contains a matrix, either
## a matrix inverse or NULL, a function to get the matrix out, and a function
## that either gets the matrix inverse if it's already been solved for, and if
## it's not, invert the matrix, then store the matrix inverse so that you don't
## have to calculate it agian.

## In other words, it's a really, really, really convoluted (dare I say dumb?)
## way of implimenting an object.

## Furthermore, we are creating another function which extracts the inverse
## of the matrix if it's already been calculated, or calculates the matrix
## inverse and tells the quasi-object what the matrix inverse is if it hasn't
## already been calculated.

## This function constructs the list described above 
## (which is masquerading as an object).

makeCacheMatrix <- function(x = matrix()) 
{
	inverse = NULL
	set_matrix <- function(y)
	{
		x <<- y
		inverse <<- NULL
	}
	get_matrix <- function() x
	set_inverse <- function(the_inverse) inverse <<- the_inverse
	get_inverse <- function() inverse
	list(set_inverse = set_inverse, get_inverse = get_inverse,
	     set_matrix = set_matrix, get_matrix = get_matrix)
}


## This function returns the inverse stored in x if it has already been
## calculated.  If not, it calculates the inverse, sets in inverse in x,
## then returns the inverse.

cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
	inverse <- x$get_inverse()
	if(!is.null(inverse))
	{
		message("Getting cached data")
		return(inverse)
	}
	matrix <- x$get_matrix()
	matrix_inverse <- solve(matrix)
	x$set_inverse(matrix_inverse)
	matrix_inverse
}
