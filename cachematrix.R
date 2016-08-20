## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) #Call from root environment E-L1
{
	
	mtxMNC <- matrix(); #Matrix not-cached (MNC)
	setMatrix <- function (mtxMC) #Call from environment E-L2
	{
		x <<- mtxMC #Preserves a "copy" of the cache matrix in E-L2
		mtxMNC <<- NULL #Following the logic in the example, not sure if this is necesary or why ¿?
	}
	get <- function() x 
	
	
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
