## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) #Call from root environment E-L1
{
	mtxMNC <- NULL #Matrix not-cached (MNC)
	
	setMatrix <- function (mtxMC) #Call from environment E-L2
	{
		x <<- mtxMC #Preserves a "copy" of the cache matrix in E-L2
		
		mtxMNC <<- NULL 
	}
	
	getMatrix <- function() { x } #Function to get the matrix in current env
	
	setInverseMatrix <- function(inverseMatrix) { mtxMNC <<- inverseMatrix } 
	
	getInverseMatrix <- function() { mtxMNC } #get the mean 
	
	list (setMatrix = setMatrix, getMatrix = getMatrix, setInverseMatrix = setInverseMatrix, getInverseMatrix = getInverseMatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    
    inverseMatrix = x$getInverseMatrix()
    
    if (!is.null(inverseMatrix)) #Test is the inverse is not null (calculated) 
    {
        message("getting cached data") #Same as the example
        
        return(inverseMatrix) #Returns already calculated matrix
    }
    
    tempMatrix = x$getMatrix()
    
    inverseMatrix = solve(tempMatrix, ...)
    
    x$setInverseMatrix(inverseMatrix)
    
    return(inverseMatrix)    
}
