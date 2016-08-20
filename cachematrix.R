## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than computing it 
## repeatedly. 

## This function creates a special "matrix" object that can cache its inverse

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


## This function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
    
    inverseMatrix = x$getInverseMatrix()
    
    if (!is.null(inverseMatrix)) #Test is the inverse is not null (calculated) 
    {
        message("getting cached data") #Same as the example
        
        return(inverseMatrix) #Returns already calculated matrix
    }
    
    tempMatrix = x$getMatrix()
    
    inverseMatrix = solve(tempMatrix, ...) #function solve inverts the
    
    x$setInverseMatrix(inverseMatrix)
    
    return(inverseMatrix)    
}

