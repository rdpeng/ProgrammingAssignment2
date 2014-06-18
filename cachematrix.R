
makeCacheMatrix<-function()
	{

		setMatrix<-function()         # To set the square matrix
		{
		mat<-matrix(5:8,2,2)		# To create a 2 by 2 matrix by passing values explicitly 
			       
		inv<<-NULL                     # Setting the inverse of the matrix as NULL
		}

		getMatrix<-function()          # To get the matrix that was set already
		{
		mat                            # returning the matrix 
		}

		setInverse<-function(Inverse)  # To set the inverse of the matrix
		{
		inv<<-Inverse
		}
		getInverse<-function()         # To get the inverse of the matrix
		{
		inv
		}
	
       		cacheSolve(list(setMatrix=setMatrix,getMatrix=getMatrix,setInverse=setInverse,getInverse=getInverse))
		#Calling the function which caches the inverse of the matrix.This caching is to avoid unnecessary recomputation
     }


cacheSolve <- function(x, ...)		# Definition of the cacheSolve function
     {
	
        if(!is.null(inv))		# if already 'inv' value exists return that value without computing inverse again
	{
	message("getting cached data")
	message("Inverse of the Matrix as follows")
	return(inv)		        
	}
	else				 # else compute the inverse of the matrix (this is time consuming for large matrices)
	{
	x$setMatrix()		# To set the matrix   
	data <- x$getMatrix()		# To get the matrix into a variable 'data' 
	inv <- solve(data)		# Finding the inverse of the matrix

   
	x$setInverse(inv)		# To set the above found inverse value of the matrix
	inv <- x$getInverse()		# To get the inverse of the matrix into a variable 'inv' 
	return(inv)			# Returning 'inv' for the very first time only
	 }
                
     

  }	
