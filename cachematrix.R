makeCacheMatrix <- function(matrix = matrix()) 

	{
		mat_inv <- NULL

        	set <- function(y = numeric()) 
				{
                		matrix <<- y

	 	               mat_inv <<- NULL
       			 }
       
		get <- function() matrix

      	setinverse <- function(inverse) mat_inv <<- inverse
        	
		getinverse <- function() mat_inv
		list(
				set = set, get = get,
          		setinverse = setinverse,
          		getinverse = getinverse
			)
	}


## Write a short comment describing this function

cacheSolve <- function(matrix, ...) 

	{
     	mat_inv <- matrix$getinverse()
        	
		if(!is.null(mat_inv)) 
			{
               	message("getting cached data")
                
				return(mat_inv)
        		}
        
		else 

			{

				message("computing inverse of the matrix")

				data <- matrix$get()

				matrix$setinverse(solve(data))
		
				mat_inv <- matrix$getinverse()
			}

		
		mat_inv

	   ## Return a matrix that is the inverse of 'x'
	}
