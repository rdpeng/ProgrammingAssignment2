makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      
      #set the matrix to the new matrix input; set inverse to null
      set <- function(y){
            x <<- y
            inv <<- NULL
      }
      
      #display the matrix to user
      get <- function() x
      
      #set the inverse to the input (I don't understand why this is being made 
      #visible to the user, the user could just input some grbage values)
      setInverse <- function(invers) inv <<- invers

      #display the inverse of the matrix to user
      getInverse <- function() inv
      
      #return the functions to the user as a list
      list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
      #get the inverse value of the matrix from the makeCacheMatrix function
      inv <- x$getInverse()
      
      #check if the inv is null.
      if(!is.null(inv)){
            #inv is not null. This means the inverse has already been calculated.
            #return the previously stored inverse to the user. Display message to 
            #user to indicate that cached data is being returned.
            message("getting cached data")
            
            #exit function
            return(inv)
      }
      
      #inv was null. We will have to calculate the inverse. First get the matrix
      data <- x$get()
      
      #solve matrix for inverse
      inv <- solve(data, ...)
      
      #set inverse of matrix using the setInverse() function
      x$setInverse(inv)
      
      #return the inverse to the user
      inv
}
