## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
 

   inv=NULL
   # Following the same format as the assignment example
	# Creating a makeCacheMatrix object will consist of
	# four functions encapsulated in a list
	# 1. set the matrix
	# 2. get the matrix
	# 3. set the inverse of the matrix
	# 4. get the inverse of the matrix

	# Initially set to NULL
	# Changes when the user sets the value
	#set the matrix but not inverse
   set<- function(y){
   
       x<<-y
	   inv=NULL
   }
     get<- function() x #gets matrix..not inverse
	 
	 setinverse <-function(inverse) inv<<- inverse #set the inverse
	 getinverse <-function() inv #getinverse
	 
	 list(set= set, get = get, setinverse = setinverse, getinverse=getinverse) #encapsulate




}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {

 # Return a matrix that is the inverse of 'x'
    # Following the same format as the assignment example

    # Get the current state of the inverse and see if it
    # has been computed yet

           inv <- x$getinverse() 
		   
		   #if present
		   if(!is.null(inv)){
		      meassage("getting the cached data")
			  return(inv)
		   }
		   #if not present then...compute
		   data<-x$get()
		   #find the inverse
		   inv <- solve(data, ...)
		   #cache it
		   x$setinverse(inv)
		   #return
		   inv
      
        ## Return a matrix that is the inverse of 'x'
}
