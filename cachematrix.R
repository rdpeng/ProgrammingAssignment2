#Create the function that have as argument a matrix called x:

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
    set <- function(y){
        x <<- y
        inverse <<- NULL
    }
    # get is a function that gets the matrix x:
    get <- function()x
    
    # set the inverse of the matrix, assign the name of inverse to it:
    setinverse <- function(inverse) inverse<<-inverse
    
    #getinverse returns the inverse variable 
    getinverse <- function() inverse
    
    #The function makeCachematrix returns a list of methods defined above:
    list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

 ## cacheSolve return a matrix that is the inverse of 'x'
#makeCachematrix will be passed as argument in this function:
cacheSolve <- function(x, ...) {
    #If inverse variable is not null, R will return the following message and the inverse matrix   
    inverse <- x$getinverse()
    if(!is.null(inverse)){
        message('getting cached data')
        return (inverse)
    }
    #get the matrix x from the previous function:
    data <- x$get()
    #calculate the inverse matrix and store the result in a variable called inverse:
    inverse <- solve(data,...)
    
    #Set the argument for setinverse equal to inverse (the inverse matrix)
    x$setinverse(inverse)
    #Return the variable that contains the inverse matrix
    inverse
}
