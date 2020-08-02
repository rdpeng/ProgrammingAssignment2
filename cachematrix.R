## makeCacheMatrix is a function that initializes two objects (x and inv) and creates four functions (set, get, setinv, getinv).
## Set allows the matrix being used to calculate the inverse to be changed without reinitializing the makeCacheMatrix function by
## passing the argument for set (y) to the parent environment using <<- and setting "x" to this value. It also resets the inverse (inv) to NULL in the parent environment
## using the same <<- operator so that when the matrix is changed using set(), any inverse value that has been calculated is also reset. Get and getinv return the matrix
## and matrix inverse, respectively, and setinv sets the inverse matrix using an argument passed from cacheSolve.

##Line by line descriptions included in the functions below as well.

makeCacheMatrix <- function(x = matrix()) {     #Create function makeCacheMatrix, initialize x as an object (1x1 matrix, value NA) within the function argument.
    inv <- NULL                                 #initialize inv as an object set to null
    set <- function(y) {                        #define set function with argument y.
        x <<- y                                 #set function assigns value of y to the object x in the parent environment using <<-;
                                                #this allows user to change the matrix being evaluated without having to re-initialize the makeCacheMatrix function.  
        inv <<- NULL                            #set also assigns NULL to inv in the parent environment, which ensures that matrix inverse is reset when the matrix is changed.
    }                                           
    get <- function() x                         #define get function with no arguments. This function returns the matrix stored in x.
    setinv <- function(inverse) inv <<- inverse #define setinv function with argument inverse, which is fed in through the cacheSolve function.
                                                #Note that setinv argument inverse should not be input directly, as it either returns an error if no argument is given,
                                                #or sets the matrix inverse to whatever input is provided, which could cause cacheSolve to return an incorrect inverse.
    getinv <- function() inv                    #define getinv function with no arguments. This function returns the inverse matrix stored in inv,
                                                #or NULL if no inverse function has been calculated yet.
    list(set = set, get = get,                  #creates a list giving each function it's matching function name.
         setinv = setinv,                       #This allows the functions to be called by name using the $ operator.
         getinv = getinv)
}

## CacheSolve takes the object containing makeCacheMatrix as an input and checks to see if an inverse has been calculated for the matrix
## stored as x in makeCacheMatrix. If an inverse is already stored cacheSolve says that it's retrieving the cached data and returns
## the inverse matrix. If no inverse matrix has yet been calculated, cacheSolve solves for the inverse matrix and then sets the inverse
## using setinv, so that next time the inverse is called (assuming the matrix has not been changed), it does not need to be calculated again.

cacheSolve <- function(x, ...) {                #Create function cacheSolve with input x; note that "x" should be whatever object makeCacheMatrix was assigned to.
    inv <- x$getinv()                           #Attempts to retrieve the inverse from the object passed in the cacheSolve argument using the getinv function.
    if(!is.null(inv)) {                         #Checks if getinv returned a NULL value.
        message("getting cached data")          #If not NULL, it returns the message "getting cached data",
        return(inv)                             #and returns the matrix inverse stored under inv to the parent environment.
    }
    data <- x$get()                             #if getinv returned a NULL value, then cacheSolve retrieves the matrix stored in x using the get function
    inv <- solve(data, ...)                     #and uses the solve function to calculate the inverse of the matrix.
    x$setinv(inv)                               #The inverse is then set to inv in the input object using the setinv function 
    inv                                         #and the inverse matrix is returned to the parent environment.
}
