## This function makes a list of 4 functions to obtain the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
stored_inverse <- NULL #initializes the inverse matrix to NULL
        set <- function(y) {
                x <<- y
                stored_inverse <<- NULL
        } #Set the x (matrix) and the stored_inverse for the function environment
        get <- function(y) return(x) #creates the function get in the makeCacheMatrix
        setinverse <- function(sent_replacement_inverse) stored_inverse <<- sent_replacement_inverse
        #Takes the inverse and sets it to the inverse in the makeCacheMatrix
        getinverse <- function() return(stored_inverse) # returns the inverse matrix form the makeCacheMatrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse) #Makes a list of the 4 functions
}


## This function calculates the inverse of a matrix if not already done so.
## Otherwise it will get the inverse matrix from the chached data.

cacheSolve <- function(x, ...) {
        local_inverse <- x$getinverse() #assigns the inverse matrix of the makeCacheMatrix (x) to this one.
        if(!is.null(local_inverse)) {
                message("getting cached data")
                return(local_inverse)
        } # if the local_inverse matrix not zero, the function takes the inverse matrix from the Cached data
        local_data <- x$get() #if this matrix has never been evaluated, the matrix is made the local matrix.
        local_inverse <- solve(local_data, ...) #the inverse of this matrix is calculated
        x$setinverse(local_inverse) #assign the calculated inverse matrix to the makeCacheMatrix function
        return(local_inverse) #display the inverse matrix
}
