makeCacheMatrix <- function(x = matrix()) 
{
## x: Receives a square invertible matrix. 
## Returns a list containing a function to:
        ## 1. set the Original matrix and NULL to the Inverse Variable
        ## 2. get the Original matrix 
        ## 3. set the Matrix already inverted 
        ## 4. get the Matrix already inverted 

        Inverse <- NULL # set the variable as NULL
        set <- function(NewMatrix) 
        {
                x <<- NewMatrix # Receive the new matrix value  
                Inverse <<- NULL # Defines NULL to the variable
        } # end set funtion
        get <- function() x # Shows the value of the variable "x"
        setInverse <- function(InversionCalculated)
        {
                Inverse <<- InversionCalculated # set the value of a Inversion already calculated
        } # end get funtion
        getInverse <- function() Inverse # Shows the value of the variable "Inverse"
        list(set = set, 
                get = get,
                setInverse = setInverse,
                getInverse = getInverse) # OutputList
} # end makeCacheMatrix function

cacheSolve <- function(OutputedList, ...) 
{
## OutputedList: Receives the output list from makeCacheMatrix function 
## Returns the Inverted Matrix
        
        MatrixInverted <- OutputedList$getInverse() # Get the value of variable "Inverse" from makeCacheMatrix function 
        if(!is.null(MatrixInverted)) { # Enter in this function if MatrixInverted isn't NULL
                message("Found cached data... Getting it!") # print message
                return(MatrixInverted) # break the function returned the Inverted Matrix
        } # endif
        OriginalMatrix <- OutputedList$get() # get the Original Matrix (before the inversion)
        MatrixInverted <- solve(OriginalMatrix, ...) # Calculate the Inversion
        OutputedList$setInverse(MatrixInverted) # Define the value of "Inverse" from makeCacheMatrix function 
        MatrixInverted # Returns the Inverted Matrix
}
