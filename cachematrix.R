## Overall Description - Developed by Steven Desmarais
#
## Program contains two functions: 1) makeCacheMatrix - create a matrix, solve 
## the inverse, and cache the result in memory for rapid retrieval;
## and 2) cacheSolve - determines if the matrix has changed and either retrieves
## the solution from cache if there has been no change or recalculates the inverse
## of the matrix.
#
## FUNCTION 1 - CREATE MATRIX AND STORE INVERSE ###############################
#
## Function creates a matrix of uniform random numbers based on a seed value
## established when the function is called. The seed value (oldSeed) and the  
## inverse of the matrix are saved in cache for use in the second function.
## Default value for the seed is 5 but it can be any value.

makeCacheMatrix <- function(ranSeed=5) {
        ## Seed value for runif function
        set.seed(ranSeed)
        ## Store seed value in cache for use by 2nd function
        oldSeed <<- ranSeed
        ## Create 5x5 matrix
        baseMatrix <- matrix(runif(25), nrow = 5, ncol = 5)
        ## Solve inverse of matrix and store result in cache
        invMatrix <<- solve(baseMatrix)
}

## FUNCTION 2 - SOLVE INVERSE OF A MATRIX ######################################
##
## Function obtains a new seed value as an argument.  It tests whether the 
## seed value is the same as used in the first function (makeCacheMatrix).
## If seed is the same, the matrix is the same and the function returns the 
## inverse of the matrix that is stored in cache.  If the seed value is 
## different, the matrix is different and a new inverse of the matrix is 
## calculated and returned.

cacheSolve <- function(ranSeed) {
        ## SAME SEED AND OLD MATRIX ######################################
        ## Test if seed value has changed
        if (ranSeed == oldSeed) {
                print("Seed is same. Return inverse stored in cache")
                cat("Old seed = ", oldSeed, "-- New seed = ", ranSeed, "\n")
                return(invMatrix)
        }
        ## NEW SEED AND NEW MATRIX ######################################
        ## Seed value for runif function
        set.seed(ranSeed)
        ## Create 5x5 matrix
        baseMatrix <- matrix(runif(25), nrow = 5, ncol = 5)
        ## Solve inverse of matrix
        invMatrixNew <- solve(baseMatrix)        
        print("New seed and new matrix. Recalculated inverse of matrix")
        cat("Old seed = ", oldSeed, "-- New seed = ", ranSeed, "\n")
        return(invMatrixNew)
}
##
## To run the program, execute the code to create both functions.  
## First, run makeCacheMatrix with any desired seed value.
## (Note: Default value for the seed is 5.)
## Second, run cacheSolve and enter a seed value. This function can be run 
## repeatedly.  The print statements indicate whether the seed value has 
## changed from the original calculation in function makeCacheMatrix.

## END OF PROGRAM
