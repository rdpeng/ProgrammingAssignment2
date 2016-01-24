## This programming assingment involves two functions.
##  1) makeCacheMatrix
##  2) CacheSolve
###############################################################################
#################### Function 1)  "MakeCacheMatrix  ###########################
###############################################################################
##
## The first function will produce an Matrix using the Array function, and runif
##   --  function with an option by user to include:
##   --   1) to set the desired set.seed (default is 10)
##   --   2) to set the desired vector number (e.g. 25 is the default).
##   --   3) to set the desired number of row. (default is set to 5).
##   --   4) to set the desired number of column (default is 5)

# - Start of Function -
makeCacheMatrix <- function(my_Seed = 10, n = 25, number_Row = 5, number_Column = 5 ) {
        set.seed(my_Seed)

  # create the matrix from above user input. If no user input then default is used.
        base_Matrix <- array(runif(n), dim = c(number_Row, number_Column))

  # This n_base_Matrix -  will be used on the next function CacheSolve
        n_base_Matrix <- number_Row * number_Column

  # Shows information of the produced Matrix (base_ Matrix).
        cat("This is a", number_Row,"by", number_Column, "matrix of n=" ,n,
            "using a set.seed of",my_Seed, "\n", "\n")

        print(base_Matrix)

  # Shows INVERSE of the square Matrix
        cat("\n","This is the Inverse of the above Matrix", "\n")
        inverse_base_Matrix <<- solve(base_Matrix)
        print(inverse_base_Matrix)

# Save all necessary results to memory as cache to be use by the succeeding function
#    -- The CacheSolve-----
#
        current_base_Matrix <<- base_Matrix
        current_inverse_base_matrix <<- inverse_base_Matrix
        current_my_Seed <<- my_Seed
        current_n_base_Matrix <<- n_base_Matrix

}
# - End of function -
###############################################################################

###############################################################################
#############  Function 2) : cacheSolve     ###################################
###############################################################################

# - Star of Function 2

   # Default set.seed = 10 similar to the function makeCacheMatrix
   #  - to prevent program from crashing. Otherwise if there is no default,
   #  - program run into error.
cacheSolve <- function(my_Seed = 10) {

        if (my_Seed == current_my_Seed) {
           # if set.seed is equal to the previous set.seed used
           #   - in makeCacheMatrix, then just show the previous inverse
           #   - result of the Matrix. Also the following columns saved all
           #   - necessary result to memory
           #
           #   This blocked of code below tries to extract what the previous
           #   matrix dimension's number of rows and number of columns. These are
           #   concatenated and will be shown in the console.
                dimCurrent_base_Matrix <- dim(current_base_Matrix)
                rowNumber <- dimCurrent_base_Matrix[1]
                colNumber <- dimCurrent_base_Matrix[2]

                cache_rowNumber <<- rowNumber
                cache_colNumber <<- colNumber

                cat("This is the same", rowNumber ,"by", colNumber, " Matrix using a set.seed of",my_Seed, "\n")
                cat("Matrix info : ",cache_rowNumber, "by", cache_colNumber, "n =",current_n_base_Matrix, "\n", "\n")

                return(current_inverse_base_matrix)

        }

        # if the entered set.seed by the user is not equal to the makeCacheMatrix,
        # - then this new set.seed will be use to calculate the inverse....
        set.seed(my_Seed)

  #   Those saved variable from memory (see previous makeCacheMatrix function), are used to
  #     - mimic the same Matrix.
        base_Matrix <- array(runif(current_n_base_Matrix), dim = c(cache_rowNumber, cache_colNumber) )

        new_Inverse_Matrix <- solve(base_Matrix)

  # Code to show in the console which serve as a definition of the matrix
        cat("\n","This is the new Inverse of the above Matrix","using set.seed =",  my_Seed,"\n")
        cat("Matrix info= ", cache_rowNumber, "by", cache_colNumber, "n =",current_n_base_Matrix, "\n", "\n")

        print(new_Inverse_Matrix)



}

##  -End of function  cacheSolve
#############################################################################
