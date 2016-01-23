## This programming assingment involves two functions.
## The first function will produce an Matrix using the Array function, runif
##   -- with an option by user to include:
##   --   1) to set the desired set.seed (default is 10)
##   --   2) to set the desired vector number (e.g. 25 is the default).
##   --   3) to set the desired number of row. (default is set to 5).
##   --   4) to set the desired number of column (default is 5)

makeCacheMatrix <- function(my_Seed = 10, n = 25, number_Row = 5, number_Column = 5 ) {
        set.seed(my_Seed)


        base_Matrix <- array(runif(n), dim = c(number_Row, number_Column))
        cat("This is a", number_Row,"by", number_Column, "matrix of n=" ,n,
            "using a set.seed of",my_Seed, "\n", "\n")


        print(base_Matrix)

        cat("\n","This is the Inverse of the above Matrix", "\n")
        inverse_base_Matrix <<- solve(base_Matrix)

        print(inverse_base_Matrix)
# Save results to memrory
        current_base_Matrix <<- base_Matrix
        current_inverse_base_matrix <<- inverse_base_Matrix
        current_my_Seed <<- my_Seed

}



## Write a short comment describing this function

cacheSolve <- function(my_Seed) {

        if (my_Seed == current_my_Seed) {
# If the same copy same code above

                dimCurrent_base_Matrix <- dim(current_base_Matrix)
                rowNumber <- dimCurrent_base_Matrix[1]
                colNumber <- dimCurrent_base_Matrix[2]

                cache_rowNumber <<- rowNumber
                cache_colNumber <<- colNumber

                cat("This is the same", rowNumber ,"by", colNumber, " Matrix using a set.seed of",my_Seed, "\n", "\n")

                return(current_inverse_base_matrix)

        }

# if the set seed is not equal to previous do the following
        set.seed(my_Seed)

        #cat("This is the result of the same Matrix using a", my_Seed, "as the new set.seed entered",  "\n", "\n")
        base_Matrix <- array(runif(10), dim = c(cache_rowNumber, cache_colNumber) )

        new_Inverse_Matrix <- solve(base_Matrix)

        cat("\n","This is the new Inverse of the above Matrix", "\n")

        print(new_Inverse_Matrix)



}
