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
        cat("This is the base matrix using a set.seed of",my_Seed, ", a", number_Row,
              "by", number_Column, "Matrix", "\n", "\n")
        print(base_Matrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
