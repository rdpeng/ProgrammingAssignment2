## These functions create a matrix and find its inverse.
## They also determine whether an inverse has already been calculated for a matrix before calculating a new inverse. 

library(pryr)

## The number of values and the numnber of columns should produce a square matrix, as demonstrated in the defaults below.
## The default environment is the Global Environment, but this can be changed in the function arguments.
makeCacheMatrix <- function(matrix.name = "test.matrix", values = 16, columns = 4, environment = .GlobalEnv){
        x <- matrix(rnorm(values), ncol = columns)
        assign(matrix.name, x, envir = environment)
}

## This function determines whether an inverse exists in the specified environment. 
## (Your environment should be the same as the environment in makeCacheMatrix.)
## Then the function either prints "Inverse already exists", and the location where it is cached, 
## or the function calculates and caches the new inverse.
## You must first create a matrix with makeCacheMatrix before you can search for and calculate its inverse.
cacheSolve <- function(matrix.name = "test.matrix", environment = .GlobalEnv){
        ## We name inverses consistently so we can find them in the given environment.
        inverse.name <- paste0(matrix.name, ".inv")
        
        ## If an inverse exists, it is printed.
        if(exists(inverse.name, envir = environment)){
                print("Inverse already exists")
                where(inverse.name)
                
        ## If an inverse doesn't exist, an inverse is calculated and saved to the given environment.
        } else {
                new.inverse <- solve(get(matrix.name))
                outputname <- paste0(matrix.name, ".inv")
                assign(outputname, value = new.inverse, envir = environment)
        }
}

## Example:
makeCacheMatrix("test.matrix2")
cacheSolve("test.matrix2")



