## These functions were created to create a matrix and calculate the inverse
## By this moment I only created solution for a 2x2 matrix
## I really try to create a function to work with 3x3 and spent a lot of time learning
## about cofactors, adjugates, but no way to make it work

## This function creates a matrix and determine the inverse, 2x2 matrix is the
## default
makeCacheMatrix <- function(x = matrix(data = 1:4, nrow = 2, ncol = 2)) {   ## matrix function 
    xa <<- x[1,1]  ## set values from a to d
    xb <<- x[1,2]
    xc <<- x[2,1]
    xd <<- x[2,2]
    mat1 <<- as.numeric(c(xd,-xc,-xb,xa)) ## change variables: a with d, b and c turn negative to calculate the determinant
    mat1 <<- matrix(mat1, 2, 2) ## create a matrix with new variables, default is 2x2
}

## This function multiplies the inverse matrix calculated by the previous 
## step, also uses as default the result of the previous function
  ## By this moment only works for 2x2 matrix
cacheSolve <- function(x, ...) {
    inv <<- mat1 / c(xa*xd-xb*xc)  ## div matrix by the determinant:  a*d-b*c
    print(inv)  ## prints the inverse matrix & saves the result in the environment
  }        

