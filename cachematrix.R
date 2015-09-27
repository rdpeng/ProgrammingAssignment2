# creates special "matrix" object - really a list of functions built on an input matrix b
# doesn't actually solve for the inverse itself, sets an environment for cacheSolve() to work in
# four functions and two cached values
make_cache_matrix <- function(b = matrix()) {
    inverse_matrix <- NULL
    original_matrix <<- b

    # caches input to matrix object b, and caches NULL to inverse_matrix
    set_matrix <- function(y) {
        b <<- y
        inverse_matrix <<- NULL
    }
    get_matrix <- function() b # returns the matrix b
    set_inverse <- function(inverse) inverse_matrix <<- inverse # caches input in inverse_matrix
    get_inverse <- function() inverse_matrix ## returns the inverse matrix cached in inverse_matrix

    list(set_matrix = set_matrix, get_matrix = get_matrix,
         set_inverse = set_inverse, get_inverse = get_inverse) # makes these functions into a list
    }

# if there's a cached inverse, and we still have the original matrix, returns the cached value
# if either isn't true, gets and solves for a new matrix
# throws an error if the new matrix isn't solvable (or isn't a matrix) courtesy of solve()
cache_solve <- function(b, ...) {
    inverse_matrix <- b$get_inverse()
    if ((!is.null(inverse_matrix)) && b$get_matrix() == original_matrix) { 
        return(inverse_matrix)
    }
    input_matrix <- b$get_matrix()
    inverse_matrix <- solve(input_matrix)
    b$set_inverse(inverse_matrix)
    inverse_matrix
}