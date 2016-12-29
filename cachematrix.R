make_cache_matrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      set_inverse <- function(inverse) inv <<- inverse
      get_inverse <- function() inv
      list(set = set,
           get = get,
           set_inverse = set_inverse,
           get_inverse = get_inverse)
}
              
cache_solve <- function(x, ...) {
      inv <- x$get_inverse()
      if (!is.null(inv)) {
            message("cached previous data")
            return(inv)
      }
      mat <- x$get()
      inv <- solve(mat, ...)
      x$set_inverse(inv)
      inv
}


m<- make_cache_matrix(matrix(c(1:4), 2, 2))
m$get()
cache_solve(m)
