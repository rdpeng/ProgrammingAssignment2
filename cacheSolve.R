cacheSolve <- function(x, ...) { ## Uses cached inverted matrix or generates a new one
  m <- x$solve() ## looks for inverted matrix value generated in another enviroment
  if(!is.null(m)) { ## In case that m is emptly, uses the already defined value
      message("getting cached data")
      return(m)
  }
  data <- x
  m <- solve(data, ...) ## if matrix was not inverted before, it calculates the inverted matrix
  x$solve(m)
  m ##returns the inverted matrix values
}
