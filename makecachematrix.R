makeCachematrix <- function(x = matrix()) {
  solve <- NULL
  set <- function(y) {
    x <<- y
    solve <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) solve <<- solve
  getsolve <- function() solve
  list(set = set, get = get, setsolve = setsolve,  getsolve = getsolve)
}