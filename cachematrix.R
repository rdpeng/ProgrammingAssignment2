
	## First I set input x as "matrix"
	## after that  I set the solved value "v" as a null
	## then changed the reference from "mean" to "solve"
	    makeCacheMatrix <- function(x = matrix(sample(1:50,8),5,5)) {
	    v <- NULL
	     set <- function(y) {
	     x <<- y
	     s <<- NULL
	   }
	   get <- function() x
	   setsolve <- function(solve) v <<- solve
	   getsolve <- function() v
	   list(set = set, get = get,
	        setsolve = setsolve,
	        getsolve = getsolve)
	 }
	  ## I also did the same procedure here 
          ## I changed "mean" to "solve" and "v" to "w"
	  cacheSolve <- function(x, ...) {
	    w <- x$getsolve()
	    if(!is.null(w)) {
	      message("getting inversed matrix")
	      return(w)
	    }
	    data <- x$get()
	    s <- solve(data, ...)
	    x$setsolve(w)
	    s
	  }
