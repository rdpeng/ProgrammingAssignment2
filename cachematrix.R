## This function creates a special "matrix" object that can cache its inverse.
##Ex-Create: mcm <- makeCacheMatrix(rbind(c(1,3), c(2,4)))  ##input is matrix by row/col: 1,3   2,4
##Ex-get: mcm$get()  ##return original matrix used as input
##Ex-get: mcm$set(iMtx)  ##Result should be, by row/col: -2,1.5   1,-0.5
##Ex-set invert: mcm$setInvertMtx(solve(mcm$get()))  ##Sets cached version of inverted matrix
##ex-Get invert: mcm$getInvertMtx()

makeCacheMatrix <- function(x = matrix()) {
	im <- NULL
	##Reset cached values of input matrix and "special" inverted matrix
	set <- function(mtxIn) {
		x <<- mtxIn
		im <<- NULL
	}
	##Return value of input matrix
	get <- function() x
	##Set the cached version of the inverted matrix 
	setInvertMtx <- function(solve) im <<- solve
	##Return the cached version of the inverted matrix (Note, must be set via setInvertMtx call first)
	getInvertMtx <- function() im 
	list(set = set, get = get,
		setInvertMtx = setInvertMtx, getInvertMtx = getInvertMtx)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve should retrieve the inverse from the cache.
##Ex: cacheSolve(mcm)   ##mcm must have been created in previous step
cacheSolve <- function(x, ...) {
        ## Attempt to retrieve inverted matrix from the cache
		im <- x$getInvertMtx()
		##If inverted matrix is found in cache, return it
		if(!is.null(im)) {
			message("Getting cached inverted matrix")
			return(im)
		}
		##otherwise, matrix has not been inverted so...
		data <- x$get()
		## ...Invert it, 
		im <- solve(data)
		##...cache the inverted version of the matrix, 
		x$setInvertMtx(im)
		##..and return the inverted version
		im
}
