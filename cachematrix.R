## Those functions cache the inverse of matrix and store it in an object

##makeCacheMatrix create an object to store matrix and there inverse in cache
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) i <<- inv
        getinverse <- function() i
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## If the inverse is not stored in cache cacheSolve calculates it using a gaussian elimination and return the inverse
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	y <- x$getinverse()
	if (!is.null(y)){
		print("get the cache")
		return(y)
	}
	m <-x$get()
	r <- 0
	id <- diag(ncol(m))
	for (j in 1:ncol(m)){
		k <- which(m[,j] == max(abs(m[,j])))
		if (m[k,j]!= 0){
			r <- r+1
			id[k,] <- id[k,]/m[k,j]
			m[k,] <- m[k,]/m[k,j]
			if (r==k) {
				tampon <- m[r,]
				tampon2 <- id[r,]
				m[r,] <- m[k,]
				id[r,] <- id[k,]
				m[k,] <- tampon
				id[k,] <- tampon2
			}
			for (i in 1:nrow(m)){
				if (i != r){
					id[i,] = id[i,] - m[i,j] * id[r,]
					m[i,] = m[i,] - m[i,j] * m[r,]
				}
			}
		}
	}
	x$setinverse(id)
	id
}
