# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
   

makeCacheMatrix <- function(x = matrix()) {
        im <- NULL										#lugar de almacenamiento en el caché / Instead of Caching
        
		set <- function(y) {							#Función para establecer el valor la matriz de entrada / set the value of the matrix
                x <<- y
                im <<- NULL
        }
			
        get <- function() x								# Regresa la matriz de entrada / get the value of the input matrix
		
        setMatrixInv <- function(solve) im <<- solve	#Asigna la matriz inversa / set the value of  inverse matrix
        getMatrixInv <- function() im					#Regresa la matriz inversa /get the value of inverse of the matrix
		
        list(set = set, get = get, setMatrixInv = setMatrixInv, getMatrixInv = getMatrixInv)
		# return in list all the functions
}
   
   
# cacheSolve: This function coimputes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse froim the cache.


cacheSolve <- function(x, ...) {
        im <- x$getMatrixInv() 							#get the inverse matrix from object x 
        if(!is.null(im)) {								# If the inverse matrix was calculated and only retrieves the result
                message("getting cached data")
                return(im)
        }
        im <- solve(x$get(),...)						#if not, we get the input array and the solution is calculated
        x$setMatrixInv(im)
        im
}


# #Ejemplos/Examples
# a <- makeCacheMatrix(matrix(1:4,2))
# a$get()
# a$getMatrixInv() #primero se debe correr cacheSolve(a), ya que el valor por default es NULL
 # cacheSolve(a)
# a$get()%*%a$getMatrixInv()


# b <- diag(2,6)
# cmatrix <- makeCacheMatrix(b)
# cacheSolve(cmatrix)
# b%*%cacheSolve(cmatrix)

# x <- rbind(c(1, -1/4), c(-1/4, 1))
# m <- makeCacheMatrix(x)
# m$get()
# cacheSolve(m)
# m$get()%*%cacheSolve(m)

