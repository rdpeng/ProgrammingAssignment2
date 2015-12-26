# approach 1: create a matrix object, then use it as input to cacheSolve()

a <- makeCacheMatrix(matrix(c(-1, -2, 1, 1), 2,2))
cacheSolve(a)

# approach 2: use makeCacheMatrix() as the input argument to cacheSolve()
#             note that the argument to cacheSolve() is a different object
#             than the argument to the first call of cacheSolve()
cacheSolve(makeCacheMatrix(matrix(c(-1, -2, 1, 1), 2,2)))

# call cacheSolve(a) a second time to trigger the "getting cached inverse" message
cacheSolve(a)

# try a non-invertible matrix
b <- makeCacheMatrix(matrix(c(0,0,0,0),2,2))
cacheSolve(b)

# illustrate getting the memory locations
a <- makeCacheMatrix(matrix(c(-1, -2, 1, 1), 2,2))
tracemem(a)
tracemem(matrix(c(-1, -2, 1, 1), 2,2))

# approach 2: use makeCacheMatrix() as the input argument to cacheSolve()
#             note that the argument to cacheSolve() is a different object
#             than the argument to the first call of cacheSolve()
cacheSolve(makeCacheMatrix(matrix(c(-1, -2, 1, 1), 2,2)))

# illustrate getting the memory locations
a <- makeCacheMatrix(matrix(c(-1, -2, 1, 1), 2,2))
tracemem(a)
tracemem(matrix(c(-1, -2, 1, 1), 2,2))


