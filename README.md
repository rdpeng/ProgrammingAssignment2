## Here is how to use the .R functions for caching an inverse of a matrix

# First, set your working directory and source the file

# Then create a square matrix

# Finally, use the two functions build in the assignment

# Here is an example of how to use the functions

mat = matrix(c(1,2,3,4),nrow=2,ncol=2)

# Let's visualize the list

l <- makeCacheMatrix(mat)

summary(l)

# Let's get the values

l$getMat()

# Let's see the inverse

cacheSolve(l)

# If we run it again, with the same matrix, we get the cached value of the inverse

cacheSolve(l)