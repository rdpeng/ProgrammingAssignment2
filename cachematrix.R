## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
{
A <- matrix(c(3,2,1, 1,2,3, 1,2,1), nrow=3); A
At <- cbind( A, diag(nrow(A))); At
At[1, ] <- At[1, ] / At[1,1]
At[2, ] <- At[2, ] - At[2,1]*At[1,]
At[3, ] <- At[3, ] - At[3,1]*At[1,]
At
At[2, ] <- At[2, ] / At[2,2]
At[3, ] <- At[3, ] - At[3,2]*At[2,]
At
At[3, ] <- At[3, ] / At[3,3]
At[2, ] <- At[2, ] - At[2,3]*At[3,]
At[1, ] <- At[1, ] - At[1,3]*At[3,]
At[1, ] <- At[1, ] - At[1,2]*At[2,]
I <-At
B <- I[,-(1:3)]
p <-A %*% B
}


}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        X <- matrix(c(3,2,1, 1,2,3, 1,2,1), nrow=3); X
B <- cbind( X, diag(nrow(X))); B
B[1, ] <- B[1, ] / B[1,1]
B[2, ] <- B[2, ] - B[2,1]*B[1,]
B[3, ] <- B[3, ] - B[3,1]*B[1,]
B
B[2, ] <- B[2, ] / B[2,2]
B[3, ] <- B[3, ] - B[3,2]*B[2,]
B
B[3, ] <- B[3, ] / B[3,3]
B[2, ] <- B[2, ] - B[2,3]*B[3,]
B[1, ] <- B[1, ] - B[1,3]*B[3,]
B[1, ] <- B[1, ] - B[1,2]*B[2,]
J <- B
K <- J[, -(1:3)]
Q <- A %*% K 

}
