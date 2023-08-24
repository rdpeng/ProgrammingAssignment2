### Description of cachematrix.R
# as request, user could use function cacheSolve to get a inverse value from a given materix.
# cacheSolve check whether the given matrix already has value stored, if yes, give result without caculeted by solve()
# if no, cacheSolve will caculated the inverse result and store the give matrix and related inverse value, so that next time avoid caculeted by solve()
# function makeCacheMatrix need be instantiated before useing, just like: fun<-makeCacheMatrix()
# using steps as below:
# 1:source("cachematrix.R")
# 2:fun<-makeCacheMatrix()
# 3:cacheSolve(fun,matrix(4:7,2))
# user could change the matrix input of step 3, makeCacheMatrix also provide function for verify whether the given function is Singular matrix. 
