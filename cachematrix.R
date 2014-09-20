makeCacheMatrix<-function(range,nrow)##enter range of values and no of rows
{
mat<-matrix(range,nrow,byrow=TRUE)
matrix<-mat
print("matrix")
print(matrix)##printing original matrix
inversematrix<-solve(matrix)
#return(matrix)
print("Inverse of Matrix")
return(inversematrix)##printing inverse of matrix
}
makeCacheMatrix(1:4,2)

