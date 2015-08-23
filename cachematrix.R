makeCacheMatrix <- function(x=matrix()) {
      inverse <- function(y){
            y<<-x
            inv_mat <<- solve(y)
      }
      list(mat=mat, inverse=inverse)
}

cacheSolve <- function(x,...){
      show_inverse <- makeCacheMatrix(x)$inverse(makeCacheMatrix(x)$mat)
      print(show_inverse)
}      
