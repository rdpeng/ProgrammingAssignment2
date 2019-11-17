makeCacheMatrix <- function(x = matrix()) {
      
      inv <-NULL
      setMatrx<-function(mat){
            m<<-mat 
      }
      getMatrix<-function() m
      
      cacheInverse<-function(temp){
            inv<<-temp
      }
      getInverse<-function(){
            if (nrow(m) != ncol(m)) {print('matrix is not square')}
            inv
      }
      
      list(setMatrix=setMatrix,getMatrix=getMatrix,cacheInverse=cacheInverse,getInverse=getInverse)
      
}


cacheSolve <- function(x) {
      n<-s$getInverse()
      if (!is.null(n)){
            message("getting cached data")
            return(n)
      }
      n<-solve(s$getMatrix())
      s$cacheInverse(n)
      n
}
