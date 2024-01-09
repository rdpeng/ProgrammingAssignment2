makeCacheMatrix <- function(x = numeric()) {
  m <- NULL 
  
  set <- function(y) {
    x <<- y # 在函数内修改外部函数的变量
    m <<- NULL 
  }
  
  get <- function() x # 设置x值
  
  setinverse <- function(solve) m <<- solve # 设置m值
  getinverse <- function() m # 返回m值
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x) {
  m <- x$getinverse()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data) 
  x$setinverse(m)
  m

}
