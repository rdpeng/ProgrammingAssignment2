test.cacheMatrix <- function() {
  m <- rbind(c(1, -1/4), c(-1/4, 1))
  m.inv <- solve(m)
  m.cacheMatrix <- makeCacheMatrix(m)
  
  ## Test case 1: first time computation
  m.cacheInv1 <- cacheSolve(m.cacheMatrix)
  checkEquals(m.cacheInv1, m.inv)
  
  ## Test case 2: fetch cached result
  m.cacheInv2 <- cacheSolve(m.cacheMatrix)
  checkEquals(m.cacheInv2, m.cacheMatrix$getInverse())
}
