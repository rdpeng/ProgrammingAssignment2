#In order to run this file in Rstudio, open it and press the source button

source('cachematrix.R')
#case1: 
# 4  8  9                   -102/505 (-0.201)    31/505 (0.061)     58/505 (0.114)
# 7  2  24  should return   89/505   (0.1762)    -32/505 (-0.063)   -11/505 (-0.0217)    
# 12 13 3                   67/1515  (0.044)      44/1515 (0.029)   -16/505 (-0.031)    

print("CASE 1:")
#create the cached matrix
cached <- makeCacheMatrix(matrix(c(4,7,12,8,2,13,9,24,3), nrow = 3, ncol = 3))

#make sure the matrix was cached
print("The original cached Matrix")
print(cached$get())

#Now lat's solve (get the Inverse Matrix)
cacheSolve(cached)

#Now lets print the solved Matrix from the cached object
print("The cached Inversed Matrix")
print(cached$getinverse())

#case2: 
# 5  4  10 8                   -65/389 (-0.167)    424/1167 (0.363)    2/389  (0.005)    -38/389 (-0.097)
# 6  3   6 6  should return    -68/389 (-0.174)   -113/1167 (-0.096)   38/389 (0.097)     56/389 (0.143) 
# 9 13  15 7                    97/389 (0.249)    -142/1167 (-0.121)    3/389 (0.007)    -57/389 (-0.146) 
# 4  5   6 9                    2/389  (0.005)     -31/1167 (-0.026)   -24/389 (-0.061)   67/389 (0.172)


print("CASE 2:")
cached2 <- makeCacheMatrix(matrix(c(5,6,9,4,4,3,13,5,10,6,15,6,8,6,7,9), nrow = 4, ncol = 4))

#make sure the matrix was cached
print("The original cached2 Matrix")
print(cached2$get())

#Now lat's solve (get the Inverse Matrix)
cacheSolve(cached2)

#Now lets print the solved Matrix from the cached object
print("The cached2 Inversed Matrix")
print(cached2$getinverse())






