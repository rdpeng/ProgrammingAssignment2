###======== clear space =============
rm(list = ls())				

###======= import function ==========
source('makeCacheMatrix.R')
source('cacheSolve.R')

Cachefun=makeCacheMatrix() 		

###===== generate matrix =============

dim=2000;  				
mat=matrix(dim*rnorm(dim*dim), dim)


###==== 1st calculation and cache ====


system.time(Invmat<-cacheSolve(mat))

###==== call cacheSolve again and ====
###==== compare execute time =========

system.time(Invmat2<-cacheSolve(mat))

###======== compare results ==========
diff=Invmat2-Invmat
str(diff)