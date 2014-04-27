####################################################################################
# cacheMatrix and cacheSolve test script
# --------------------------------------
# Author: James B Taylor
# Date : 26/04/2014
#
####################################################################################


#clear the environment and load the code
rm(list=ls())
source("cachematrixv2.R")

#Create some matrices and their inverse
#Inverse calcuated using: http://www.bluebit.gr/matrix-calculator/calculate.aspx
##square matrices non-singular (i.e. there is an inverse)

a<-rbind(c(1,2),c(3,4))
a_inv<-rbind(c(-2,1),c(1.5,-.5))

b<-rbind(c(3,4,5,8),c(2,3,1,1),c(10,5,2,8),c(4,7,6,5))
b_inv<-rbind(c(-0.293,-0.572,0.192,0.275),c(0.183,0.926,-0.135,-0.262),c(-0.288,-1.026,0.070,0.555),c(0.323,0.393,-0.048,-0.319))

#singular matrix - determinate 0 - no defined inverse
c<-matrix(1:100, nrow=10, ncol=10)

#random 10 by 10 square matrix (singular?? unlikely)
d<-matrix(rnorm(100), 10, 10)
#don't know the inverse so build a unitary matrix for comparison via %*%
d_unitary<-rbind(c(1,0,0,0,0,0,0,0,0,0),
                 c(0,1,0,0,0,0,0,0,0,0),
                 c(0,0,1,0,0,0,0,0,0,0),
                 c(0,0,0,1,0,0,0,0,0,0),
                 c(0,0,0,0,1,0,0,0,0,0),
                 c(0,0,0,0,0,1,0,0,0,0),
                 c(0,0,0,0,0,0,1,0,0,0),
                 c(0,0,0,0,0,0,0,1,0,0),
                 c(0,0,0,0,0,0,0,0,1,0),
                 c(0,0,0,0,0,0,0,0,0,1))

#non-square matrix (no true inverse)
e<-rbind(c(1,2,7),c(3,2,5))

# build required objects

print("Creating matrix objects:")
a_obj <- makeCacheMatrix(a)
b_obj <- makeCacheMatrix(b)
c_obj <- makeCacheMatrix(c)
d_obj <- makeCacheMatrix(d)
e_obj <- makeCacheMatrix(e)

# Test 1
print("1. Testing matrix a is correct:")

#if(identical(a_obj$getInv(),a_inv)){print("Pass")}else{print("Fail")}
if(identical(round(cacheSolve(a_obj),3),a_inv)){print("Pass")}else{print("Fail")}


# Test 2
print("2. Testing matrix b is correct:")
if(identical(round(cacheSolve(b_obj),3),b_inv)){print("Pass")}else{print("Fail")}

# Test 3
print("3. Testing matrix c is identified as singular matrix")
if(is.null(cacheSolve(c_obj))){print("Pass")}else{print("Fail")}

# Test 4
print("4. Testing matrix d %*% d_inv forms a unitary matrix")

if(det(d)==0 & identical(cacheSolve(d_obj),NULL))
{
  print("Pass - singular, no inverse")
  
} else if (identical(round(d_obj$get()%*%cacheSolve(d_obj),2),d_unitary))
{
  print("Pass - correct inverse")
} else
{
  print("Fail")
}

# Test 5
print("5. Testing matrix e is identified as not square")
if(identical(cacheSolve(e_obj),NULL)){print("Pass")}else{print("Fail")}

print("Tests complete.")
## end of tests