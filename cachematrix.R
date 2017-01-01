## This file contains 3 functions:
##
## 1) makeCacheMatrix
## 2) cacheSolve
## 3) CheckSingular
##
## "makeCacheMatrix" needs as input a (square and invertible) matrix.
##  This function creates a special "matrix" object that can cache its inverse.
##
## "cacheSolve": This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.
##
## "CheckSingular" checks, if the process of inverting has worked correctly
##
## Two matrices with testdata (mat1, mat2) are provided.


# clear workspace/ optional / uncomment via ctrl + shift + C
# rm(list = ls())
# ls()


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


cacheSolve<- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}



# generate testdata
mat1 <- matrix(rnorm(10000),100,100)
mat2 <- matrix(rnorm(625), 25, 25)


# test function call
a <- makeCacheMatrix(mat1)
matinv <- cacheSolve(a)

# second call should retrieve chached data.
matinv <- cacheSolve(a)
a$getinv()
a$get()




# TEST
# check, if inverted matrix is calculated correctly
checkSingular <- function(mat = matrix()) {
  matinv <- cacheSolve(makeCacheMatrix(mat))
  # multiply matrix and it's inverse
  singular <- mat %*% matinv
  s1 <- 0
  s2 <- 0
  for (i in 1:nrow(singular)) {
    for (j in 1:ncol(singular)) {
      if (i == j) {s1 <- s1 + singular[i,j]
      } else {
        s2 <- s2 + singular[i,j]
      }
    }
  }
  cat("dimensionality  of matrix: ", nrow(singular), " rows and ", ncol(singular), " columns", "\n")
  cat("sum of diagonal elements = ", s1, "\n")
  cat("sum of non-diagonal elements = ", round(s2, digits = 0))
}


checkSingular(mat1)
checkSingular(mat2)