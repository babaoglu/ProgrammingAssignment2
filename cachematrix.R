## Matrix inversion is usually a costly computation and 
## there may be some benefit to caching the inverse of 
## a matrix rather than compute it repeatedly.

## This function creates a special "matrix" object that 
## can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x   <<- y
                inv <<- NULL
        }
        get     <- function() x
        set_inv <- function(val) inv <<- val
        get_inv <- function() inv
        list(set = set, get = get,
             set_inv = set_inv,
             get_inv = get_inv)
}


## This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. If the 
## inverse has already been calculated (and the matrix 
## has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$get_inv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv  <- solve(data, ...)
        x$set_inv(inv)
        inv
}

## How to run : 
## 1. Go to the folder having the code 
## > setwd("ProgrammingAssignment2")
## 2. Load the functions in source file
## > source("cachematrix.R")
## 3. Check the following example:
##      a) Create an invertiable matrix
##         and store it as "mat"
## > mat <- matrix(c(1,0,-1,1),nrow=2,ncol=2,byrow=T)
##      b) Check the content
## > mat
##      [,1] [,2]
## [1,]    1    0
## [2,]   -1    1
##      c) Check the inverse
## > solve(mat)
##     [,1] [,2]
## [1,]    1    0
## [2,]    1    1
##      d) Run makeCacheMatrix and 
##         check the data
## > x <- makeCacheMatrix(mat)
## > x$get()
##      [,1] [,2]
## [1,]    1    0
## [2,]   -1    1
##      e) Get the inverse (should
##         be NULL at this point)
## > x$get_inv()
## NULL
##      f) Run cacheSolve (this will
##         initiate cached inverse "inv")
## > y <- cacheSolve(x)
##      g) Get the inverse now
## > x$get_inv()
##      [,1] [,2]
## [1,]    1    0
## [2,]    1    1
##      g) Run cacheSolve again (this
##         time cached value is used)
## > y <- cacheSolve(x)
## getting cached data
##
