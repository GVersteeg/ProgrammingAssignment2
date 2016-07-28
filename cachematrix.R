## This R-file contains two functions makeCacheMatrix and cacheSolve. 
## Both functions work in together to enable the inverse of a matrix
## to be cached rather than recaclulated each time it is requested.
## Please note: this functionm does NOT check for not invertible matrices
## like the ones with a determinant = 0 or non-square matrices !!
## 

## 
## makeCacheMatrix is a function that creates a special "matrix"
## object that is able to cache its inverse. Function returns a list
## containing 4 functions get(), set(), getinverse() and setinverse().
## myMatrix <- makeCacheMatrix(matrix(1:8, 3, 3)) results in an object
## myMatrix that contains the 4 functions and 2 data objects: 
## m (the source matrix passed as an argument) and i (the inversed matrix).
##
## myMatrix, in essence, contains a complete copy of the environment
## for makeCacheMatrix(), including any objects that are defined within
## makeCacheMatrix() at design time. This is because the pointers to the
## objects are maintained when an R function retruns an object that contains
## functions, including the original argument used calling the function.
## 

## 
##################### 
## 

makeCacheMatrix <- function(m = matrix()) {
        i <- NULL             ## inits inverse 
        set <- function(y) {  ## defines set function
                m <<- y       ## source matrix to parent frame
                i <<- NULL    ## inverse to parent frame (reset i for new m)
        }
        get <- function() m   ## defines get function
        setinverse <- function(solve) i <<- solve  ## defines setinverse fun
        getinverse <- function() i      ## defines getinverse fun
        list(set = set, get = get,      ## return named list of functions
             setinverse = setinverse,
             getinverse = getinverse)
}

## cacheSolve computes the inverse of the special "matrix" object
## that is returned by the beforementioned makeCacheMatrix
## (i.e. NOT a regular square matrix!).
## If the inverse matrix is already previously calculated while the source
## matrix remained unchanged it will return the cached inverse. If the source
## matrix is changed or the inverse has not yet been calculated, the 
## function will (re)calculate the inverse from the source matrix.
## cacheSolve works because functions that return list objects also allow
## access to any other object (m, 1) defined in the environment of the orginal
## function, because the pointers to their environment are maintained.
## 
## 

cacheSolve <- function(m, ...) {    ## caches inverse of special 'matrix'
        i <- m$getinverse()         ## try to retrieve cached inverse
        if(!is.null(i)) {           ## if inverse present return cached inverse 
                message("getting cached data")
                return(i)
        }
        data <- m$get()             ## if inverse does not exist
        i <- solve(data, ...)       ## recalculate inverse matrix and
        m$setinverse(i)             ## set the cached inverse through setter
        i                           ## return inverse matrix
}
