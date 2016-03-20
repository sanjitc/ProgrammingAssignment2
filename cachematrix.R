## The following functions work together to create a square invertible matrix
## and make the inverse of the matrix available in the cache environment
##
## Sample run-time example included results

## > source("cachematrix.R")    load R program
## > a <- makeCacheMatrix()     create functions
## > a$set(matrix(1:4, 2, 2))   create matrix in working environment
## > cacheSolve(a)              1st run returns inverted matrix
##                              from working environment
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
##
## > cacheSolve(a)              2nd and subsequent runs
##                              returns inverted matrix from cache
## retriving cached data          
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

## The makeCacheMatrix() creates and returns a list of functions used by 
## cacheSolve() to get or set the inverted matrix in cache

makeCacheMatrix <- function(x = matrix()) {

    cache <- NULL

    set <- function(y) {
      x <<- y
      cache <<- NULL
    }
    
    get <- function() x

    setMatrix <- function(inverse) cache <<- inverse

    getInverse <- function() cache
    
    list(set = set, get = get,
         setMatrix = setMatrix,
         getInverse = getInverse)
}


## cacheSolve() calcluates the inverse of the matrix that created by
## makeCacheMatrix(). If the inverted matrix does not exist in cache,
## it creats in the working environment and invertes value.
cacheSolve <- function(x, ...) {

    cache <- x$getInverse()

    if (!is.null(cache)) {
      message("retriving cached data")
      
      return(cache)
    }
    
    matrix <- x$get()
    
    tryCatch( {
      cache <- solve(matrix, ...)
    },
    error = function(e) {
      message("Error:")
      message(e)
      
      return(NA)
    },
    warning = function(e) {
      message("Warning:")
      message(e)
      
      return(NA)
    },
    finally = {
      x$setMatrix(cache)
    } )
    
    return (cache)
}
