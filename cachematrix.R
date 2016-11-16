## makeCacheMatrix function creates a special "matrix" object
## that can cache its inverse

## The function takes x as a square invertible matrix
## and returns a list containing functions to:
    ## 1.  set the value of the matrix
    ## 2.  get the value of the matrix
    ## 3.  set the value of the inverse
    ## 4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
set <- function(y) {
        x <<- y
        inv <<- NULL
}
get <- function() x
setinv <- function(inverse) inv <<- inverse
getinv <- function() inv
list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## CacheSolve function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already
## been calculated (and the matrix has not changed). then the cacheSolve
## retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
        ## x: output of makeCacheMatrix()
        ## return: inverse of original matrix input to makeCacheMatrix
         inv <- x$getinv()  ## get inv
         if(!is.null(inv)) {   ## if inv has already been calculated
              message("getting cached data")
              return(inv)
         }
         data <- x$get()     ## else calculate inverse
         inv <- solve(data, ...)
         x$setinv(inv)
         inv
}
