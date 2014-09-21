## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##This function will
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inversematrix
## 4. get the value of the inversematrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  

}


## Write a short comment describing this function
## The following function returns the inverse of a matrix. 
## However, it first checks to see if the inverse has already been 
## computed. If so, it gets the inverse from the cache and skips 
## the computation. Otherwise, it will do the inverse of the data and
## sets the value of the inverse in the cache via the setinverse 
## function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
