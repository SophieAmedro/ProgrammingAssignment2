## makeCacheMatrix and cacheSolve is a pair of functions to compute and  
## cache the inverse of a matrix.


## makeCacheMatrix creates a special matrix object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
       m <- NULL
       # to update matrix value and forget cache
       set <- function(y){
              x <<- y
              m <<- NULL
       }
       # to retrieve matrix value
       get <- function() x
       # to put inverse matrix 
       setInverse <- function(inverse) m <<- inverse
       # to retrieve cached value of inverse matrix
       getInverse <- function() m
       # list of accessor and mutator functions
       list(set = set, get = get, 
            setInverse = setInverse, getInverse = getInverse)

}


## cacheSolve returns a matrix that is the inverse of 'x'
## This function computes the inverse of the special matrix returned
## by makeCacheMatrix above. If the inverse has already been calculated,
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
       # retrieve the cache from 'x'
       m <- x$getInverse()
       # If not empty, return directly the cache with message
       if (!is.null(m)){
              message("getting cached data")
              return (m)
       }
       # else compute the inverse matrix, cache it and return it
       data <- x$get()
       m <- solve(data)
       x$setInverse(m)
       m
}
