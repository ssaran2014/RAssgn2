## Put comments here that give an overall description of what your
## functions do

##  This function sets the inverse of the matrix using solve and gets it

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
      set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Calculates the inverse with the above function. However, if the inverse 
## has already been calculated, then it gets the inverse from the cache and 
## skips the computation.

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
