## Functions for creating special 'matrix' objects with caching capabilities
## and calculating inverse of such matrices while using cached results if
## the calculation has already been done

## Creates special 'matrix' object which is a list of four functions:
## set - sets a value of a special 'matrix' object 
## get - returns special 'matrix' value, if it has been set, or NULL otherwhise
## setinverse - caches the inverse value of the special 'matrix' object
## getinverse - returnes cached value of the special 'matrix' object, if it has been set, or NULL otherwhise

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


## Calculates inverse of the special 'matrix' object. First looks for a cached
## value of this operation, and uses that value, or does the calculation if cached 
## value is not found.

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
