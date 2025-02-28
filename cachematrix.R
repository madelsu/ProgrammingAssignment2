## # These functions work together to calculate and cache the inverse of a matrix. 

###creates a matrix object that stores the matrix and its cached inverse, by providing functions to set/get both.
### The inverse is initially NULL, only calculated when needed.


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  
  set <- function(y) {
    x <<- y
    inv <<- NULL 
  }
  get <- function() x
  
  setInverse <- function(inverse) inv <<- inverse

  getInverse <- function() inv
  
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

###computes the inverse of the matrix created by makeCacheMatrix.
# If the inverse is already cached, it takes it it instead of calculating again
#If not it calculates the inverse using solve(), caches it, and returns the result.


cacheSolve <- function(x, ...) {
  inv <- x$getInverse() 
  
  if(!is.null(inv)) {
    message("Getting cached inverse")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv) 
  inv
}
