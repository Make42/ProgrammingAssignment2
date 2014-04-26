
## Short comment describing the following function:

# The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
#     1. set the value of the matrix
#     2. get the value of the matrix
#     3. set the value of the inverse of the matrix
#     4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  invMatr <- NULL
  set <- function(y) {
    x <<- y
    invMatr <<- NULL
  }
  get <- function() x
  setinvMatr <- function(compinvMatr) invMatr <<- compinvMatr
  getinvMatr <- function() invMatr
  list(set = set, get = get,
       setinvMatr = setinvMatr,
       getinvMatr = getinvMatr)
}


## Short comment describing the following function:

# The following function calculates the inverse of the special "matrix" created with the above function. 
# However, it first checks to see if the inverse has already been calculated.
# If so, it gets the inverse from the cache and skips the computation.
# Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinvMatr function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  invMatr <- x$getinvMatr()
  if(!is.null(invMatr)) {
    message("getting cached data")
    return(invMatr)
  }
  data <- x$get()
  invMatr <- solve(data, ...)
  x$setinvMatr(invMatr)
  invMatr
  
}
