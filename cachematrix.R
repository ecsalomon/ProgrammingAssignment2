## These functions create an object that con be used to solve the inverse of a
## a matrix. Send a matrix to makeCacheMatrix. Solve the inverse by calling 
## cacheSolve.

## This function creates an item containing the functions to store the matrix
## and its inverse, change which matrix is used, and solve the inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL                               # initialize with no inverse
  set <- function(y) {
    x <<- y                               # store the uninverted matrix
    m <<- NULL                            # clear the inverse
  }
  get <- function() x                     # return the matrix
  setsolve <- function(solve) m <<- solve # solve the inverse
  getsolve <- function() m                # return the inverse
  list(set = set, get = get,              # build a list of the four functions
       setsolve = setsolve,
       getsolve = getsolve)
}


## This function checks to see if the inverse has already been cached. If so,
## it returns the cached inverse. If not, it calculates the inverse.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()          # try to pull in cached inverse
  if(!is.null(m)) {          # was there a cached inverse?
    message("getting cached data")
    return(m)                # if so, return that cached matrix / ends function
  }
  data <- x$get()            # if no cached inverse, get the original matrix
  m <- solve(data)           # solve the inverse
  x$setsolve(m)              # cache the inverse
  m                          # return the inverse
}
