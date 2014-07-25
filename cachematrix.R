## The first function, MakeCacheMatrix, creates a special "matrix" - which is actually a list containing a function to
## (1) set the value of the matrix, (2) get the value of the matrix, (3) set the value of the inverse (solve), 
## (4) get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}
##  This second function, cacheSolve,calculates the inverse of the special "matrix" created by makeCacheMatrix above
##
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s

}
