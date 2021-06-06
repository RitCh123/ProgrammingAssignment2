#Similar to makeVector() function
#makeCacheMatrix function
#takes matrix x for input, assigns variable m to the cache 
#functions to get value of matrix, set value of inverse matrix in cache
#and get the value of inverse of matrix in cache
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  get <- function() x
  setSolve <- function(matrixObject) m <<- matrixObject
  getSolve <- function() m
  list(get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}
#cacheSolve function
#takes parameter matrix x
#gets inverse from cache, checks for existence
#if so, get inverse from cache, if not
#solve inverse and store in cache

cacheSolve <- function(x, ...) {
  m <- x$getSolve()
  
  if(!is.null(m)) {
    message("Cache data from variable")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setSolve(m)
  m
}
