# Put comments here that give an overall description of what your
# functions do

# Write a short comment describing this function
# Create a wrapper object around a matrix to allow caching the inverse.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


# Write a short comment describing this function
# Return a matrix that is the inverse of 'x', only calculating the inverse once
# and subsequently using the cached version.
cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}


# Test
x <- cbind(c(1,2, 3),c(4,1,5),c(5,3,4))
X <- makeCacheMatrix(x)
print(cacheSolve(X))
print(cacheSolve(X))
