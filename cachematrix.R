# The first function, is a child function that will be used by the parent function to complete all tasks

makeCacheMatrix<-function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solved) m <<- solved
  getinverse <- function() m
  list(set = set, get = get,setinverse = setinverse,
       getinverse = getinverse)
}

# the following code first looks to see if in the list provided by makeCacheMatrix the matrix has already been solved(m isn't empty)
# ifso the function returns that value, if not the function solves the matrix and returns it
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
