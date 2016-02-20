# The first function, makeVector creates a special "vector", which is really a list containing a function to
# 
# set the value of the vector
# get the value of the vector
# set the value of the Inverse
# get the value of the Inverse

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

#the following function calculates the inverse of the special "vector"
#created with the above function. However, it first checks to see if the inverse has already been calculated. 
#If so, it gets the Inverse from the cache and skips the computation. Otherwise, it calculates the Inverse of the data and sets the value of the Inverse in the cache via the setinverse function.

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
