## Return a matrix's inverse (if invertable)
## Caches the inverse for faster retrieval

## Usage: cacheSolve(makeCacheMatrix(x)) - where 'x' is an invertable matrix



## Create a special object that stores a matrix and cache's its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL

  # set the value of the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }

  # get the value of the matrix
  get <- function() x

  # set the inverse of the matrix
  setinverse <- function(solve) m <<- solve

  # get the inverse of the matrix
  getinverse <- function() m

  # return list
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}



## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
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
