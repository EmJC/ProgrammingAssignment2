##create a special object that stores a matrix and caches it's inverse

##create a special matrix containing a function to set/get value of matrix, set/get value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


##calculates and returns the inverse of the matrix created in 'makeCacheMatrix'

cacheSolve <- function(x, ...) {
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setmatrix(m)
  m
}

##creates matrix 'amatrix' for testing purposes

amatrix<-matrix(c(1,2,4,6,8,10,12,14,16),nrow=3, ncol=3)
