## Put comments here that give an overall description of what your
## functions do

## This function creates a "special" matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function (y) {
            x <<- y
            m <<- NULL
      }
      get <- function () x
      setminv <- function (solve) m <<- solve
      getminv <- function () m
      list (set = set, get = get , setminv = setminv, getminv = getminv)
}


## This version computes de inverse of a "special" matrix returned by makeCacheMatrix
## If inverse has already been calculated and matrix is unchanged, retreives inverse from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getminv ()
      if (!is.null (m)) {
            message ("getting cached data")
            return (m)
      }
      data <- x$get ()
      m <- solve (data)
      x$setminv (m)
      m
}

