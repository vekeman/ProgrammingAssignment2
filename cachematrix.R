## Function that computes the inverse of a matrices and caches it for future use

## Creates a specialized list containing functions that can be used to cache matrix inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
                        x <<- y
                        i <<- NULL
                       }
    get <- function() x
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i
    list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## Will check to see if previous run created i (the inverse of x). If so will return cached value. Else will compute and store

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
                    message("getting cached data")
                    return(i)
                  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}




