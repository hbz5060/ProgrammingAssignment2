## These two functions below can cache the inverse of a matrix

## This one can cache the inverse of a matrix object

makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(a){
   x <<- a
   inver <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inver <<- inverse
  getinverse <- function() inver
  list(set = set,
       get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}


## This one can compute the inverse of the matrix returned by the above function. If 
## the inverse already exists, then this function will retrieve the existed inverse.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inver <- x$getinverse()
  if (!is.null(inver)) {
    message("getting cached data")
    return(inver)
  }
  mat <- x$get()
  inver <- solve(mat, ...)
  x$setinverse(inver)
  inver
}

