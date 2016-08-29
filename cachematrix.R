#Programming Assignment #2

#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  mInv <- NULL
  set <- function(y) {
    x <<- y
    mInv <<- NULL
  }
  get <- function() x
  setMInverse <- function(inverse) mInv <<- inverse
  getMInverse <- function() mInv
  list(set = set, get = get,
       setMInverse = setMInverse,
       getMInverse = getMInverse)
}

#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  mInv <- x$getMInverse()
  if(!is.null(mInv)) {
    message("getting cached data")
    return(mInv)
  }
  message("computing inverse...")
  data <- x$get()
  mInv <- solve(data)
  x$setMInverse(mInv)
  mInv
}

#Unit Test
#> cachedMatrix <- makeCacheMatrix(matrix(c(1,4,3,6),2,2))
#> cacheSolve(cachedMatrix)
#computing inverse...
#[,1]       [,2]
#[1,] -1.0000000  0.5000000
#[2,]  0.6666667 -0.1666667
#> cacheSolve(cachedMatrix)
#getting cached data
#[,1]       [,2]
#[1,] -1.0000000  0.5000000
#[2,]  0.6666667 -0.1666667
#> cachedMatrix <- makeCacheMatrix(matrix(c(1,4,3,3),2,2))
#> cacheSolve(cachedMatrix)
#computing inverse...
#[,1]       [,2]
#[1,] -0.3333333  0.3333333
#[2,]  0.4444444 -0.1111111


