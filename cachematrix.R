## The first function makeCacheMatrix creates a matrix, and defines functions to inverse the matrix
## The second function inverses the matrix if it is not already cached

## This function creates a square matrix and caches the function in the parent environment

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


## This function inverses the matrix passed in its argument using the cached function defined in the
## first function

cacheSolve <- function(x=matrix(), ...) {
        
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix, ...) ## If not cached, return a matrix that is the inverse of 'x'
  x$setmatrix(m)
  m
}
