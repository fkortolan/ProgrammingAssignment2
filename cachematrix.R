## The main goal of this assignment is to write two functions (makeCacheMatrix ()
## and cacheSolve()) that cache the inverse of a matrix

##### Function makeCacheMatrix()
## Arguments: x (a matrix) 
## Creates a special "matrix" object with x (the matrix) and i (the inverse) 
## that contains four functions:
## set() - assigns the matrix to x and NULL to i in the parent environment
## get() - gets the matrix x from the parent environment
## setinverse() - assigns the inverse calculated in cacheSolve to i
## getinverse() - retrieves the value of the inverse i
## In the end, it's all assigned to a list.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

##### Function cacheSolve()
## Arguments: x (a matrix) and ... (extra arguments)
## Calculates the inverse of the matrix. If it has already been 
## calculated, it'll retrieve the result of the operation.
## It uses the $getinverse function to get the inverse if has 
## already been calculated. If it hasn't, it uses the $get()
## function to get the matrix, solve for the inverse and
## use $setinverse to cache the inverse.
## It returns the inverse of the matrix.
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

#### Example of use:
## Creates a 2x2 matrix called mat to be used as an example
mat <- matrix(c(1,2,3,4),nrow=2,ncol=2) 

## Creates the special "matrix" object than can cache its inverse
cache_matrix <- makeCacheMatrix(mat)  

## Using $getinverse() returns null, since the inverse hasn't been calculated
cache_matrix$getinverse()

## Computes the inverse and returns it. If the inverse had already been
## calculated, it would just retrieve it from the cache.
cacheSolve(cache_matrix)




