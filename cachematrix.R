## R Programming assignment 2 - calculating the inverse of a matrix

## This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

## The following code can be used to illustrate how these functions work
## Create a matrix using the makeCacheMatrix function and label it 'a'
#  a <- makeCacheMatrix(matrix(1:4,2,2))
## The following command returns the matrix
#  a$get()
## The following command returns nothing since the inverse hasn't been cached:
#  a$getinv()
## The matrix can be set using a$set(matrix())
## The inverse of the matrix a can be stored using the cacheSolve function
#  cacheSolve(a)
##  'a$getinv()' will now return the cached inverse
#  a$getinv()
## If the matrix is changed then the cacheSolve function needs to be re-run. I.e.
#  a$set(matrix(c(2,0,1,6),nrow = 2, ncol = 2))
#  a$get()
## The following code will return null
#  a$getinv()
## Re-run cacheSolve
#  cacheSolve(a)
## 'a$getinv()' will now return the cached inverse
#  a$getinv()

