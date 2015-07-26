## makeCacheMatrix - gets/sets matrix and its inverse
## cacheSolve - retrieves the inverse, either from the cache or
## calculates if necessary

## Usage e.g.:
# > A <- matrix(1:4,2,2)
# > m1 <- makeCacheMatrix(A)
# > m2 <- cacheSolve(m1)
# calculating inverse
# > m2
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > m3 <- cacheSolve(m1)
# getting cached data
# > B <- matrix(2:5,2,2)
# > m1$set(B)
# > m3 <- cacheSolve(m1)
# calculating inverse
# > m3
# [,1] [,2]
# [1,] -2.5    2
# [2,]  1.5   -1

# The first function, `makeCacheMatrix` creates a list containing
# functions as follows:
# 1. set:  sets the value of the matrix to be inverted
# 2. get: gets the value of the matrix to be inverted
# 3. setinv:  sets the value of the inverse
# 4. getinv:  gets the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  
  list(set=set, get=get,
       setinv=setinv,
       getinv=getinv)

}

# cacheSolve calculates the inverse of the matrix created with
# the above function. However, it first checks to see if the
# inverse has already been calculated. If so, it `get`s the 
# inverse from the cache and skips the computation. Otherwise,
# it calculates the inverse of the matrix and sets the value of
# the inverse in the cache via the `setinv` function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  message("calculating inverse")
  mat <- x$get()
  i <- solve(mat,...)
  x$setinv(i)
  i
}
