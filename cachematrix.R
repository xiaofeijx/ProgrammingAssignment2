## my functions are the copy of the functions provided by the assignment intruction
## with minor changes.

## I have learned a lot from the discussion forum, J.R. Jasperson's post made understand
## how these functions should be used. 

## makeCacheMatrix function takes a matrix as its argument and make it persist in it's 
## environment. 
# The first function, makeCacheMatrix creates a special "vector", which is really a list containing a function to 
# 1.set the value of the matrix
# 2.get the value of the matrix
# 3.set the value of the inverse
# 4.get the value of the inverse
#  the returned value(list) should be used as an argument of the cacheSolve


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y # <<- looking object in the parent environment first!!
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## cacheSolve takes a list returned by the makeCacheMatrix
##it first checks to see if the inverse has already been calculated. 
# If so, it gets the inverse from the cache and skips the computation.
# Otherwise, it calculates the inverse of the data and sets the value of
# the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
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

#first we create a list
my.matrix <-makeCacheMatrix(matrix(c(1,2,3,4),ncol=2))

#the we use cachesolve to calculate the inverse
#if you run it for the first time, it will calculate the inverse
cacheSolve(my.matrix)
#if you run it for the second time, it will get the inverse and give you a message.
cacheSolve(my.matrix)