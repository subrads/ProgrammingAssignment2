# *------------------------------------------------------------------
# | PROGRAM NAME:  cachematrix                                      | 
# | DATE: 05/24/2015                                                |
# | CREATED BY:  subrads                                            |  
# | PROJECT FILE:  cachematrix.R                                    |  
# *------------------------------------------------------------------

# *------------------------------------------------------------------
# |PURPOSE:                                                         |
# | This program contains Two functions. One to Make the cache of a  |   
# | matrixand other to compute and cache the matix.                 |
# | makeCacheMatrix takes a parametr of type Matrix as input        |
# | and returns a special list which contains method to get and     |
# | set inverse.                                                    |
# | CacheSolve function takes the special matrix and looks          |
# | up the inverse from cache. if inverse availbale returns the     | 
# | value, if not available ,inverse in computerd, set in cache     | 
# | and the value is returned                                       |
# *------------------------------------------------------------------

##  makeCacheMatrix takes a parametr of type Matrix as input 
##  setInverse sets the inverse of matrix
##  getInverse gets the inverse of matrix
##  returns a list of functions to operate on

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL

  set <- function(y) {
    x <<- y
    m <<- NULL
  }

  ## Return vlaue of matrix x
  get <- function(){
    x
  } 
  
  ## set the inverse of the matrix in  varaible
  setinverse <- function(matrixInverse) {
    m <<- matrixInverse
  }
  
  ## get the inverse of the matrix from stored varaible
  getinverse <- function() {
    m
  }
  
  # Return a list of functions to operate on the special matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


##  cacheSolve takes a special matrix list a as a paramter
##  and calls getInverse method on the matrix passed 
##  if the getinverse method returns a not null value
##  then the inverse of the metod has been cached and simply returns
##  the cached value. if teh value returned is null then 
##  the inverse is calcuated using sovle method and stored in the cache
##  using setInverse method so that values can be returned from cache.
## If a non-invertible matrix is passed, then the Solve method will throw 
## a Exception.
## Function also takes ... as second paramater to pass any custom paramater
## to sovle function.


cacheSolve <- function(x, ...) {
  
  ## get the inverse value from Cache
  m <- x$getinverse()
  
  ## Check if the value is not null. if not null return the data from cached data
  if(!is.null(m)) {
    message("Getting  cached Inverse data")
    return(m)
  }
  
  # get the value of the matrix
  data <- x$get()
  
  # compute inverse of the matrix
  m <- solve(data, ...)
  
  # set the inverse of the matrix in cache
  x$setinverse(m)
  
  return (m)
  
}
