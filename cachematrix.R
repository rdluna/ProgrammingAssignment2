## These functions are used to get the inverse of a matrix and store
## its value in the cache to avoid recalculating it. If the inverse
## already exists in the cache, the functions read the value from it
## thus avoiding the recalculation of the inverse.

## The makeCacheMatrix function takes as input a matrix, which must
## be a non-singular square matrix (det(m) != 0)

makeCacheMatrix <- function(x = matrix()) {
  ## The dimensions of the matrix are extracted to validate if it's square
  nrows <- dim(x)[1]
  ncols <- dim(x)[2]
  ## If the matrix is not square the function exits with an error
  if(nrows != ncols){
    stop(paste("Input matrix is a non-square matrix (",nrows,"x",ncols,")"))
  }
  ## If the determinant of the matrix is 0 (i.e the matrix is singular)
  ## the function exits with an error.
  ## The execution time for the determinant function det(m) is shorter than
  ## the execution time for the inverse function solve(m).
  if(det(x) == 0){
    stop("Input matrix is a singular matrix")
  }
  i <- NULL
  ## Set the value of the matrix
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  ## Get the value of the matrix
  get <- function() x
  ## Set the inverse of the matrix
  setinv <- function(solve) i <<- solve
  ## Get the inverse of the matrix
  getinv <- function() i
  ## Create the list with the 4 elements
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## The cacheSolve function receives as input a matrix and returns its inverse
## either calculating it or getting it from the cache if it already exists.

cacheSolve <- function(x, ...) {
  ## Check if the input is in the list form expected by this function.
  ## If the input is not in a list form then the function exits with an error.
  if(class(x) != "list"){
    stop("Input is not a list. 
    Assign the result of makeCacheMatrix to another variable first:
      m <- makeCacheMatrix(<your matrix>)
    Then run the function again:
      minv <- cacheSolve(m)")
  }
  ## Get the value of the inverse of the matrix
  i <- x$getinv()
  ## If it's not null then it has already been calculated.
  ## The existing value is returned.
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  ## If it's null then it hasn't been calculated.
  ## The inverse is calculated.
  data <- x$get()
  i <- solve(data, ...)
  ## It's value in the cache is set.
  x$setinv(i)
  ## In either case the function returns a matrix that is the inverse of 'x'
  i
}

## Regarding the execution times for the inverse and determinant functions
## it was tested using the following script:
## > m <- matrix(sample(1:10,10000,replace=TRUE), ncol = 100, nrow = 100)
## > system.time(replicate(10000, solve(m)))
# user  system elapsed 
# 11.23    0.89   12.15 
# > system.time(replicate(10000, det(m)))
# user  system elapsed 
# 2.66    0.00    2.68 
