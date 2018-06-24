## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


makeCacheMatrix <- function(x = matrix()) # this function stores the matrix as well as its inverse
{
  inv <- NULL     # initializing inv
  set <- function(y)
  { # function is to reset any previous stored inverse value of matrix stored
    x <<- y  # input argument y is set to x object of the parent environment
    inv <<- NULL  # assign NULL value to inv of the parent environment
  }
  get <- function() x  # get gets the value of x from parent environment
  setInverse <- function(solveMatrix) inv <<- solveMatrix # after setInverse is computed the value is assigned to inv of parent environment
  getInverse <- function() inv # retrieves inv value
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse) # listing elements will provide flexibility to use $ operator
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) # argument used is the one returned by makeCacheMatrix for retrieving the inverse from the cached value
{
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse() # retrive the inverse of the matrix already exists
  if(!is.null(inv)) # check if the inverse of matrix is not null
  {
    message("getting cached data")
    return(inv)             # returns the cached value of inverse matrix and function finishes
  }
  data <- x$get()
  inv <- solve(data)  # solve function to compute the inverse of the matrix
  x$setInverse(inv)  # set the value of the inverse of the matrix
  inv     
}
