# Programming Assigment 2
# Creates a special object that can store a matrix and inverse

# Stores a matrix and its inverse
# Also stores functions that allows the matrix and its inverses to be updated
makeCacheMatrix <- function(x = matrix()) {
  # Resets inverse
  inv <- NULL
  
  # Allow cacheSolve to access matrix
  get <- function() x
  
  # Allow cacheSolve to access cached inverse (if inv exists)
  getInv <- function() inv
  
  # Allow cacheSolve to set the inverse (if inv does not exist)
  setInv <- function(inverse) inv <<- inverse
  
  list(get = get,
       setInv = setInv,
       getInv = getInv)
}


# Computes the inverse of the original makeCacheMatrix
# Checks if the inverse has been calculated - if so retrieves from makeCacheMatrix and returns
# If not, calculates inverse, updates the inv in makeCachceMatrix, and returns the inverse
cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  
  # Check if inverse has already been calculated
  if (!is.null(inv)) {
    return(inv)
  }
  # If not, calculate the inverse
  m <- x$get()
  inv <- solve(m)
  
  # Reset the inverse in the makeCacheMatrix
  x$setInv(inv)
  
  # Return the value
  inv
}
