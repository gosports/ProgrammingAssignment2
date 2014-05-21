## Computing the inverse of Matrix.

    # > x <- matrix(rnorm(16), nrow = 4)  (Creates a matrix x)
    # > x                                 (displays it)
    # > x1 <- solve(x)                    (calculates the inverse)
    # > x1                                (displays the inversse)
    # > X*x1                              (element based multiplication)
    # > x%*%x1                            (true matrix multiplication, yeilding Identity matrix, proving x1 is the inverse of x)
  # > cx <- makeCacheMatrix(x)        (Creates special matrix)
  # > cx$get()                        (Returns the matrix)
  # > cacheSolve(cx)                  (Returns the inverse)
  # > cacheSolve(cx)                  (Calls the 2nd time, so return is the cached inverse)
  # > cacheSolve(cx)%*%cx$get()       (Returns Identity matrix, providing cacheSolve (cx) is the inverse of cx$get()


## Caching the Inverse of a Matrix

## Below are two functions that are used to create a special "matrix" that 
### 1) can cache its inverse with the "solve" function.
### 2) retrieve the inverse from the casche OR compute the inverse of a matrix with the "solve" function. 


## 1) This function creates a special "matrix" object that can cache its inverse. 
## Computing the inverse of matrix is done with the "solve" function in R, returning a list of functions to:
  # 1. Set the value of the matrix
  # 2. Get the value of the matrix
  # 3. Set the value of the inverse
  # 4. Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL                   # Inverse will store the casches inverse matric.
  set <- function(y) {        # set the value of the matrix.
    x <<- y
    s <<- NULL
  }                           # get the value of the matrix.
  get <- function() x
                              # set the value of Inverse with Solve function.
  setsolve <- function(solve) s <<- solve
                              # get the value of inverse.
  getsolve <- function() s
                              # Returne the matrix with newly defined functions.
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}
  


## 2) This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache. 
## Otherwise it computes the inverse of a matrix with the "solve" function as above.  

cacheSolve <- function(x, ...) {
                      # Return a matrix that is the inverse of 'x'
  s <- x$getsolve()
                      # If the inverse is already calculated, return it.
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
                      # If the inverse is not yet calculated, solve () returns its inverse.                   
  data <- x$get()
  s <- solve(data, ...)
                      # cache the inverse
  x$setsolve(s)
                      # return it.
  s
  
  }
