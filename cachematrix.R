## A pair of functions 'makeCacheMatrix' and 'cacheSolve' that cache the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
# This function creates a special "matrix" object that can cache its inverse.
 m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x                                    # get original matrix
        setsolve <- function(solve) m <<- solve                # save cached matrix
        getsolve <- function() m                               # get cached matrix
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)                              # return special matrix
}


## 'cacheSolve' --this function returns a matrix that is the inverse of 'x' 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## How it works:
  ## 1. Gets the inverse of the special matrix 
  ##    which is returned by makeCacheMatrix above. 
  ## 2. It checks if the inverse has been calculted, if it has been calculated
  ##    it retrives it from the cache else it calculates the inverse and stores it in the cache
  ## 3  It returns the inverse
    m <- x$getsolve()                                          # retrive matrix inverse
         if(!is.null(m)) {                                     # check if inverse has been calculated
                 message("getting cached data")
                 return(m)                                     # return inverse from cache
         }
         data <- x$get()                                       # get original matrix 'x'
         m <- solve(data, ...)                                 # calculate inverse of original matrix 'x'
         x$setsolve(m)                                         # save result in cache
         m                                                     # return inverse of matrix 'x'
}
