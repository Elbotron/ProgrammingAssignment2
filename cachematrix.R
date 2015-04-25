
## creates a cache for our solved variable (result of the solve() function).
## if the result of solve(x) has already been computed, we will use this 
## function to retrieve the saved result instead of computing solve(x) again,
## which could be a computationally expensive task.
makeCacheMatrix <- function(x = matrix()) {
  
  ## set solved to null initially
  solved <- NULL
  
  ## sets the value of our matrix x and sets solved to null
  set <- function(y) {
    x <<- y
    solved <<- NULL
  }
  
  ## this function returns our matrix x
  get <- function() x
  
  ## sets solved to the argument s
  setSolved <- function(s) solved <<- s
  
  ## returns solved
  getSolved <- function() solved
  
  list(set = set, get = get, setSolved = setSolved, 
       getSolved = getSolved)

}


## This function will first check to see if solve(x) was computed. If not, it
## will compute solve(x) and return it. If it has already been computed and
## cached, it will return the cached value.
cacheSolve <- function(x, ...) {

  ## get cached value
  solved <- x$getSolved()
  
  ## if cached value is not null...
  if(!is.null(solved)) {
    
    message("Getting cached data...")
    
    ## ...return cached value
    return(solved)
  }
  
  ## othewise:
  
  ## get the value of the matrix x
  data <- x$get()
  
  ## compute solve(data)
  solved <- solve(data, ...)
  
  ## save the computed value in the cache for future use
  x$setSolved(solved)
  
  ## return the computed value
  solved
  
}
