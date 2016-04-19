##Two functions to compute and cache the inverse of a square matrix

## Stores the inverse of a square matrix

makeCacheMatrix <- function(x = matrix()) 
{
    inv_mat <- NULL
    set <- function(y)
    {
          x <<- y
          inv_mat <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv_mat <<- solve
    getinv <- function() inv_mat
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Checks cache to see if inverse has already been computed;
## Computes inverse if it is not cached

cacheSolve <- function(x, ...) 
{
      inv_mat <- x$getinv()      
      if(!is.null(inv_mat))
      {
            message("Getting cached matrix inverse")
            return(inv_mat)
      }
      data <- x$get()
      inv_mat <- solve(data, ...)
      x$setinv(inv_mat)
      inv_mat
}
