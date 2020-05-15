## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object and store(cache) its inverse 

makeCacheMatrix <- function(x = matrix())
    {
        m <- NULL
  
        set <- function(y)
            {
                x <<- y
                m <<- NULL
            }
  
        get <- function() x
  
        setinvmat <- function(invmat) m <<- invmat
  
        getinvmat <- function() m
  
        list(set = set, get = get, setinvmat = setinvmat, getinvmat = getinvmat)
    }


## This function computes the inverse of the special "matrix" object
## created by the makeCacheMatrix above.
## If the inverse is already computed(i.e if the matrix is not changed)
## it is retrived from the cache.


cacheSolve <- function(x, ...)
    {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getinvmat()
  
        if(!is.null(m))
            {
                message("getting cached data")
                return(m)
            }
  
        data <- x$get()
  
        m <- solve(data, ...)
  
        x$setinvmat(m)
  
        m
    }
