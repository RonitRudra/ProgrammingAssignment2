
## makeCacheMatrix creates a cache of a matrix as well as its inverse
## It has functions to store, calculate and fetch the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {           ## argument coerces x as matrix
  inv <- NULL                                         ## setting inverse as empty
  set <- function(y){                                 ## caching function
    x <<- y                                           ## cache matrix
    inv <<- NULL                                      ## cache inverse (NULL for first pass)
  }
  get <- function()x                                  ## function to get matrix
  setinv <- function(solve) inv<<-solve               ## function to calculate inverse
  getinv <- function() inv                            ## function to fetch cached inverse
  list(set=set,get=get,setinv=setinv,getinv=getinv)   ## list to store all functions

}


## cacheSolve checks whether a matrix has its inverse cached or not
## It fetches the cache if present otherwise calculates the inverse

cacheSolve <- function(x, ...) {                      ## argument is matrix
    
  inv <- x$getinv()                                   ## fetch inverse of matrix
  if(!is.null(inv)){                                  ## check if inverse is empty
    message("fetching cached inverse")                ## display message
    return(inv)                                       ## fetching cached inverse
  }
  val <- x$get()                                      ## store matrix in local object
  inv <- solve(val,...)                               ## calculate inverse using solve()
  x$setinv(inv)                                       ## store calculated inverse in cache
  inv                                                 ## return calculated inverse
}
