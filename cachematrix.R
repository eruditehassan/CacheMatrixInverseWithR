
## This function will create an object that is capable of caching matrix inverse
makeCacheMatrix <- function(x = matrix()) {
    ## i denotes the inverse
    i <- NULL
    ## Set function sets the given value to x
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    ## get function returns the value of x
    get <- function() x
    ## setinverse function will set given "inverse" value to i
    setinverse <- function(inverse) i <<- inverse
    ## getinverse function returns the inverse value i
    getinverse <- function() i
    ## creating a list using the functions coded above
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## This function will solve the matrix to find its inverse
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    ## storing inverse value in i
    i <- x$getinverse() 
    ## checking if the inverse value is null or not
    if(!is.null(i)) {
        ## if not null then obtaining from cache
        message("getting cached data")
        ## returning the value to terminate function
        return(i)
    }
    ## if no value in cache, then solving it
    ## Obtaining data i.e matrix
    data <- x$get()
    ## Solving the matrix and storing inverse in i
    i <- solve(data, ...)
    ## Setting inverse value
    x$setinverse(i)
    ## returning inverse i
    i
}
