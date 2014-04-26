makeCacheMatrix <- function(x = matrix()) {     # This function is expecting a matrix as input and will cache the value of the matrix into a cache in a global environment
        cachedMatrix <- NULL                    # initialisation of the cachedMatrix Variable as NULL. at this point, nothing is cached
        set <- function(y) {                    # this function sets the value of the matrix
                x <<- y                         # assigns y to a Global environment X
                cachedMatrix <<- NULL
        }
        get <- function() x                     # this function gets the value of the matrix         
        setInverse <- function(solve) cachedMatrix <<- solve # this function takes the matrix and assigned the inverse the the cachedMatrix, this is where the inverse of the matrix is cached
        getInverse <- function() cachedMatrix   # this function gets the value of the new cachedMatrix containing the inverse             
        list(set = set, get = get,              # definition of all the different functions available within makeCacheMatrix as a list
                setInverse = setInverse,
                getInverse = getInverse)
}


cacheSolve <- function(x, ...) {                # this function is meant to return the value of the cachedMatrix, which is the inverse of the matrix x      
        cachedMatrix <- x$getInverse()          # query the inverse of the matrix x cached
        if(!is.null(cachedMatrix)) {            # if the cachedMatrix contains values representing the inverse of matrix x ...
                message("cached data")          # ... then send message saying that the data has been cached ...
                return(cachedMatrix)            # and return its value
        }
        data <- x$get()                         # if the cache is non existing, let's get the value of the matrix
        cachedMatrix <- solve(data, ...)        # and cache its inverse using solve()
        x$setInverse(cachedMatrix)              # assign the inverse computed and set the value as the inverse of the matrix and Cache it in cachedMatrix variable
        cachedMatrix                            # return the result of the cachedmatrix -- the inverse of the matrix x
}