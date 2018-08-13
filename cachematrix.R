## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function creates a special "matrix" object that can cache its inverse.
# this function sets and gets the value of the matrix and also sets and gets 
# the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL 
    set <- function(y) { # set x so it can be used in the parent environment
        x <<- y 
        i <<- NULL
    }
    get <- function() x # get x from the parent environment
    setinverse <- function(inverse) i <<- inverse # set the value of the matrix in the parent environment
    getinverse <- function() i # get i from the parent environment
    list(set = set, # name the objects so you can call on them with '$' later
         get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}


## Write a short comment describing this function
# this function calculates the inverse of a given matrix and caches that inverse.
# if the inverse is stored in the cache it will display the inverse
# if the inverse hasn't been calculated it will calculate and return the value to the parent environment

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse() # grab i from the parent environment
    if(!is.null(i)) { # if the inverse of i has already been calculated return the cached data
        message("getting cached data")
        return(i) # return i without calculating the inverse of the matrix again
    }
    data <- x$get() # if i hasn't been calculated already then gets the matrix from the input object 
    i <- solve(data, ...) # calculate the inverse of the matrix and store as i
    x$setinverse(i) # set the inverse in the input object 
    i # return the value of the inverse matrix to the parent environment by printing the inverse object
}