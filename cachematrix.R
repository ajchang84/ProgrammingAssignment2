## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix allows the caching of the matrix and its inversion through the variables x and i that lives only within the enclosing environment
## cacheSolve is the actual matrix inversion function but it firsts checks if it has been cached before solving

## Write a short comment describing this function

## function that returns a list of functions, acts similiarily to objects in OOP

## functions include: 
## set() stores a matrix 
## get() retrieves the matrix
## setinverse() stores the inverted matrix
## getinverse() retrieves the inverted matrix
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
                setinverse = setinverse,
                getinverse = getinverse)
}


## Write a short comment describing this function

## function that attempts to retrieve the cached inverted matrix, if not found, compute the inverted matrix via solve()
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                print("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
