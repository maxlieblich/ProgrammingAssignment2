## This creates a matrix object with a cacheable inverse and defines a function
## to recall or compute and cache the inverse. I added a tiny bit of checking
## to make sure inverses are within a tolerance and the user is not passing a named
## parameter to cacheSolve that will interfere with the solve function.

## Construct the cacheable matrix: an object with a cacheable inverse.
## Unlike in the example code for vectors, we keep a copy of the input in the
## local environment to protect against lazy eval of the input. This is discussed
## at https://class.coursera.org/rprog-002/forum/thread?thread_id=370
## As per instructions, I assume an invertible matrix is passed in when the
## function is used (contrary to the default value of x!)
## NOTE: I have added a tolerance parameter and an optional test to the setter to
## see if the user is passing an inverse that is within the tolerance in each entry.
## Who knows where the disgusting inverse the user inserts comes from?
## I just want to make sure it has some semblence of correctness if I will permit
## it to be set. I'm sure this is not the best way to check such things.
## It is easy to require tighter
## tolerance than solve can achieve, but at least it's good to know that.
## The inverse will not be set without an appropriate tolerance attached
## to the makeCacheMatrix.

makeCacheMatrix <- function(x = matrix(), tol = 1e-7, check.inverse = TRUE) {
    inv <- NULL
    x <- x
    tol <- tol
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse, check = check.inverse) {
        if (check){
            diff <- x %*% inverse - diag(nrow(x))
            # if any entry exceeds tolerance warn user
            if (sum(abs(diff) > tol)) message(paste(c("passed inverse not within required tolerance of ", tol,". Inverse set to NULL.")))
            inv <<- NULL
        } else {
            inv <<- inverse
        }
    }
    getinv <- function() inv
    settol <- function(tolerance) tol <<- tolerance
    gettol <- function() tol
    list(set = set, get = get,
         setinv = setinv, getinv = getinv,
         settol = settol, gettol = gettol)
}


## If the makeCacheMatrix has a cached inverse, return it.
## Otherwise, calculate the inverse, cache it, and return it.
## We check for an accidental parameter
## name collision: if ... contains a parameter named b, it will wreak havoc
## with the use of solve

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        tol <- x$gettol()
        args <- list(...)
        if(!is.null(inv)) {
            message("getting cached inverse")
            return(inv)
        }
        mat <- x$get()
        # If user has passed an argument called "b",
        # it will interfere with the "b" argument of solve, resulting in chaos.
        # Check first that args is non-empty, then whether "b" is in there
        # and issue a warning if necessary.
        if (length(args) > 0 && exists("b", where=args)) {
            warning("the passed b argument will interfere with the construction of the inverse matrix")
        }

        inv <- solve(a=mat, ...)
        x$setinv(inv)
        inv
}
