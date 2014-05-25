# ==========================
#  Inverse Caching Matrices
# ==========================
# These functions provide inverse-caching functionality for matrices.
# makeCacheMatrix creates an inverse-caching matrix from a regular
# matrix, and cacheSolve returns the inverse of an inverse-caching
# matrix.

# =================
#  makeCacheMatrix 
# =================
# Create a special "matrix" which can cache its inverse. The actual data
# type returned is a list containing getter and setter functions for both
# the matrix itself and its inverse. Setting the value of the matrix will
# automatically erase any cached inverse (ie. reset it to NULL)

makeCacheMatrix <- function(x = matrix()) {
        
        inverse <- NULL
        get <- function() x
        set <- function(mat) {
                x   <<- mat     # set x to the new matrix value
                inv <<- NULL    # old inverse is invalid: erase it
        }
        
        getinverse <- function() inverse
        setinverse <- function(inv) inverse <<- inv
        
        list(get = get,
             set = set,
             getinverse = getinverse,
             setinverse = setinverse)
}

# ------------------------------------------------------------------------

#    Note regarding Assignment: Additional arguments (...)
#   -------------------------------------------------------
# Since the cacheSolve() signature includes additional arguments (...),
# I assume they should be passed on to the solve() function to allow 
# for general linear equation solving using the cachematrix. 

# Note that the cached inverse is not used in equation solving. This 
# could be done by simply left-multiplying the RHS by x$getinverse().
# However this is not implemented in cacheSolve(), since it is beyond
# the scope of the assignment and would clutter up the code.

# Thus, cacheSolve(x) performs a caching inverse operation, while
# cacheSolve(x, b, ...) solves for y in x %*% y == b without caching.

# ============
#  cacheSolve
# ============
# Compute the inverse of a caching matrix created with makeCacheMatrix.
# cacheSolve(x) will return a cached inverse of x. If the inverse is not
# already cached, it will compute and cache it.
# With additional arguments, cacheSolve solves a linear matrix equation.
# cacheSolve(x, b, ...) is equivalent to calling solve(x, b, ...) with
# a regular matrix.

cacheSolve <- function(x, ...) {
        # if additional arguments are provided, just call solve()
        if (length(list(...)) > 0) { solve(x$get(), ...) }
        # otherwise, perform cached inverse operation
        else {
                cached <- x$getinverse()
                # if cache is empty, compute inverse and store
                if (is.null(cached)) {
                        inv <- solve(x$get())
                        x$setinverse(inv)
                }
                # return stored inverse
                x$getinverse()
        }
}

# ------------------------------------------------------------------------

# Examples:
mat <- matrix(c(5,0,0,5,5,0,5,5,5), 3, 3)
c <- makeCacheMatrix(mat)
test <- function() {
        # get matrix
        message( "retrieving matrix:" )
        print( c$get() )
        # get inverse
        message( "retrieving cached inverse:" )
        print( c$getinverse() )
        # call cacheSolve
        message( "calling cacheSolve():" )
        print( cacheSolve(c) )
        # get inverse
        message( "retrieving cached inverse:" )
        print( c$getinverse() )
        # test inverse
        message( "multiplying matrix by cached inverse:" )
        print( m$get() %*% m$getinverse() )    # identity matrix
        # solve equation c * b = 2I_3
        message( "calling cacheSolve with additional arguments - " )
        message( "solving matrix equation C X = 2I:" )
        print( cacheSolve(c, matrix(c(2,0,0,0,2,0,0,0,2),3,3)) )
        # return NULL
        invisible(NULL)
}
