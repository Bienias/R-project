## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL                                        # set m variable to NULL 
        set <- function(y) {                             # take a matrix and set m to NULL 
                x <<- y
                m <<- NULL
        }
        get <- function() x                              # function returns original matrix
        set_Inv_Matrix <- function(solve) m <<- solve    # function count a inverted matrix and set a variable "m" as that matrix 
        get_Inv_Matrix <- function() m                   # function return "m" - that means inverted matrix or NULL
        list(set = set, get = get,                       # returns a list of functions
             set_Inv_Matrix = set_Inv_Matrix,
             get_Inv_Matrix = get_Inv_Matrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        m <- x$get_Inv_Matrix()                   # taking Iverted Matrix if is able ( or Null )
        if(!is.null(m)) {                         # if Inv. matrix exist - print "getting cached data" and return it
                message("getting cached data")
                return(m)
        }
        data <- x$get()                           # if inv. matrix not exist - take original one
        m <- solve(data, ...)                     # invert it by function solve
        x$set_Inv_Matrix(m)                       # set in makeCacheMatrix envirovment
        m                                         # return it
}
