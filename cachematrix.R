## These functions are used to cache the inverse of a matrix in order to save
## computing time in situations in which this calculation needs to be repeated
## several times withouth changing the result.


## The "makeCacheMatrix" funcion creates a special object from a given matrix.
## This object stores the original matrix (x, given as an argument), its inverse
## (x_inverse, originally NULL), and four methods to recover and modify the two
## values.

makeCacheMatrix <- function(x = matrix()) {
      x_inverse <- NULL
      
      set <- function(y) {
            x <<- y
            x_inverse <<- NULL
      }
      get <- function() x
      set_inverse <- function(i) x_inverse <<- i
      get_inverse <- function() x_inverse
      
      list(set = set, get = get, set_inverse = set_inverse,
           get_inverse = get_inverse)
}


## The "cacheSolve" funcion recovers the inverse function (x_inverse) from the
## object created. If it is null, the function recovers the original matrix,
## computes its inverse (using the solve function), writes the result in the
## object and returns it. If x_inverse is not null (meaning that it has previously
## been calculated and written in the object), it displays a message and returns
## the stored result.

cacheSolve <- function(x, ...) {
      x_inverse <- x$get_inverse()
      if(!is.null(x_inverse)) {
            message("getting cached data")
            return(x_inverse)
      }

      m <- x$get()
      inverse <- solve(m)

      x$set_inverse(inverse)
      
      inverse
}