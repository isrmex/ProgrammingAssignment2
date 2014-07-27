## Put comments here that give an overall description of what your
## functions do
Pairs of functions in the first one create a special object that stores a numeric matrix, and has metods to obtain the content of the matrix, or the inverse of the matrix, and to assigna contente to the matrix or inverse of the matrix, to cache the inverse of the matrix, the second one obtain the inverse of the matrix and assign the content to special object, or if the inverse was calculated previously obtain the content.

## Write a short comment describing this function

##create a cache function, need to receive an object of class matrix, for the first
##element of the list

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL ## contain the inverse of the matrix
        set <- function(y) {
                ##with thefunction s (part of the list), change the content of the matrix
                ##to calculate the inverse
                x <<- y ##Assign the object y to original object x (from the cacheMatrix)
                inv <<- NULL ##The content of the matrix change then the inverse matrix is null
        }
        get <- function() x ##the get function return the original matrix x
        ##the setInv function assign the content of the parameter inverse to matrix inv, the inverse
        ## of the x matrix, from the inv atribute is defined in cacheMatrix function
        setInv <- function(inverse) inv <<- inverse 
        getInv <- function() inv ##Return the inverse of the matrix
        ##Define the names for the special list
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)

}


## Write a short comment describing this function

##cacheSolve is for calculate the inverse for quad matrix x 
##(we supouse is possible to calculate the inverse) x is a special object (cacheMatrix)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ##use the getInv function form cacheMatrix to obtain the inv matrix for x
        inv <- x$getInv()
        ##If inv is not null (if the inverse was calclated previously)
        if(!is.null(inv)) {
                message("getting cached data") ##Send a message for inverse is obtained only
                return(inv)  ##return value for inverse matrix (end cacheSolve)
        }
        data <- x$get() ##Obtain matrix to calculate the invesre
        inv <- solve(data, ...) ## Calculate the inverse matrix
        x$setInv(inv) ##Assign the inverse matrix to inv in cacheMatrix (x)
        inv ##return the calculated inverse
}
