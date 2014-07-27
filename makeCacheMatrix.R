##Name: Israel
##Date: 24-july-2014
##last mod: 26-july-2014
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