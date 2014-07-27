##Name: Israel
##Date: 24-july-2014
##last mod: 26-july-2014
##cacheSolve is for calculate the inverse for quad matrix x 
##(we supouse is possible to calculate the inverse) x is a special object (cacheMatrix)
cacheSolve <- function(x, ...) {
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