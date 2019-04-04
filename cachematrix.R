## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## getmatrix devuelve la matriz
## setmatrix setea la matriz
## getinverse devuelve la inversa de la matriz
## setinverse setea la inversa de la matriz
makeCacheMatrix <- function(x = matrix()) {
        invmat <- NULL
        set <- function(y) {
                x <<- y
                invmat <<- NULL
        }
        get <- function() x
        setinverse <- function() invmat <<- solve(x) ## inversa de la matriz
        getinverse <- function() invmat
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Write a short comment describing this function
## Obtengo la matriz con la funcion getinverse
## Si lo que obtengo NO es nulo está cacheada y devuelvo ese valor
## Si lo que obtengo es nulo no está cacheada
## entonces la calculo y lo cacheo
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	invmat <- x$getinverse()
	if ( !is.null(invmat) ) {
		message ("devuelvo dato de la cache")
		return(invmat)
	}
	datos <- x$get()
	invmat <- solve(datos, ...)
	x$setinverse(invmat)
	invmat
}
