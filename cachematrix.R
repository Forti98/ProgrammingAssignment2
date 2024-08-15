## Put comments here that give an overall description of what your
## functions do

## Esta función crea un objeto "matriz" especial que puede almacenar en caché su inversa.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Esta función calcula la inversa de la "matriz" especial devuelta por makeCacheMatrix anteriormente. Si la inversa ya ha sido calculada (y la matriz no ha cambiado), entonces cacheSolve debe recuperar la inversa de la caché.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}

# Crear una matriz simple
m <- matrix(c(1, 2, 3, 4), 2, 2)

# Crear un objeto especial de tipo matriz que puede almacenar su inversa
cacheMatrix <- makeCacheMatrix(m)

# Calcular la inversa usando cacheSolve
cacheSolve(cacheMatrix)

# Si llamas de nuevo a cacheSolve, debe devolver la inversa desde la caché
cacheSolve(cacheMatrix)
