library('lattice')

f <- Vectorize( function (x) {
    x^2 / exp(x) - 2 * exp(-9 * sin(x) / (x^2 + x + 1) )
}, 'x')

crossover <- function( x, y ) {  (x + y)/2  }
mutate <- function( x ) { (x^2) %% 30 }

## Plot f
plotf <- function (points) {
    xs <- seq(0, 30, by=0.02)
    ys <- f(xs)

    xyplot(ys~xs,
           panel = function (x, y) {
               llines(x, y, col = 1)
               for (p in points) {
                   panel.xyplot(p, f(p), pch = 20)
               }
           })
}

gen <- function (maxiter, mutprob, initpop = seq(0, 30, by = 5)) {
    pop <- initpop
    vals <- f(pop)

    maxY <- numeric(maxiter)
    maxX <- numeric(maxiter)

    for (i in 1:maxiter) {
        parent_ind <- sample(1:length(pop), size = 2)
        victim_ind <- which.min( vals )
        kid <- crossover( pop[parent_ind[1]], pop[parent_ind[2]] )
        kid <- ifelse( runif(1) <= mutprob, mutate(kid), kid )
        pop[ victim_ind ] <- kid
        vals[ victim_ind ] <- f( kid )

        maxY[i] <- max(vals)
        maxX[i] <- pop[which.max(pop)]
    }

    list(
        maxX = maxX,
        maxY = maxY,
        pop = pop,
        bestX = pop[which.max(vals)],
        bestY = vals[which.max(vals)])
}
