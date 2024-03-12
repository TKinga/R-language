x <- rnorm(100)
y <- rnorm(100)
n <- length(x)

nthOrderPolynomialRegression <- function(X, Y) {
    function(X, Y) {
        CalcA2(X, Y)
    }
    CalcA1(X, Y)
    CalcA0(X, Y)
}

CalcA2 <- function(X, Y) {
    sumX <- Summary(X)
    sumY <- Summary(Y)
    sumXY <- Summary(X, Y)

    sumX2Y <- Summary(X^2, Y)
    sumX2 <- Summary(X^2)

    sumX3 <- Summary(X^3)
    sumX4 <- Summary(X^4)

    numerator <- sumX2Y * ((sumX2 * n) - (sumX * sumX)) - sumX3 * ((sumXY * n) - (sumY * sumX)) + sumX2 * ((sumXY * sumX) - (sumY * sumX2))
    denominator <- sumX4 * ((sumX2 * n) - (sumX * sumX)) - sumX3 * ((sumX3 * n) - (sumX2 * sumX)) + sumX2 * ((sumX3 * sumX) - (sumX2 * sumX2))
    return(numerator / denominator)
}

CalcA1 <- function(X, Y) {

}

CalcA0 <- function(X, Y) {

}

Summary <- function(X, Y) {
    sum <- 0
    n <- length(X)

    if (missing(Y)) {
        for (i in 1:n) {
            sum <- sum + X[i]
        }
        return(sum)
    }

    for (i in 1:n) {
        sum <- sum + X[i] * Y[i]
    }
    return(sum)
}

nthOrderPolynomialRegression(x, y)
