# CECI EST UN TEST

rm(list = ls())
library(survival)

generateData <- function(n1, n2, lambda = 0.1, theta = 0.07){

    obs <- matrix(NA, n1 + n2, 2)
    colnames(obs)  <- c("time", "delta")

    if (n1 >= 1)
        obs[1:n1,] <- cbind(rexp(n1, lambda), 1)

    if (n2 >= 1)
        obs[-(1:n1),] <- cbind(rexp(n2, theta), 0)

    return(Surv(obs[,1], event = obs[,2]))
}

## Essayons d'avoir un code sur un cas particulier
x <- generateData(90, 10)
fit <- survfit(x ~ 1)
summary(fit, time = 10)
## et pour obtenir juste l'estimation de la proba.
summary(fit, time = 10)$surv

## Creation de la fonction
performance <- function(n1, n2 = 100 - n1, N = 500){
    estimations <- rep(NA, N)
    for (i in 1:N){
        x <- generateData(n1, n2)
        fit <- survfit(x ~ 1)
        estimations[i] <- summary(fit, time = 10)$surv
    }

    theo <- 1 - pexp(10, 0.1)
    biais <- mean(estimations) - theo
    variance <- var(estimations)
    mse <- mean((estimations - theo)^2)

    return(c(biais = biais, variance = variance, mse = mse))
}

ans <- matrix(NA, 10, 3)
colnames(ans) <- c("biais", "variance", "mse")

## La boucle prend du temps à tourner...
i <- 1
for (n2 in seq(0, 90, by = 10)){
    ans[i,] <- performance(100 - n2, n2)
    i <- i + 1
}

par(mfrow = c(1, 3))
names <- paste(seq(0, 90, by = 10), "%", sep = "")
xlab <- "Taux de censure"
barplot(ans[,"biais"], names = names, xlab = xlab, ylab = "Biais")
barplot(ans[,"variance"], names = names, xlab = xlab, ylab = "Variance")
barplot(ans[,"mse"], names = names, xlab = xlab, ylab = "MSE")

