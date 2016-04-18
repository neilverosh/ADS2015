# Load all necessary libararies
# If you're using R for the fist time, you will need to install.packges("X") 
# for all of the libraries below.
library(MASS)
library(R.utils)
# library(R2jags)
library(manipulate)
library(MCMCpack)
library(R2WinBUGS)
library(mvtnorm)  

# Bayesian Inference Using Coin Flipping Experiment
# Must have R Studio for this code to work
# If you understand this visual, then you will understand the core of the Bayesian concept of 
# Prior Probability
p <- seq(from=0.005, to=0.995, by=0.005)
manipulate( 
  {plot(p, dbeta(p, alpha.hyper, beta.hyper), 
        col="blue", lwd=2, type="l", las=1, bty="n", 
        ylim=c(0, 8), ylab="Density", xlab="Pr(Heads)",
        main="Prior Probability")
    polygon(c(p, rev(p)), c(dbeta(p, alpha.hyper, beta.hyper), 
                            rep(0, length(p))), col=rgb(0, 0, 1, 0.2), border=NA)}, 
  alpha.hyper=slider(0.1, 10, step=0.1, initial=1), 
  beta.hyper=slider(0.1, 10, step=0.1, initial=1))

# Flip a coin 10 times
set.seed(12345)
p <- seq(from=0.005, to=0.995, by=0.005)  # Support for outcome
p.true <- 0.5  # This is the true probability of heads.  Increase if you want an unbalanced coin.
N <- 10  # Number of flips.  Increase if you want a larger set of outcomes
y <- rbinom(N, size=1, prob=p.true)  # Generate flips following binomial random variable with parameter p.
table(y) # Outcomes

# Graph the likelihood of the observed outcomes given a fair coin assuming the binomial distribution.
likelihood <- sapply(p, function(p) { prod(p^y * (1-p)^(1-y)) } )
# plot(p, likelihood, lwd=2, las=1, bty="n", type="l", xlab="Coin Flip", ylab="Likelihood")
like.rescale <- N * p.true * likelihood/max(likelihood)  # This rescales the likelihood for visual presentation.
plot(p, like.rescale, lwd=2, las=1, bty="n", type="l", xlab="Coin Flip", ylab="Likelihood", main="Likelihood of Observed Data")

# Examine posterior using outcomes from the flips.  Prior and posteriors are conjugate beta distributions.
manipulate(
  {plot(p, like.rescale, lwd=2, las=1, bty="n", 
        ylim=c(0,8), type="l", ylab="Density", xlab="",
        main="Posterior (red) is proportional to Likelihood (black) x Prior (blue)")
    alpha.hyper.post <- alpha.hyper + sum(y)
    beta.hyper.post <- beta.hyper + N - sum(y)
    lines(p, dbeta(p, alpha.hyper, beta.hyper), col="blue", lwd=2)
    polygon(c(p, rev(p)), c(dbeta(p, alpha.hyper, beta.hyper), 
                            rep(0, length(p))), col=rgb(0, 0, 1, 0.2), border=NA)
    lines(p, dbeta(p, alpha.hyper.post, beta.hyper.post), col="red", lwd=2)
    polygon(c(p, rev(p)), c(dbeta(p, alpha.hyper.post, beta.hyper.post), 
                            rep(0, length(p))), col=rgb(1, 0, 0, 0.2), border=NA)
    lines(p, like.rescale, lwd=2)}, 
  alpha.hyper=slider(0.1, 10, step=0.1, initial=1), 
  beta.hyper=slider(0.1, 10, step=0.1, initial=1))