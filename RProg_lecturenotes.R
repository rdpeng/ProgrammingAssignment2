# R week 4 - Simulation 
# Date: 8 Nov 

# rnorm: random normal distributions 
# pnorm: poisson dist. cumulative distribution
# dnorm: normal probaility density 
# qnorm: quantile function 

All require you to specify mean and sd
Default: mean 0, sd = 1

# dnorm: dnorm allows evaluation in log scale, usually you'll use this
# pnorm: allows lower.tail argument, set lower.tail=FALSE if you want to
# evaluate the upper tail 
# rnorm: n = number of numbers to be generated

set1 <- rnorm(n=10) # generates 10 numbers
print(set1)

set2 <- rnorm(n=10, m=0, sd=2)
print(set2)

# set.seed()
# use set.seed to ensure reproducibility

set.seed(1)
set3 <- rnorm(n=5); print(set3) # seed = 1
set4 <- rnorm(n=5); print(set4) # no seed
set.seed(1)
set5 <- rnorm(n=5); print(set5) # seed = 1

# set3 will be same as set5

# ---------------------------------------------------------------------------- #

# rpois generates random poisson numbers
# rpois(n= how many numbers, p = rate of numbers)

set6 <- rpois(10, 1); print(set6) # 10 numbers, rate = 1, mean also tends to 1.

# ppois generates a probability poisson curve of cumulative distribution of 
# numbers where poisson variables is less than or equal to two, when rate is 2.

ppois(2,2)
ppois(6,2)

# ---------------------------------------------------------------------------- #
# Generating random numberes from linear model

Y = B0 + (B1)X + e

# e = N(0.2)^2
# Assume x = N(0.1)^2 
# B1 = 2
# B0 = 0.5

# test
set.seed(2)
x <- rnorm(n=100)
e = rnorm(n=100, mean=0, sd=2)
Y <- 0.5 + 2 + x + e # equation with substitutes

# view results
summary(Y)
plot(x, Y) # clear linear relationship

## What if x is a binary random variable instead of a normally distributed 
## random variable, like gender, or treatment for a control?

# Solution: Generate random binomial data from rbinom function
# rbinom (n = num observations, size = num trials, >=0, prob = probability of 
# success on each trial, lower.tail=TRUE, log.p = false)

set.seed(10)
x <- rbinom(n=100, size=1, prob=0.5)
e <- rnorm(n=100, mean=0, sd=2)
Y <- 0.5 + 2 + x + e
summary(Y)
plot(x, Y)

# ---------------------------------------------------------------------------- #
# Simulate a poisson model

# Y ~ poisson(mu)
# log.mu = B0 + B1(x)

# B0 = 0.5
# B1 = 0.3

set.seed(1)
set7 <- rnorm(100) # predictor var
log.mu <- 0.5 + 0.3 * set7 # linear var

Y = rpois(100, exp(log.mu))
summary(Y) # mean 1.5, range between 0 and 6
plot(set7, Y)

# clear linear relationship
# clear mean = 0

# ---------------------------------------------------------------------------- #
# Simulate random sampling 

# Get any arbitrary set of numbers then sample them

set.seed(1)
sample(1:10, 4)
sample(letters, 5)

# permutation; permutes all elements in random order
sample(1:10)

# with replacements//repeats
sample(1:10, replace=TRUE)

# ---------------------------------------------------------------------------- #
R Profiler: Why is my code so slow

# optimise code
# figure which parts are slow, better than guessing
# small piece runs fast but they might get embedded in bigger, longer loops

General rules

## Don't think about it at first
## Write something functional first
## Second, make it readable, understandable
## Last: optimise

system.time() # measures to evaluate length of time, t in seconds
user time = amount of time charged to CPU to run function (by computer)
elapsed time = "wall clock" elapsed time

Usually the same, but ... 

# Parallel package: elapsed time less than user time, multi-core processors present
# Busy CPU: elapsed time greater than user time if CPU is finishing other tasks

Multi-core libs: BLAS libraries

# BLAS: basic linear algebra standard libraries
# subroutine libraries
# vecLib
# Accelerate libs 

system.time(readLines("http:www.jpsph.edu")) # laggy website

# ---------------------------------------------------------------------------- #
# System profiler - find more time sinks

Rprof() 
# starts profiler in r, works in most cases except old versions
# regularly prints function call-stack (0.02s), tabulates time spent per function 
# Useless for functions faster than .02s

summaryRprof()
# summarises long-winded output of Rprof()

## WARNING: DO NOT USE RPROF() + SYSTEM.TIME() TGT
## YOU WILL BE SAD

by.self() 
# most useful 

Notes

# break code into different functions
# makes easier for profiler and you to understand, sort by function name
# C and fortran code not profiled, can't measure it
# remember to use summaries
