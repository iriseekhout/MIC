# Part 1 contains the code to simulate a dataset with 10 items.
# If you want to analyze your own dataset, go to Part 2

# Remove all objects
rm(list=ls(all=TRUE))

# Acquire packages
library(mirt)     # for simulating data
library(lavaan)   # for estimating the transition ratings reliability
library(pROC)     # for estimating ROC-based MIC


## Simulate a set of IRT item parameters

set.seed(12345)

b2  <- c(-0.8, -0.8, -0.4, -0.4, 0, 0, 0.4, 0.4, 0.8, 0.8)
bc <- b2/4
b1 <- b2 - 1 + sample(bc)
b3 <- b2 + 1 + sample(bc)
a1 <- sample( 1+b2/2 )

( cf.simb <- data.frame(a1,b1,b2,b3) )

# Transform b-parameters to d-parameters (mirt works with d-parameters)
# difficulty (b) = easiness (d) / -a

cf.sim <- cf.simb

colnames(cf.sim) <- c("a1","d1","d2","d3")

cf.sim$d1 <- -cf.simb$b1*cf.sim$a1
cf.sim$d2 <- -cf.simb$b2*cf.sim$a1
cf.sim$d3 <- -cf.simb$b3*cf.sim$a1
round(cf.sim, 3)
round(colMeans(cf.sim), 3)

# Simulate dataset using 'mirt'

a1 <- as.matrix(cf.sim[ , 1])
d1 <- as.matrix(cf.sim[ , -1])


### Create theta T1 and dataset 1 (baseline)

N <- 1000         # sample size
prop.imp <- 0.3   # proportion improved
mn.imic  <- 0.5   # mean individual MICs in terms of theta change

tet1s <- rnorm(N, 0, 1)   # simulate theta T1
tet1s <- as.matrix(tet1s)

set.seed( sample(10000:20000,1) )

dat1 <- simdata(a1, d1, N, itemtype="graded", Theta=tet1s)
dat1 <- as.data.frame(dat1)

xo1 <- rowSums(dat1)   # This is the baseline test (sum) score


### Create theta change

rt1ch <- -0.5            # correlation between theta T1 and theta change
tetchs <- rt1ch*tet1s + sqrt(1-rt1ch^2)*rnorm(N, mean(tet1s), sd(tet1s))
cor(tet1s,tetchs)

tetchs <- tetchs - mean(tetchs)         # makes mean = 0
tetchs <- (tetchs/sd(tetchs))*1         # makes SD = 1

( qtl <- quantile(tetchs, prob=(1-prop.imp)) )
( mean.tetchs <- mn.imic - qtl )        # Estimate mean theta change to get
# the desired proportion improved
tetchs <- tetchs + mean.tetchs          # transform mean of tetchs


### Create theta T2 and dataset 2  (follow-up)

tet2s <- as.matrix(tet1s + tetchs)     # theta T2

cor(tet1s, tet2s)

set.seed( sample(30000:40000,1) )

dat2 <- simdata(a1, d1, N, itemtype="graded", Theta=tet2s)
dat2 <- as.data.frame(dat2)


### Insert ERROR IN TRANSITION RATINGS

rel.trat <- 0.5   # reliability of the perceived change/transition rating

( sd.ch.error <- sqrt(((1-rel.trat)/rel.trat)*sd(tetchs)^2) )
tetch.percv <- tetchs + rnorm(N, 0, sd.ch.error)  # perceived change


### Create iMIC distribution
imic <- rnorm(N, mn.imic, 0.1*mn.imic)

### create dichotomous transition ratings
trat <- numeric(N)
trat[tetch.percv > imic] <- 1
mean(trat)                           # proportion improved based on perceived change

### Create dataset

org <- data.frame(dat1, dat2, trat)

nitems = 10     # Provide the number of items in the scale

## Simplify/standardize the item names
names(org)[1:nitems] <- paste0('v1', '_', 1:nitems)
names(org)[(nitems+1):(2*nitems)] <- paste0('v2', '_', 1:nitems)
names(org)[2*nitems+1] <- "trat"
names(org)

## copy original data into a workfile (dat)
example <- org
head(example)

save(example, file ="data/example.rda")
