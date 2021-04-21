#############################################################

################################  PART 2  ##############################


###  Read your own data

# The following code opens a windows dialogue box in which you can browse
# to your data file.
# Note that your datafile should contain the items of T1 and T2 (items in
# the same order) and in the last column the dichotomous transition ratings.
# Don't include scale scores or change scores in your file.


org <- read.table(file.choose(), header=T)  # The file is named "org"

nitems = 10     # Provide the number of items in the scale

## Simplify/standardize the item names
names(org)[1:nitems] <- paste0('v1', '_', 1:nitems)
names(org)[(nitems+1):(2*nitems)] <- paste0('v2', '_', 1:nitems)
names(org)[2*nitems+1] <- "trat"
names(org)

# The file is next copied to a file called "dat" which is the working file

dat <- org


###### Estimate TR reliability with lavaan (NOTE: modify the model to the number of items)

model <- '
   # Factors
   F1 =~ a1*v1_1+a2*v1_2+a3*v1_3+a4*v1_4+a5*v1_5+a6*v1_6+a7*v1_7+a8*v1_8+
         a9*v1_9+a10*v1_10+trat
   F2 =~ a1*v2_1+a2*v2_2+a3*v2_3+a4*v2_4+a5*v2_5+a6*v2_6+a7*v2_7+a8*v2_8+
         a9*v2_9+a10*v2_10+trat

   # Correlated errors over time
   v1_1 ~~ v2_1
   v1_2 ~~ v2_2
   v1_3 ~~ v2_3
   v1_4 ~~ v2_4
   v1_5 ~~ v2_5
   v1_6 ~~ v2_6
   v1_7 ~~ v2_7
   v1_8 ~~ v2_8
   v1_9 ~~ v2_9
   v1_10 ~~ v2_10
   '

fit <- cfa(model, data=dat, ordered=names(dat),
           test="mean.var.adjusted")

fitMeasures(fit, fit.measures = c("cfi.scaled","tli.scaled","rmsea.scaled",
                                  "rmsea.ci.lower.scaled","rmsea.ci.upper.scaled","rmsea.pvalue.scaled",
                                  "srmr") )

MI <- modificationIndices(fit)
MI[MI$sepc.lv>0.3,]

# Note: Check the model fit. If necessary improve model fit by allowing
# correlated errors cross-sectionally (e.g., v1_1 ~~ v1_2, v2_1 ~~ v2_2)

pe <- parameterEstimates(fit, standardized=T, rsquare=T)
( rel.trat <- pe$est[pe$lhs=="trat" & pe$op=="r2"] )   # Reliability TR


###### Estimate MICs

xo1 <- rowSums(dat[,1:nitems])                 # sumscore T1
xo2 <- rowSums(dat[,(nitems+1):(2*nitems)])    # sumscore T2
dat$xoc <- xo2 - xo1                           # change score

( q <- mean(dat$trat) )            # q = proportion improved (perceived)
( p <- log(q/(1-q)) )              # p = logodds(pre)

( cor.trat.xoc <- cor(dat$trat, dat$xoc) )  # correlation anchor-change

## Do ROC analysis

rocobj <- roc(dat$trat, dat$xoc, quiet = TRUE)

( mic.roc <- coords(rocobj, x="best", input="threshold", ret="threshold",
                    best.method="youden", transpose = TRUE) )


## Do logistic regression and calculate MIC(predicted) and MIC(adjusted)

mylogit <- glm(trat ~ xoc, data = dat, family = "binomial")

C <- coef(mylogit)[1]                 # intercept coefficient C
B <- coef(mylogit)[2]                 # regression coefficient B

( mic.pred <- (p-C)/B )               # MIC(predicted)

## Adjusted MIC
rf <- (0.8/rel.trat - 0.5) * sd(dat$xoc) * cor.trat.xoc
( mic.adj <- mic.pred - rf * p )


###############################################################

###### BOOTSTRAPPEN

nboot    <- 1000                # number of bootstrap samples
mic.roc  <- numeric(nboot)
mic.pred <- numeric(nboot)
mic.adj  <- numeric(nboot)
boot <- data.frame(mic.roc, mic.pred, mic.adj)

start.time <- Sys.time()

for(i in 1:nboot) {

  print(i)

  dat <- example[sample(1:dim(example)[1], dim(example)[1], replace=TRUE),]


  ### Estimate TR reliability with lavaan

  # model is already defined in the previous section

  fit <- cfa(model, data=dat, ordered=names(dat),
             test="mean.var.adjusted")

  pe <- parameterEstimates(fit, standardized=T, rsquare=T)
  ( rel.trat <- pe$est[pe$lhs=="trat" & pe$op=="r2"] )   # Reliability TR

  ### Estimate MICs

  xo1 <- rowSums(dat[,1:nitems])                 # sumscore T1
  xo2 <- rowSums(dat[,(nitems+1):(2*nitems)])    # sumscore T2
  dat$xoc <- xo2 - xo1

  ( q <- mean(dat$trat) )            # q = proportion improved (perceived)
  ( p <- log(q/(1-q)) )              # p = logodds(pre)

  ( cor.trat.xoc <- cor(dat$trat, dat$xoc) )  # correlation anchor-change

  ## Do ROC analysis

  rocobj <- roc(dat$trat, dat$xoc, quiet = TRUE)

  mic.roc <- coords(rocobj, x="best", input="threshold", ret="threshold",
                    best.method="youden", transpose = TRUE)

  ( boot$mic.roc[i] <- mic.roc[sample(length(mic.roc),1)] )


  ## Do logistic regression and calculate parameters and MIC(pred)

  mylogit <- glm(trat ~ xoc, data = dat, family = "binomial")

  C <- coef(mylogit)[1]                 # intercept coefficient C
  B <- coef(mylogit)[2]                 # regression coefficient B

  ( boot$mic.pred[i] <- (p-C)/B )               # MIC(predicted)

  ## Adjusted MIC
  rf <- (0.8/rel.trat - 0.5) * sd(dat$xoc) * cor.trat.xoc
  ( boot$mic.adj[i] <- boot$mic.pred[i] - rf * p )

}


end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

### NOTE: If you click on the R Console you can see how much bootstrap samples
### have been processed.

### Bootstrap results

round(mean(boot$mic.roc),1)
round(mean(boot$mic.pred),1)
round(mean(boot$mic.adj),1)

round(quantile(boot$mic.roc, c(0.025, 0.975)),1)
round(quantile(boot$mic.pred, c(0.025, 0.975)),1)
round(quantile(boot$mic.adj, c(0.025, 0.975)),1)


