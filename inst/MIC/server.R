#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {


    #set.seed (12345)

    #start.time <- Sys.time()
     vals <- reactiveValues(outputdf = data.frame(prop.imp = 0, mic.roc = 0, mic.pred = 0, mic.adj = 0, rel.trat = 0))
    ## create variables that are to contain the outcomes of the simulations
    observeEvent(input$goButton, {
        shinyjs::show("showoutput")
        shinyjs::hide("startscreen")
#outputdf <- reactive({

    mn.xo1 <- numeric(input$nt)           # mean observed PRO score at T1
    sd.xo1 <- numeric(input$nt)           # SD observed PRO score at T1
    mn.xo2 <- numeric(input$nt)           # mean observed PRO score at T2
    sd.xo2 <- numeric(input$nt)           # SD observed PRO score at T2
    dt <- data.frame(mn.xo1, sd.xo1, mn.xo2, sd.xo2)   # creates dataframe

    dt$mn.xoc <- numeric(input$nt)        # mean observed change score
    dt$sd.xoc <- numeric(input$nt)        # sd observed change score
    dt$mn0.xoc <- numeric(input$nt)       # mean observed change score of not-improved
    # subjects
    dt$mn1.xoc <- numeric(input$nt)       # mean observed change score of improved
    # subjects
    dt$sd0.xoc <- numeric(input$nt)       # sd observed change score of not-improved
    # subjects
    dt$sd1.xoc <- numeric(input$nt)       # sd observed change score of improved
    # subjects
    dt$rel.xo1 <- numeric(input$nt)       # reliability observed PRO score T1
    dt$rel.xo2 <- numeric(input$nt)       # reliability observed PRO score T2
    dt$rel.xoc <- numeric(input$nt)       # reliability observed PRO change score
    dt$rel.trat <- numeric(input$nt)      # reliability of transition ratings (TR)
    dt$cor.trat.xoc <- numeric(input$nt)  # correlation between TR and observed PRO
    # change score
    dt$cor.xt1.xtc <- numeric(input$nt)   # correlation true PRO baseline and change
    # score
    dt$cor.xo1.xoc <- numeric(input$nt)   # correlation observed PRO baseline and
    # change score
    dt$prop.imp <- numeric(input$nt)      # proportion improved
    dt$logodds  <- numeric(input$nt)      # log-odds of improvement
    dt$true.mic <- numeric(input$nt)      # mean iMIC = genuine MIC
    dt$sd.imic <- numeric(input$nt)       # sd of iMIC
    dt$mic.roc <- numeric(input$nt)       # MIC(ROC-based)
    dt$mic.pred <- numeric(input$nt)      # MIC(predictive)
    dt$mic.adj <- numeric(input$nt)       # MIC(revised adjusted)
    dt$residuals <- numeric(input$nt)     # genuine MIC minus MIC(revised adjusted)


    ## Create starting point for the dataframe to be use in the simulations

    xt1 <- numeric(input$ns)   # creates provisional true PRO measurement at T1
    xt2 <- numeric(input$ns)   # creates provisional true PRO measurement at T2
    xtc <- numeric(input$ns)   # creates provisional true change score variable
    df <- data.frame(xt1, xt2, xtc)     # creates dataframe


    ## Do the simulations

    for(i in 1:input$nt)                  {

        ## Simulate a dataset

        # create "true" PRO measurement at T1
        df$xt1 <- rnorm(n=input$ns, m=input$mn.true.t1, s=input$sd.true.t1)

        ## Create a true change score that is correlated with the true T1 score
        df$xtc <- input$cor.t1ch*(df$xt1 - mean(df$xt1)) +
            sqrt(1-input$cor.t1ch^2)*rnorm(input$ns, 0, sd(df$xt1))
        df$xtc <- df$xtc*input$sd.true.ch/sd(df$xtc) + input$mn.true.ch

        mean(df$xtc)
        sd(df$xtc)
        cor(df$xt1, df$xtc)

        # create "true" PRO measurement at T2
        df$xt2 <- df$xt1 + df$xtc

        # create observed PRO measurement at T1 by adding measurement error
        sd.xt.error <- sqrt(((1-input$rel.t1)/input$rel.t1)*sd(df$xt1)^2)
        df$xo1 <- round(df$xt1 + rnorm(n=input$ns, m=0, s=sd.xt.error))

        # create observed PRO measurement at T2 by adding measurement error
        df$xo2 <- round(df$xt2 + rnorm(n=input$ns, m=0, s=sd.xt.error))

        # create observed PRO change score with measurement error
        df$xoc <- df$xo2 - df$xo1

        # create individual MIC values
        df$imic <- rnorm(n=input$ns, m=input$mn.imic, s=input$sd.imic)

        # create dichotomous transition ratings
        sd.pc.error <- sqrt(((1-input$rel.trat)/input$rel.trat)*sd(df$xtc)^2)
        df$xtc.percv <- df$xtc + rnorm(n=input$ns, m=0, s=sd.pc.error)

        mean(df$xtc.percv)
        sd(df$xtc.percv)
        var(df$xtc)/var(df$xtc.percv)   # reliability of perceived change

        df$trat <- 0
        df$trat[df$xtc.percv > df$imic] <- 1
        mean(df$trat)     # proportion improved

        ## Do calculations and save values to variables

        q <- mean(df$trat)              # q = proportion improved
        dt$prop.imp[i]    <- q
        dt$logodds[i]     <- log(q/(1-q))
        dt$mn.xo1[i]      <- mean(df$xo1)
        dt$mn.xo2[i]      <- mean(df$xo2)
        dt$sd.xo1[i]      <- sd(df$xo1)
        dt$sd.xo2[i]      <- sd(df$xo2)
        dt$mn.xoc[i]      <- mean(df$xoc)
        dt$sd.xoc[i]      <- sd(df$xoc)
        dt$mn0.xoc[i]     <- mean(df[df$trat==0, ]$xoc)
        dt$mn1.xoc[i]     <- mean(df[df$trat==1, ]$xoc)
        dt$sd0.xoc[i]     <- sd(df[df$trat==0, ]$xoc)
        dt$sd1.xoc[i]     <- sd(df[df$trat==1, ]$xoc)
        dt$true.mic[i]    <- mean(df$imic)
        dt$sd.imic[i]     <- sd(df$imic)
        dt$cor.trat.xoc[i] <- cor(df$trat, df$xoc)
        dt$cor.xt1.xtc[i] <- cor(df$xt1, df$xtc)
        dt$cor.xo1.xoc[i] <- cor(df$xo1, df$xoc)
        dt$rel.xo1[i]     <- var(df$xt1)/var(df$xo1)
        dt$rel.xo2[i]     <- var(df$xt2)/var(df$xo2)
        dt$rel.xoc[i]     <- var(df$xtc)/var(df$xoc)
        dt$rel.trat[i]     <- var(df$xtc)/var(df$xtc.percv)




        ### HEEL SIMPEL NU OPGEZET! HIER GEBLEVEN 22-10-21

        ## Do ROC analysis

        rocobj <- roc(df$trat, df$xoc, quiet = TRUE)

        mic.roc <- coords(rocobj, x="best", input="threshold", ret="threshold",
                          best.method="youden", transpose = TRUE)
        dt$mic.roc[i] <- mic.roc[sample(length(mic.roc),1)]


        ## Do logistic regression and calculate parameters and MIC(pred)

        mylogit <- glm(trat ~ xoc, data = df, family = "binomial")

        C <- coef(mylogit)[1]                 # intercept coefficient C
        B <- coef(mylogit)[2]                 # regression coefficient B
        p <- log(q/(1-q))                     # p = logodds(pre)

        dt$mic.pred[i] <- (p-C)/B            # MIC(predicted)

        ## "old" adjusted MIC
        # cf <- 0.09 * dt$sd.xoc[i] + 0.103 * dt$sd.xoc[i] * dt$cor.trat.xoc[i]
        # dt$mic.adj[i] <- dt$mic.pred[i] - cf * dt$logodds[i]

        ## Revised adjusted MIC
        rf <- (0.8/dt$rel.trat[i] - 0.5) * dt$sd.xoc[i] * dt$cor.trat.xoc[i]
        ( dt$mic.adj[i] <- dt$mic.pred[i] - rf * p )

        dt$residuals[i]   <- dt$true.mic[i] - dt$mic.adj[i]

    }

    vals$outputdf <- dt
})



    ###### RESULTS

    # Mean proportion improve and 95% CI
output$propimp <- renderTable({
    data.frame(mean = round(mean(vals$outputdf$prop.imp),2),
    t(round(quantile(vals$outputdf$prop.imp, c(0.025, 0.975)),2)))
    })


output$cortratoc <- renderTable({
  data.frame(mean = round(mean(vals$outputdf$cor.trat.xoc),2),
             t(round(quantile(vals$outputdf$cor.trat.xoc, c(0.025, 0.975)),2)))
})

output$micestimates <- renderTable({
    # Mean ROC-based MIC and 95% CI
   microc <- c(mean = mean(vals$outputdf$mic.roc),
    quantile(vals$outputdf$mic.roc, c(0.025, 0.975)))

  micpred <- c(
    # Mean Predictive MIC and 95% CI
    mean =round(mean(vals$outputdf$mic.pred),1),
    round(quantile(vals$outputdf$mic.pred, c(0.025, 0.975)),1))
  micadj <- c(
    # Mean Adjusted MIC and 95% CI
    mean = round(mean(vals$outputdf$mic.adj),1),
    round(quantile(vals$outputdf$mic.adj, c(0.025, 0.975)),1))

  rbind(microc,micpred, micadj)
}, rownames = TRUE)


output$trat <- renderTable(
    data.frame(
    # Mean TR reliability and 95% CI
    mean = mean(vals$outputdf$rel.trat),
    t(quantile(vals$outputdf$rel.trat, c(0.025, 0.975))))
)

    ##################################

    ## If you want to see a summary of all variables of the samples:
    ## Run the next line after removing the #
    # summary(dt)

    ## If you want to inspect all variables of the samples:
    ## Run the next line after removing the #
    # fix(dt)

    ## If you want to save all variables of the samples:
    ## Run the next line after removing the # (adjust the disk name "G:/")
    # write.table(dt, file = "G:/sim.dat", sep = " ", row.names = F, col.names = T)






    output$distPlot <- renderPlot({
        ggplot(data = vals$outputdf) +
            geom_histogram(aes(mic.roc), fill = "blue", alpha = 0.25)+
            geom_histogram(aes(mic.pred), fill = "green", alpha = 0.25)+
            geom_histogram(aes(mic.adj), fill = "orange", alpha = 0.25)+
            xlab("MIC estimates")


        dtl <- vals$outputdf %>%
            pivot_longer(cols = c(mic.roc, mic.pred, mic.adj), names_to = "method", values_to = "MIC")

        ggplot(data = dtl) +
            geom_histogram(aes(MIC, group = method, fill = method, color = method), alpha = 0.4)+
            theme_light()
    })

})
