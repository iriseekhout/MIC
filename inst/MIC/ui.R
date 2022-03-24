#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(dplyr)
library(ggplot2)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("MIC validation tool"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            numericInput("ns",
                         "Sample size",
                         value = 200),
            numericInput("nt",
                         "Simulated samples",
                         value = 1000),
            numericInput("mn.true.t1",
                         "Mean true baseline",
                         value = 40),
            numericInput("sd.true.t1",
                         "SD true baseline",
                         value = 10),
            numericInput("rel.t1",
                         "Reliability of baseline score",
                         value = 0.8,
                         min = 0,
                         max = 1),
            numericInput("mn.true.ch",
                         "Mean true change score",
                         value = 0),
            numericInput("sd.true.ch",
                         "SD true change score",
                         value = 10),
            numericInput("mn.imic",
                         "Mean iMIC (= genuine MIC)",
                         value = 5),
            numericInput("sd.imic",
                         "SD iMIC distribution",
                         value = 0.5),
            numericInput("cor.t1ch",
                         "Correlation between true and change scores",
                         value = -0.3,
                         min = -1,
                         max = 1),
            numericInput("rel.trat",
                         "Reliability of transition ratings",
                         value = 0.4,
                         min = 0,
                         max = 1),
            br(),
            actionButton("goButton", "Go!")



        ),

        # Show a plot of the generated distribution
        mainPanel(
            shinyjs::useShinyjs(),
            h1("Simulation tool for MIC estimation"),
            p("Attachment to: Terluin B, Eekhout I, Terwee CB, Improved adjusted minimal important change takes reliability of transition ratings into account. J Clin Epidemiol 2022."),
            p("With this tool you can replicate our simulations or perform simulations using your own simulation parameters. In the sidebar you can define your own parameters. The number of combinations of different parameters is infinite. You might want to try more extreme reliabilities or proportions improved and check how the adjusted MIC performs under these circumstances. Because you can simulate the genuine MIC, you can check whether the adjusted MIC is able to recover that MIC in terms of bias and precision. Note that you cannot directly simulate the proportion improved. The proportion improved depends on the values of the genuine MIC and the mean true change score. If the mean true change score is smaller than the genuine MIC the proportion improved will be <0.5; if the mean true change score is greater than the genuine MIC the proportion improved will be >0.5."),

            tags$div(em("Note, for estimating an MIC in your own data, you can use the formula in the paper, or the R-package MIC.")),
            br(),
            div(id = "startscreen",
                h3("Click GO! to start the simulations")
                ),
            div(id = "showoutput",
            h3("Proportion improved"),
            p("Mean of simulated samples and the 2.5th and 97.5th quantiles"),
            tableOutput("propimp"),
            h3("Reliability of transition ratings (TR)"),
            p("Mean of simulated samples and the 2.5th and 97.5th quantiles"),
            tableOutput("trat"),
            h3("Point biserial correlation between the transition ratings (TR) and the change score"),
            p("Mean of simulated samples and the 2.5th and 97.5th quantiles"),
            tableOutput("cortratoc"),
            h3("Estimated MIC values"),
            plotOutput("distPlot"),
            tableOutput("micestimates")
            )%>% shinyjs::hidden()
        )
    )
))
