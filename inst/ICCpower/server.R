#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(MASS)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

    # simulation page ----
    nseq <- reactive({
        seq(from = input$n[1], to = input$n[2], length.out = input$nlength)
    })
    nseq <- seq(from = 10, to = 100, length.out = 5)

    kseq <- reactive({
        seq(from = input$k[1], to = input$k[2], by =1)
    })
    kseq <- seq(from = 2, to = 5, by = 1)

    mean = 0
    correlation = 0.7
    variance = 1

    output <- vector()
    for(ni in nseq){ #nseq = nseq()
        for(ki in kseq){ #kseq = kseq()
            for(i in 1:100){
                means <- rep(mean, ki) #mean = input$mean kr = kr()
                cov <- matrix(correlation,ki, ki) #correlation = input$correlation
                    # set up variance
                diag(cov) <- 1
                cov <- cov* variance #variance = input$variance

                dat <- as.data.frame(mvrnorm(means, cov, n=ni))

                measures <- Agree::icc(data = dat, var = TRUE)


                iteration <- c(measures["oneway",], measures["agreement",], measures["consistency",])

                iteration <- c(measures["oneway",], measures["agreement",], measures["consistency",])


                names(iteration) <- c(paste(names(iteration)[1:7], "oneway", sep = "_"),
                                      paste(names(iteration)[8:14], "agr", sep = "_"),
                                      paste(names(iteration)[15:21], "cons", sep = "_"))
                iteration <- c(set=i,n=ni, k=ki, iteration)


                output <- rbind(output, iteration) #stack each simulation iteration in rows (end of simulation run, 1000 rows)


            }
        }
    }
    # settings page ----

    # background page ----

})


## to do:
## use toggle hide show to show relevant pages at relevant times. - works now for D-score "results". adjust for question part to

