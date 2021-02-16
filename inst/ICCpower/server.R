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




    ## show results for MSE current contitions
    disdat <- reactive({

        output_mse %>%
        mutate(n = factor(n),
               k = factor(k)) %>%
        filter(cor == !!input$correlation &
               variance == !!input$variance &
               type == !!input$icc &
               sk == !!input$systdif)
    })

    output$variableselection <- renderText(
        paste("cor", input$correlation, "var", input$variance, "type", input$icc, "sk", input$systdif, "n", input$startn, "k", input$startk)
    )

    output$table <- renderDataTable(
        disdat()
    )

    output$simresulticc <- renderPlot({
        ggplot(disdat(), aes(x = k, y = icc, group = n, color = n))+
        geom_line() +
        ylab("MSE for ICC") +
        xlab("Raters (k)")+
        theme(text = element_text(size = 16))
    })

    output$simresultsem <-
        renderPlot({
            ggplot(disdat(), aes(x = k, y = sem, group = n, color = n))+
            geom_line() +
            ylab("MSE for SEM") +
            xlab("Raters (k)")+
                theme(text = element_text(size = 16))
        })


    ## calculate MSE ratio's with settings.
    ratdat_n <- reactive({
            disdat2 <- output_mse %>%
                filter(cor == !!input$correlation &
                           variance == !!input$variance &
                           type == !!input$icc &
                           sk == !!input$systdif) %>%
                filter(n == !!input$startn)

            iccref <- disdat2[disdat2$k == input$startk,"icc"]

            fratios <- as.numeric(unlist(iccref)/disdat2$icc)
            ks <- disdat2$k
            ns <- as.numeric(input$startn) * fratios
            res <- data.frame(k = ks, n = ns, start = ifelse(ns == as.numeric(input$startn), 1, 0))
            res
    })

    ratdat_k <- reactive({
        disdat <- output_mse %>%
            filter(cor == !!input$correlation & variance == !!input$variance &
                       type == !!input$icc & sk == !!input$systdif) %>%
        filter(k == !!input$startk)
        iccref <- disdat[disdat$n == input$startn,"icc"]

        fratios <- unlist(iccref)/disdat$icc
        ns <- disdat$n
        ks <- as.numeric(input$startk) * fratios
        res <- cbind(n = ns, k = ks , start = ifelse(ks == as.numeric(input$startk), 1, 0))
        data.frame(res)
    })

    output$ratdf_k <- renderDataTable(
        ratdat_n()
    )


    output$mseratio_k <- renderPlot({
         ggplot(ratdat_n(), aes(x = k, y = n, color = factor(start)))+
            geom_point(size = 3)+
            xlab("Raters (k)") + ylab("Sample size (n)") +
            scale_color_manual(values=c("#999999", "#E69F00"))+
            theme(legend.position = "none",
                  text = element_text(size = 16))+
             ggtitle("Required sample size increase",
                    subtitle = "Orange point indicates the current sample size.")#+
           # ggtitle("Required sample size increase when the precision of more raters wanted",
            #        subtitle = "Orange point indicates the current sample size.")
    })


    output$mseratio_n <- renderPlot({
        ggplot(ratdat_k(), aes(x = n, y = k, color = factor(start)))+
            geom_point(size = 3)+
            xlab("Sample size (n)") + ylab("Raters (k)") +
            scale_color_manual(values=c("#999999", "#E69F00"))+
            theme(legend.position = "none",
                  text = element_text(size = 16))+
            ggtitle("Required rater increase",
                            subtitle = "Orange point indicates the current number of raters.")#+
           # ggtitle("Required rater increase when the precision of higher sample size is wanted",
            #        subtitle = "Orange point indicates the current number of raters.")
    })





    # MSE ratios page ----




    # # simulation page ----
    # nseq <- reactive({
    #     seq(from = input$n[1], to = input$n[2], length.out = input$nlength)
    # })
    # nseq <- seq(from = 10, to = 100, length.out = 5)
    #
    # kseq <- reactive({
    #     seq(from = input$k[1], to = input$k[2], by =1)
    # })
    # kseq <- seq(from = 2, to = 5, by = 1)
    #
    # mean = 0
    # correlation = 0.7
    # variance = 1
    #
    # output <- vector()
    # for(ni in nseq){ #nseq = nseq()
    #     for(ki in kseq){ #kseq = kseq()
    #         for(i in 1:100){
    #             means <- rep(mean, ki) #mean = input$mean kr = kr()
    #             cov <- matrix(correlation,ki, ki) #correlation = input$correlation
    #                 # set up variance
    #             diag(cov) <- 1
    #             cov <- cov* variance #variance = input$variance
    #
    #             dat <- as.data.frame(mvrnorm(means, cov, n=ni))
    #
    #             measures <- Agree::icc(data = dat, var = TRUE)
    #
    #
    #             iteration <- c(measures["oneway",], measures["agreement",], measures["consistency",])
    #
    #             iteration <- c(measures["oneway",], measures["agreement",], measures["consistency",])
    #
    #
    #             names(iteration) <- c(paste(names(iteration)[1:7], "oneway", sep = "_"),
    #                                   paste(names(iteration)[8:14], "agr", sep = "_"),
    #                                   paste(names(iteration)[15:21], "cons", sep = "_"))
    #             iteration <- c(set=i,n=ni, k=ki, iteration)
    #
    #
    #             output <- rbind(output, iteration) #stack each simulation iteration in rows (end of simulation run, 1000 rows)
    #
    #
    #         }
    #     }
    # }
    # settings page ----

    # background page ----

})


## to do:
## use toggle hide show to show relevant pages at relevant times. - works now for D-score "results". adjust for question part to

