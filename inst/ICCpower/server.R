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



################################################################################
    #RESULTS
################################################################################
  #manually select condition in app
  observe({
    if(input$compare == "none"){
        shinyjs::show("methods_buttons")
        shinyjs::show("correlation_buttons")
        shinyjs::show("variance_buttons")
        shinyjs::show("systdif_buttons")
    ## show results for MSE current conditions
    disdat <- reactive({

        Agree::simoutput %>%
        filter(n != 25) %>%
        mutate(n = factor(n),
               k = as.numeric(k)) %>%
        dplyr::filter(cor == !!input$correlation &
               variance == !!input$variance &
               method == !!input$method &
               deviation == !!input$systdif)

        })

    output$variableselection <- renderText(
        paste("Input ICC:", input$correlation, "Variance:", input$variance, "Method:", input$method, "Raters with deviation:", input$systdif)
    )

    output$table <- renderDataTable(
        disdat()
    )

    ##change this for all graphs -- change ratercolor
    output$biasicc <- renderPlot({
        ggplot(disdat(), aes(x = k, y = bias_icc, group = n, color = n))+
            geom_line(size = 1) +
            ylab("Bias for ICC") +
            #ylim(-0.055,0.015)+
        xlab("Raters (k)")+
        theme(text = element_text(size = 16))+
            scale_color_manual(name = "Sample size (n)", values = ncolors)+
            ggtitle(paste("ICC type =", input$method), subtitle = paste("| correlation:", input$correlation, "| variance:", input$variance, "| deviate raters:", input$systdif))
    })

    output$biassem <-
        renderPlot({
            ggplot(disdat(), aes(x = k, y = bias_sem, group = n, color = n))+
                geom_line(size = 1) +
                ylab("Bias for SEM") +
                xlab("Raters (k)")+
                theme(text = element_text(size = 16))+
                scale_color_manual(name = "Sample size (n)", values = ncolors)+
                ggtitle(paste("ICC type =", input$method), subtitle = paste("| correlation:", input$correlation, "| variance:", input$variance, "| deviate raters:", input$systdif))
        })


    output$mseicc <- renderPlot({
        ggplot(disdat(), aes(x = k, y = mse_icc, group = n, color = n))+
            geom_line(size = 1) +
            ylab("MSE for ICC") +
            #ylim(0,0.055)+
            xlab("Raters (k)")+
            theme(text = element_text(size = 16))+
            scale_color_manual(name = "Sample size (n)", values = ncolors)+
            ggtitle(paste("ICC type =", input$method), subtitle = paste("| correlation:", input$correlation, "| variance:", input$variance, "| deviate raters:", input$systdif))
    })

    output$msesem <-
        renderPlot({
            ggplot(disdat(), aes(x = k, y = mse_sem, group = n, color = n))+
                geom_line(size = 1) +
                ylab("MSE for SEM") +
                xlab("Raters (k)")+
                theme(text = element_text(size = 16))+
                scale_color_manual(name = "Sample size (n)", values = ncolors)+
                ggtitle(paste("ICC type =", input$method), subtitle = paste("| correlation:", input$correlation, "| variance:", input$variance, "| deviate raters:", input$systdif))
        })


    output$covicc <- renderPlot({
        ggplot(disdat(), aes(x = k, y = cov_icc, group = n))+
            geom_point(aes(group = n, color = n), stroke = 2.5, size = 1.7, pch = 3)+
                geom_line(size = 0.8) +
            ylab("Coverage for ICC") +
            xlab("Raters (k)")+
            ylim(0.9,1)+
            theme(text = element_text(size = 16))+
            scale_color_manual(name = "Sample size (n)", values = ncolors)+
            ggtitle(paste("ICC type =", input$method), subtitle = paste("| correlation:", input$correlation, "| variance:", input$variance, "| deviate raters:", input$systdif))
    })

    output$widthicc <- renderPlot({
        ggplot(disdat(), aes(x = k, y = width_icc, group = n, color = n))+
            geom_line(size = 1) +
            ylab("95% CI width for ICC") +
            xlab("Raters (k)")+
            theme(text = element_text(size = 16))+
            scale_color_manual(name = "Sample size (n)", values = ncolors)+
            ggtitle(paste("ICC type =", input$method), subtitle = paste("| correlation:", input$correlation, "| variance:", input$variance, "| deviate raters:", input$systdif))
    })

    }
      })

  #compare between ICC methods
  observe({
        if(input$compare == "methods"){
            shinyjs::hide("methods_buttons")
            shinyjs::show("correlation_buttons")
            shinyjs::show("variance_buttons")
            shinyjs::show("systdif_buttons")
            ## show results for MSE current conditions with methods in the facets
            disdat <- reactive({

                Agree::simoutput %>%
                    filter(n != 25) %>%
                    mutate(n = factor(n),
                           k = as.numeric(k),
                           method = factor(method),
                           method = relevel(method, ref = "oneway")) %>%
                    dplyr::filter(cor == !!input$correlation &
                                      variance == !!input$variance &
                                      deviation == !!input$systdif)

            })

            output$variableselection <- renderText(
                paste("Input ICC:", input$correlation, "Variance:", input$variance, "Method: all", "Raters with deviation:", input$systdif)
            )

            output$table <- renderDataTable(
                disdat()
            )

            ##change this for all graphs -- change ratercolor
            output$biasicc <- renderPlot({
                ggplot(disdat(), aes(x = k, y = bias_icc, group = n, color = n))+
                    geom_line(size = 1) +
                    ylab("Bias for ICC") +
                   # ylim(-0.055,0.015)+
                    xlab("Raters (k)")+
                    theme(text = element_text(size = 16))+
                    scale_color_manual(name = "Sample size (n)", values = ncolors)+
                    ggtitle("ICC", subtitle = paste("correlation:", input$correlation, "| variance:", input$variance, "| deviate raters:", input$systdif))+
                    facet_wrap(~method)
            })

            output$biassem <-
                renderPlot({
                    ggplot(disdat(), aes(x = k, y = bias_sem, group = n, color = n))+
                        geom_line(size = 1) +
                        ylab("Bias for SEM") +
                        xlab("Raters (k)")+
                        theme(text = element_text(size = 16))+
                        scale_color_manual(name = "Sample size (n)", values = ncolors)+
                        ggtitle("ICC", subtitle = paste("correlation:", input$correlation, "| variance:", input$variance, "| deviate raters:", input$systdif))+
                        facet_wrap(~method)
                })


            output$mseicc <- renderPlot({
                ggplot(disdat(), aes(x = k, y = mse_icc, group = n, color = n))+
                    geom_line(size = 1) +
                    ylab("MSE for ICC") +
                    #ylim(0,0.055)+
                    xlab("Raters (k)")+
                    theme(text = element_text(size = 16))+
                    scale_color_manual(name = "Sample size (n)", values = ncolors)+
                    ggtitle("ICC", subtitle = paste("correlation:", input$correlation, "| variance:", input$variance, "| deviate raters:", input$systdif))+
                    facet_wrap(~method)

            })

            output$msesem <-
                renderPlot({
                    ggplot(disdat(), aes(x = k, y = mse_sem, group = n, color = n))+
                        geom_line(size = 1) +
                        ylab("MSE for SEM") +
                        xlab("Raters (k)")+
                        theme(text = element_text(size = 16))+
                        scale_color_manual(name = "Sample size (n)", values = ncolors)+
                        ggtitle("SEM", subtitle = paste("correlation:", input$correlation, "| variance:", input$variance, "| deviate raters:", input$systdif))+
                        facet_wrap(~method)
                })


            output$covicc <- renderPlot({
                ggplot(disdat(), aes(x = k, y = cov_icc, group = n))+
                    geom_point(aes(group = n, color = n), stroke = 2.5, size = 1.7,  pch = 3)+
                    geom_line(size = 0.8) +
                    ylab("Coverage for ICC") +
                    xlab("Raters (k)")+
                    ylim(0.9,1)+
                    theme(text = element_text(size = 16))+
                    scale_color_manual(name = "Sample size (n)", values = ncolors)+
                    ggtitle("ICC", subtitle = paste("correlation:", input$correlation, "| variance:", input$variance, "| deviate raters:", input$systdif))+
                    facet_wrap(~method)
            })

            output$widthicc <- renderPlot({
                ggplot(disdat(), aes(x = k, y = width_icc, group = n, color = n))+
                    geom_line(size = 1) +
                    ylab("95% CI width for ICC") +
                    xlab("Raters (k)")+
                    theme(text = element_text(size = 16))+
                    scale_color_manual(name = "Sample size (n)", values = ncolors)+
                    ggtitle("ICC", subtitle = paste("correlation:", input$correlation, "| variance:", input$variance, "| deviate raters:", input$systdif))+
                    facet_wrap(~method)
            })

        }
    })

  #compare between correlations
  observe({
      if(input$compare == "correlation"){
          shinyjs::show("methods_buttons")
          shinyjs::hide("correlation_buttons")
          shinyjs::show("variance_buttons")
          shinyjs::show("systdif_buttons")
          ## show results for MSE current conditions with methods in the facets
          disdat <- reactive({

              Agree::simoutput %>%
                  filter(n != 25) %>%
                  mutate(n = factor(n),
                         k = as.numeric(k)) %>%
                  dplyr::filter(variance == !!input$variance &
                                method == !!input$method &
                                deviation == !!input$systdif)

          })

          output$variableselection <- renderText(
              paste("Input ICC:", input$correlation, "Variance:", input$variance, "ICC type: all", "Raters with deviation:", input$systdif)
          )

          output$table <- renderDataTable(
              disdat()
          )

          ##change this for all graphs -- change ratercolor
          output$biasicc <- renderPlot({
              ggplot(disdat(), aes(x = k, y = bias_icc, group = n, color = n))+
                  geom_line(size = 1) +
                  ylab("Bias for ICC") +
                  ylim(-0.055,0.015)+
                  xlab("Raters (k)")+
                  theme(text = element_text(size = 16))+
                  scale_color_manual(name = "Sample size (n)", values = ncolors)+
                  ggtitle("ICC", subtitle = paste("ICC type:", input$method, "| variance:", input$variance, "| deviate raters:", input$systdif))+
                  facet_wrap(~cor)
          })

          output$biassem <-
              renderPlot({
                  ggplot(disdat(), aes(x = k, y = bias_sem, group = n, color = n))+
                      geom_line(size = 1) +
                      ylab("Bias for SEM") +
                      xlab("Raters (k)")+
                      theme(text = element_text(size = 16))+
                      scale_color_manual(name = "Sample size (n)", values = ncolors)+
                      ggtitle("ICC", subtitle = paste("ICC type:", input$method,  "| variance:", input$variance, "| deviate raters:", input$systdif))+
                      facet_wrap(~cor)
              })


          output$mseicc <- renderPlot({
              ggplot(disdat(), aes(x = k, y = mse_icc, group = n, color = n))+
                  geom_line(size = 1) +
                  ylab("MSE for ICC") +
                  #ylim(0,0.055)+
                  xlab("Raters (k)")+
                  theme(text = element_text(size = 16))+
                  scale_color_manual(name = "Sample size (n)", values = ncolors)+
                  ggtitle("ICC", subtitle = paste("ICC type:", input$method,  "| variance:", input$variance, "| deviate raters:", input$systdif))+
                  facet_wrap(~cor)

          })

          output$msesem <-
              renderPlot({
                  ggplot(disdat(), aes(x = k, y = mse_sem, group = n, color = n))+
                      geom_line(size = 1) +
                      ylab("MSE for SEM") +
                      xlab("Raters (k)")+
                      theme(text = element_text(size = 16))+
                      scale_color_manual(name = "Sample size (n)", values = ncolors)+
                      ggtitle("SEM", subtitle = paste("ICC type:", input$method,  "| variance:", input$variance, "| deviate raters:", input$systdif))+
                      facet_wrap(~cor)
              })


          output$covicc <- renderPlot({
              ggplot(disdat(), aes(x = k, y = cov_icc, group = n))+
                  geom_point(aes(group = n, color = n), stroke = 2.5, size = 1.7,  pch = 3)+
                  geom_line(size = 0.8) +
                  ylab("Coverage for ICC") +
                  xlab("Raters (k)")+
                  ylim(0.9,1)+
                  theme(text = element_text(size = 16))+
                  scale_color_manual(name = "Sample size (n)", values = ncolors)+
                  ggtitle("ICC", subtitle = paste("ICC type:", input$method,  "| variance:", input$variance, "| deviate raters:", input$systdif))+
                  facet_wrap(~cor)
          })

          output$widthicc <- renderPlot({
              ggplot(disdat(), aes(x = k, y = width_icc, group = n, color = n))+
                  geom_line(size = 1) +
                  ylab("95% CI width for ICC") +
                  xlab("Raters (k)")+
                  theme(text = element_text(size = 16))+
                  scale_color_manual(name = "Sample size (n)", values = ncolors)+
                  ggtitle("ICC", subtitle = paste("ICC type:", input$method,  "| variance:", input$variance, "| deviate raters:", input$systdif))+
                  facet_wrap(~cor)
          })

      }
  })

  #compare between variance
  observe({
      if(input$compare == "variance"){
          shinyjs::show("methods_buttons")
          shinyjs::show("correlation_buttons")
          shinyjs::hide("variance_buttons")
          shinyjs::show("systdif_buttons")
          ## show results for MSE current conditions with methods in the facets
          disdat <- reactive({

              Agree::simoutput %>%
                  filter(n != 25) %>%
                  mutate(n = factor(n),
                         k = as.numeric(k)) %>%
                  dplyr::filter(cor == !!input$correlation &
                                method == !!input$method &
                                deviation == !!input$systdif)

          })

          output$variableselection <- renderText(
              paste("Input ICC:", input$correlation, "Variance:", input$variance, "ICC type: all", "Raters with deviation:", input$systdif)
          )

          output$table <- renderDataTable(
              disdat()
          )

          ##change this for all graphs -- change ratercolor
          output$biasicc <- renderPlot({
              ggplot(disdat(), aes(x = k, y = bias_icc, group = n, color = n))+
                  geom_line(size = 1) +
                  ylab("Bias for ICC") +
                #  ylim(-0.055,0.015)+
                  xlab("Raters (k)")+
                  theme(text = element_text(size = 16))+
                  scale_color_manual(name = "Sample size (n)", values = ncolors)+
                  ggtitle("ICC", subtitle = paste("ICC type:", input$method, "| correlation:", input$correlation, "| deviate raters:", input$systdif))+
                  facet_wrap(~variance)
          })

          output$biassem <-
              renderPlot({
                  ggplot(disdat(), aes(x = k, y = bias_sem, group = n, color = n))+
                      geom_line(size = 1) +
                      ylab("Bias for SEM") +
                      xlab("Raters (k)")+
                      theme(text = element_text(size = 16))+
                      scale_color_manual(name = "Sample size (n)", values = ncolors)+
                      ggtitle("ICC", subtitle = paste("ICC type:", input$method,  "| correlation:", input$correlation, "| deviate raters:", input$systdif))+
                      facet_wrap(~variance)
              })


          output$mseicc <- renderPlot({
              ggplot(disdat(), aes(x = k, y = mse_icc, group = n, color = n))+
                  geom_line(size = 1) +
                  ylab("MSE for ICC") +
                  #ylim(0,0.055)+
                  xlab("Raters (k)")+
                  theme(text = element_text(size = 16))+
                  scale_color_manual(name = "Sample size (n)", values = ncolors)+
                  ggtitle("ICC", subtitle = paste("ICC type:", input$method,  "| correlation:", input$correlation, "| deviate raters:", input$systdif))+
                  facet_wrap(~variance)

          })

          output$msesem <-
              renderPlot({
                  ggplot(disdat(), aes(x = k, y = mse_sem, group = n, color = n))+
                      geom_line(size = 1) +
                      ylab("MSE for SEM") +
                      xlab("Raters (k)")+
                      theme(text = element_text(size = 16))+
                      scale_color_manual(name = "Sample size (n)", values = ncolors)+
                      ggtitle("SEM", subtitle = paste("ICC type:", input$method,  "| correlation:", input$correlation, "| deviate raters:", input$systdif))+
                      facet_wrap(~variance)
              })


          output$covicc <- renderPlot({
              ggplot(disdat(), aes(x = k, y = cov_icc, group = n))+
                  geom_point(aes(group = n, color = n), stroke = 2.5, size = 1.7,  pch = 3)+
                  geom_line(size = 0.8) +
                  ylab("Coverage for ICC") +
                  xlab("Raters (k)")+
                  ylim(0.9,1)+
                  theme(text = element_text(size = 16))+
                  scale_color_manual(name = "Sample size (n)", values = ncolors)+
                  ggtitle("ICC", subtitle = paste("ICC type:", input$method,  "| correlation:", input$correlation, "| deviate raters:", input$systdif))+
                  facet_wrap(~variance)
          })

          output$widthicc <- renderPlot({
              ggplot(disdat(), aes(x = k, y = width_icc, group = n, color = n))+
                  geom_line(size = 1) +
                  ylab("95% CI width for ICC") +
                  xlab("Raters (k)")+
                  theme(text = element_text(size = 16))+
                  scale_color_manual(name = "Sample size (n)", values = ncolors)+
                  ggtitle("ICC", subtitle = paste("ICC type:", input$method,  "| correlation:", input$correlation, "| deviate raters:", input$systdif))+
                  facet_wrap(~variance)
          })

      }
  })

  #compare between deviation
  observe({
      if(input$compare == "deviation"){
          shinyjs::show("methods_buttons")
          shinyjs::show("correlation_buttons")
          shinyjs::show("variance_buttons")
          shinyjs::hide("systdif_buttons")
          ## show results for MSE current conditions with methods in the facets
          disdat <- reactive({

              Agree::simoutput %>%
                  filter(n != 25) %>%
                  mutate(n = factor(n),
                         k = as.numeric(k)) %>%
                  dplyr::filter(cor == !!input$correlation &
                                    method == !!input$method &
                                    variance == !!input$variance)

          })

          output$variableselection <- renderText(
              paste("Input ICC:", input$correlation, "Variance:", input$variance, "ICC type: all", "Raters with deviation:", input$systdif)
          )

          output$table <- renderDataTable(
              disdat()
          )

          ##change this for all graphs -- change ratercolor
          output$biasicc <- renderPlot({
              ggplot(disdat(), aes(x = k, y = bias_icc, group = n, color = n))+
                  geom_line(size = 1) +
                  ylab("Bias for ICC") +
                  #ylim(-0.055,0.015)+
                  xlab("Raters (k)")+
                  theme(text = element_text(size = 16))+
                  scale_color_manual(name = "Sample size (n)", values = ncolors)+
                  ggtitle("ICC per diviation condition", subtitle = paste("ICC type:", input$method, "| correlation:", input$correlation, "| variance:", input$variance))+
                  facet_wrap(~deviation)
          })

          output$biassem <-
              renderPlot({
                  ggplot(disdat(), aes(x = k, y = bias_sem, group = n, color = n))+
                      geom_line(size = 1) +
                      ylab("Bias for SEM") +
                      xlab("Raters (k)")+
                      theme(text = element_text(size = 16))+
                      scale_color_manual(name = "Sample size (n)", values = ncolors)+
                      ggtitle("ICC", subtitle = paste("ICC type:", input$method,  "| correlation:", input$correlation, "| variance:", input$variance))+
                      facet_wrap(~deviation)
              })


          output$mseicc <- renderPlot({
              ggplot(disdat(), aes(x = k, y = mse_icc, group = n, color = n))+
                  geom_line(size = 1) +
                  ylab("MSE for ICC") +
                  #ylim(0,0.055)+
                  xlab("Raters (k)")+
                  theme(text = element_text(size = 16))+
                  scale_color_manual(name = "Sample size (n)", values = ncolors)+
                  ggtitle("ICC", subtitle = paste("ICC type:", input$method,  "| correlation:", input$correlation, "| variance:", input$variance))+
                  facet_wrap(~deviation)

          })

          output$msesem <-
              renderPlot({
                  ggplot(disdat(), aes(x = k, y = mse_sem, group = n, color = n))+
                      geom_line(size = 1) +
                      ylab("MSE for SEM") +
                      xlab("Raters (k)")+
                      theme(text = element_text(size = 16))+
                      scale_color_manual(name = "Sample size (n)", values = ncolors)+
                      ggtitle("SEM", subtitle = paste("ICC type:", input$method,  "| correlation:", input$correlation, "| variance:", input$variance))+
                      facet_wrap(~deviation)
              })


          output$covicc <- renderPlot({
              ggplot(disdat(), aes(x = k, y = cov_icc, group = n))+
                  geom_point(aes(group = n, color = n), stroke = 2.5, size = 1.7,  pch = 3)+
                  geom_line(size = 1) +
                  ylab("Coverage for ICC") +
                  xlab("Raters (k)")+
                  ylim(0.9,1)+
                  theme(text = element_text(size = 16))+
                  scale_color_manual(name = "Sample size (n)", values = ncolors)+
                  ggtitle("ICC", subtitle = paste("ICC type:", input$method,  "| correlation:", input$correlation, "| variance:", input$variance))+
                  facet_wrap(~deviation)
          })

          output$widthicc <- renderPlot({
              ggplot(disdat(), aes(x = k, y = width_icc, group = n, color = n))+
                  geom_line(size = 1) +
                  ylab("95% CI width for ICC") +
                  xlab("Raters (k)")+
                  theme(text = element_text(size = 16))+
                  scale_color_manual(name = "Sample size (n)", values = ncolors)+
                  ggtitle("ICC", subtitle = paste("ICC type:", input$method,  "| correlation:", input$correlation, "| variance:", input$variance))+
                  facet_wrap(~deviation)
          })

      }
  })


  ################################################################################
  #Choice assistant
  ################################################################################
    # MSE ratios page ----

    #scenario 1: icc agreement is about 0.7. Currently have 20 pts rated by 3 raters; n of 50 is recommended, how many additional raters to I need?

    # SE = sd/sqrt(n)


    # scenario 3: icc agreement is about 0.7, currently we have 30 patients rated by 4 raters, how close are we to the 3 raters; n = 50 situation - will we need more raters or patients?
    output$powercilow <- reactive({
        FALSE
        if(length(input$power) > 0 & input$power == "CI lower") TRUE
    })
    outputOptions(output, "powercilow", suspendWhenHidden = FALSE)

    output$powerci <- reactive({
        FALSE
        if(length(input$power) > 0 & input$power == "CI width") TRUE
    })
    outputOptions(output, "powerci", suspendWhenHidden = FALSE)

    output$powermse <- reactive({
        FALSE
        if(length(input$power) > 0 & input$power == "MSE ratio") TRUE
    })
    outputOptions(output, "powermse", suspendWhenHidden = FALSE)


    ### CI lower ----

    output$n_icc_plot <- renderPlotly({
        krange <- input$raterrange[1]:input$raterrange[2]

        n <- n_icc(k = krange, icc = input$icc_e, icc_lower = input$cilower)

        dat <- data.frame(raters = krange, n = n)

        p <- ggplot(dat, aes(x = raters, y = n)) +
            geom_bar(stat = "identity") +
            xlab("Number of raters (k)") +
            ylab('Sample size (n)')
        ggplotly(p)
    })

    ### CI width -------
    refwidth_icc <- reactive({
        out <- Agree::simoutput %>%
            mutate(
                cent = 0,
                ciwidth = mean(width_icc),
                SE = sqrt(mse_icc),
                lower = cent - (1.96 * SE),
                upper = cent + (1.96 * SE),
                ciwidthm = upper - lower
            )


        scenario <- out %>%
            filter(method %in% !!input$method_iccrg &
                    deviation %in% !!input$systdif_iccrg &
                    cor %in% !!input$correlation_iccrg &
                    variance %in% !!input$variance_iccrg &
                    ciwidthm <= !!input$ciwidth)
        scenario

    })
   #simple dotplot
    output$widthcond <- renderPlot({
        ggplot(refwidth_icc(), aes(x = k, y = k, color = ciwidthm))+
            geom_point(size = 3) +
            xlim(0,200) +
            ylim(0,6) +
            xlab("Sample size (n)")+
            ylab("Raters (k)")+
            ggtitle("Combinations of sample size and raters with required CI width.")
    })
     #plotly heatmap
    output$widthmap <- renderPlotly({
    # Matrix format
        mat <- Agree::simoutput %>%
            filter(

                method %in% !!input$method_iccrg &
                    deviation %in% !!input$systdif_iccrg &
                    cor %in% !!input$correlation_iccrg &
                    variance %in% !!input$variance_iccrg) %>%
            # method %in% "agreement" &
            #     deviation %in% 0 &
            #     cor %in% 0.7 &
            #     variance %in% 1) %>%
            mutate(
                cent = 0,
                ciwidth = mean(width_icc),
                SE = sqrt(mse_icc),
                lower = cent - (1.96 * SE),
                upper = cent + (1.96 * SE),
                ciwidthm = upper - lower,
                ciwidthm = ifelse(ciwidthm >!!input$ciwidth, NA, ciwidthm)
            ) %>% dplyr::select(n, k, ciwidthm) %>%
            group_by(n, k) %>%
            summarise(ciwidthm = mean(ciwidthm, na.rm = TRUE),
                      .groups = "drop") %>%
            pivot_wider(id_cols = n, names_from = k, values_from = ciwidthm) %>%
            dplyr::select(n, everything()) %>% as.data.frame()
        rownames(mat) <- mat$n
        mat <- mat %>% dplyr::select(-n)
        mat <- as.matrix(mat)


    p <- heatmaply(mat,
                   dendrogram = "none",
                   xlab = "Raters", ylab = "Sample size",
                   main = "Width for Confidence interval of ICC for sample size and rater combinations",
                   scale = "none",
                   margins = c(60,100,40,20),
                   grid_color = "white",
                   grid_width = 0.001,
                   titleX = FALSE,
                   hide_colorbar = TRUE,
                   branches_lwd = 0.1,
                   label_names = c("Sample size", "Raters", "CI width"),
                   fontsize_row = 14, fontsize_col = 14,
                   labCol = colnames(mat),
                   labRow = rownames(mat),
                   heatmap_layers = theme(axis.line=element_blank())
    )
    p
    })

    ### MSE ratio --------------
    output$designk <- reactive({
        FALSE
        if(length(input$design) > 0 & input$design == "raters") TRUE
    })
    outputOptions(output, "designk", suspendWhenHidden = FALSE)

    output$designn <- reactive({
        FALSE
        if(length(input$design) > 0 & input$design == "sample size") TRUE
    })
    outputOptions(output, "designn", suspendWhenHidden = FALSE)


    k_iccr <- reactive({
        if(input$design != "raters"){
            k_icc <- input$k_iccrg
        }
        if(input$design == "raters"){
            k_icc <- input$k_iccr
        }
        k_icc
    })
    k_iccg <- reactive({
        if(input$design != "raters"){
            k_icc <- input$k_iccrg
        }
        if(input$design == "raters"){
            k_icc <- input$k_iccg
        }
        k_icc
    })
    n_iccr <- reactive({
        if(input$design != "sample size"){
            n_icc <- input$n_iccrg
        }
        if(input$design == "sample size"){
            n_icc <- input$n_iccr
        }
        n_icc
    })
    n_iccg <- reactive({
        if(input$design != "sample size"){
            n_icc <- input$n_iccrg
        }
        if(input$design == "sample size"){
            n_icc <- input$n_iccg
        }
        n_icc
    })

    scenario_icc <- reactive({
         ref <- Agree::simoutput %>%
            filter(method %in% !!input$method_iccrg &
                       k %in% !!k_iccr() &
                       n %in% !!n_iccr() &
                       deviation %in% !!input$systdif_iccrg &
                       cor %in% !!input$correlation_iccrg &
                       variance %in% !!input$variance_iccrg) %>%
            summarise(
                icc_e = mean(icc),
                icc = 0,
                mse = mean(mse_icc),
                ciwidth = mean(width_icc)
            ) %>%
            mutate(scenario = "reference")

        goal <-
            Agree::simoutput %>%
            filter(method %in% !!input$method_iccrg &
                       k %in% !!k_iccg() &
                       n %in% !!n_iccg() &
                       deviation %in% !!input$systdif_iccrg &
                       cor %in% !!input$correlation_iccrg &
                       variance %in% !!input$variance_iccrg) %>%
            summarise(
                icc_e = mean(icc),
                icc = 0,
                mse = mean(mse_icc),
                ciwidth = mean(width_icc)
            ) %>%
            mutate(scenario = "goal")


        scenario <- bind_rows(ref, goal) %>%
            mutate(SE = sqrt(mse),
                   lower = icc - (1.96 * SE),
                   upper = icc + (1.96 * SE),
                   scenario = factor(scenario, levels = c("reference", "goal")),
                   mseratio = ref$mse/goal$mse)

        scenario
    })

    output$mseratio_icc <- renderPlot({
    ggplot(scenario_icc(), aes(x = scenario, y = icc))+
        # geom_point() +
        geom_line(data = scenario_icc(), aes(x = scenario, y = lower, group = 1), lty = "dashed") +
        geom_line(data = scenario_icc(), aes(x = scenario, y = upper, group = 1), lty = "dashed") +
        geom_errorbar(aes( x = scenario, ymin = lower, ymax = upper), width = 0.2)+
        ylim(-0.5, 0.5) + ylab("Confidence interval for ICC") +
        annotate(geom= "text", label= paste("MSE ratio = ", round(scenario_icc()$mseratio[1],2)), x = 2.2, y = 0.45)
    })

    output$MSEratio <- renderText({
        if(input$design == "sample size"){
           statement <-  paste0("The MSE ratio is the required increase in number of raters to achieve the precision for the design with a sample size of ", input$n_iccg, " (indicated as goal), with an actual sample size of ", input$n_iccr, " (reference). Accordingly, the number of raters (", input$k_iccrg, ") needs to be multiplied by ", round(scenario_icc()$mseratio[1],2), ", and change to ",round(as.numeric(input$k_iccrg) * scenario_icc()$mseratio[1]), " to have a precision close to the precision with a sample size of ", input$n_iccg)
        }
        if(input$design == "raters"){
           statement <-  paste0("The MSE ratio is the required sample size increase to achieve the precision for the design with ", input$k_iccg, " raters (indicated as goal), with only ", input$k_iccr, " raters. Accordingly, the sample size of ", input$n_iccrg, " needs to be multiplied by ", round(scenario_icc()$mseratio[1],2), " and change to ", round(as.numeric(input$n_iccrg) * scenario_icc()$mseratio[1]), " to have a precision close to the precision with ", input$k_iccg, " raters.")
        }
        statement
    })

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


#
#
# ### old code for MSE ratios.
# ## calculate MSE ratio's with settings.
# ratdat_n <- reactive({
#     disdat2 <- output_mse %>%
#         filter(cor == !!input$correlation &
#                    variance == !!input$variance &
#                    type == !!input$icc &
#                    sk == !!input$systdif) %>%
#         filter(n == !!input$startn)
#
#     iccref <- disdat2[disdat2$k == input$startk,"icc"]
#
#     fratios <- as.numeric(unlist(iccref)/disdat2$icc)
#     ks <- disdat2$k
#     ns <- as.numeric(input$startn) * fratios
#     res <- data.frame(k = ks, n = ns, start = ifelse(ns == as.numeric(input$startn), 1, 0))
#     res
# })
#
# ratdat_k <- reactive({
#     disdat <- output_mse %>%
#         filter(cor == !!input$correlation & variance == !!input$variance &
#                    type == !!input$icc & sk == !!input$systdif) %>%
#         filter(k == !!input$startk)
#     iccref <- disdat[disdat$n == input$startn,"icc"]
#
#     fratios <- unlist(iccref)/disdat$icc
#     ns <- disdat$n
#     ks <- as.numeric(input$startk) * fratios
#     res <- cbind(n = ns, k = ks , start = ifelse(ks == as.numeric(input$startk), 1, 0))
#     data.frame(res)
# })
#
# output$ratdf_k <- renderDataTable(
#     ratdat_n()
# )
#
#
# output$mseratio_k <- renderPlot({
#     ggplot(ratdat_n(), aes(x = k, y = n, color = factor(start)))+
#         geom_point(size = 3)+
#         xlab("Raters (k)") + ylab("Sample size (n)") +
#         scale_color_manual(values=c("#999999", "#E69F00"))+
#         theme(legend.position = "none",
#               text = element_text(size = 16))+
#         ggtitle("Required sample size increase",
#                 subtitle = "Orange point indicates the current sample size.")#+
#     # ggtitle("Required sample size increase when the precision of more raters wanted",
#     #        subtitle = "Orange point indicates the current sample size.")
# })
#
#
# output$mseratio_n <- renderPlot({
#     ggplot(ratdat_k(), aes(x = k, y = k, color = factor(start)))+
#         geom_point(size = 3)+
#         xlab("Sample size (n)") + ylab("Raters (k)") +
#         scale_color_manual(values=c("#999999", "#E69F00"))+
#         theme(legend.position = "none",
#               text = element_text(size = 16))+
#         ggtitle("Required rater increase",
#                 subtitle = "Orange point indicates the current number of raters.")#+
#     # ggtitle("Required rater increase when the precision of higher sample size is wanted",
#     #        subtitle = "Orange point indicates the current number of raters.")
# })
#
