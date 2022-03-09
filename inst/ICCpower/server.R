
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
                ggtitle( paste("ICC type =", input$method), subtitle = paste("| correlation:", input$correlation, "| variance:", input$variance, "| deviate raters:", input$systdif))
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

    output$widthsem <- renderPlot({
      ggplot(disdat(), aes(x = k, y = width_sem, group = n, color = n))+
        geom_line(size = 1) +
        ylab("95% CI width for SEM") +
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

            output$widthsem <- renderPlot({
              ggplot(disdat(), aes(x = k, y = width_sem, group = n, color = n))+
                geom_line(size = 1) +
                ylab("95% CI width for SEM") +
                xlab("Raters (k)")+
                theme(text = element_text(size = 16))+
                scale_color_manual(name = "Sample size (n)", values = ncolors)+
                ggtitle("SEM", subtitle = paste("correlation:", input$correlation, "| variance:", input$variance, "| deviate raters:", input$systdif))+
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

          output$widthsem <- renderPlot({
            ggplot(disdat(), aes(x = k, y = width_sem, group = n, color = n))+
              geom_line(size = 1) +
              ylab("95% CI width for SEM") +
              xlab("Raters (k)")+
              theme(text = element_text(size = 16))+
              scale_color_manual(name = "Sample size (n)", values = ncolors)+
              ggtitle("SEM",  subtitle = paste("ICC type:", input$method,  "| variance:", input$variance, "| deviate raters:", input$systdif))+
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

          output$widthsem <- renderPlot({
            ggplot(disdat(), aes(x = k, y = width_sem, group = n, color = n))+
              geom_line(size = 1) +
              ylab("95% CI width for SEM") +
              xlab("Raters (k)")+
              theme(text = element_text(size = 16))+
              scale_color_manual(name = "Sample size (n)", values = ncolors)+
              ggtitle("SEM",  subtitle = paste("ICC type:", input$method,  "| correlation:", input$correlation, "| deviate raters:", input$systdif))+
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

          output$widthsem <- renderPlot({
            ggplot(disdat(), aes(x = k, y = width_sem, group = n, color = n))+
              geom_line(size = 1) +
              ylab("95% CI width for SEM") +
              xlab("Raters (k)")+
              theme(text = element_text(size = 16))+
              scale_color_manual(name = "Sample size (n)", values = ncolors)+
              ggtitle("SEM",  subtitle = paste("ICC type:", input$method,  "| correlation:", input$correlation, "| variance:", input$variance))+
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
  output$startpage <- reactive({
      FALSE
      if(length(input$power) == 0) TRUE
  })
  outputOptions(output, "startpage", suspendWhenHidden = FALSE)

  #note: evaluation of "input$power# givens een warning (Warning: Error in if: argument is of length zero) by definition, because when length(input$power) == 0; then input$power == "CI lower" cannot be evaluated. >> not a problem for the app.
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
      if(input$icc_e < input$cilower){stop("The lower limit of the Confidence Interval cannot exceed the expected ICC.")}

      krange <- input$raterrange[1]:input$raterrange[2]


        n <- n_icc(k = krange, icc = input$icc_e, icc_lower = input$cilower, alpha = (1-input$alphalevel), beta = input$betapower)

        dat <- data.frame(raters = krange, n = n)

        p <- ggplot(dat, aes(x = raters, y = n)) +
            geom_bar(stat = "identity") +
            xlab("Number of repeated measurements (k)") +
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
            filter(method %in% !!input$method_iccrgw &
                    deviation %in% !!input$systdif_iccrgw &
                    cor %in% !!input$correlation_iccrgw &
                    variance %in% !!input$variance_iccrgw &
                    ciwidthm <= !!input$ciwidthw)
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

    ### for icc ----------------

    output$widthmap_icc <- renderPlotly({
    # Matrix format
        mat <- Agree::simoutput %>%
            filter(

                method %in% !!input$method_iccrgw &
                    deviation %in% !!input$systdif_iccrgw &
                    cor %in% !!input$correlation_iccrgw &
                    variance %in% !!input$variance_iccrgw) %>%
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
                ciwidthm = ifelse(ciwidthm >!!input$ciwidthw_icc, NA, ciwidthm)
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
                   xlab = "Repeated measurements", ylab = "Sample size",
                   main = paste("Width of Confidence interval for ICC type", paste(input$method_iccrgw, collapse = " & ")),
                   scale = "none",
                   margins = c(60,100,40,20),
                   grid_color = "white",
                   grid_width = 0.001,
                   titleX = TRUE,
                   hide_colorbar = TRUE,
                   branches_lwd = 0.1,
                   label_names = c("Sample size", "Repeated Measurements", "CI width"),
                   fontsize_row = 14, fontsize_col = 14,
                   labCol = colnames(mat),
                   labRow = rownames(mat),
                   heatmap_layers = theme(axis.line=element_blank())
    )
    p
    })

    ### for sem ----------------
    output$slider_ciwidthw_sem <- renderUI({
      sliderInput("ciwidthw_sem", "Target width of the 95% Confidence interval of SEM",
                  min=0,
                  max=round(sqrt(as.numeric(input$variance_iccrgw))/2,1),
                  value=sqrt(as.numeric(input$variance_iccrgw))/2*0.3,
                  step = 0.05)
    })
    output$widthmap_sem <- renderPlotly({
      # Matrix format
      mat <- Agree::simoutput %>%
        filter(

          method %in% !!input$method_iccrgw &
            deviation %in% !!input$systdif_iccrgw &
            cor %in% !!input$correlation_iccrgw &
            variance %in% !!input$variance_iccrgw) %>%
        # method %in% "agreement" &
        #     deviation %in% 0 &
        #     cor %in% 0.7 &
        #     variance %in% 1) %>%
        mutate(
          cent = 0,
          ciwidth = mean(width_icc),
          SE = sqrt(mse_sem),
          lower = cent - (1.96 * SE),
          upper = cent + (1.96 * SE),
          ciwidthm = upper - lower,
          ciwidthm = ifelse(ciwidthm > !!input$ciwidthw_sem , NA, ciwidthm)
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
                     xlab = "Repeated measurements", ylab = "Sample size",
                     main = paste("Width of Confidence interval for SEM type", paste(input$method_iccrgw, collapse = " & ")),
                     scale = "none",
                     margins = c(60,100,40,20),
                     grid_color = "white",
                     grid_width = 0.001,
                     titleX = TRUE,
                     hide_colorbar = TRUE,
                     branches_lwd = 0.1,
                     label_names = c("Sample size", "Repeated Measurements", "CI width"),
                     fontsize_row = 14, fontsize_col = 14,
                     labCol = colnames(mat),
                     labRow = rownames(mat),
                     heatmap_layers = theme(axis.line=element_blank())
      )
      p
    })


    ### MSE ratio --------------

      observe({ #all output is in this observe!
      if(input$design == "raters"){
       show("designk")
       hide("designn")

       k_iccr <- reactive(input$k_iccr)
       k_iccg <- reactive(input$k_iccg)
       n_iccr <- reactive(input$n_iccrg)
       n_iccg <- reactive(input$n_iccrg)

      }
      if(input$design == "sample size"){
        show("designn")
        hide("designk")

        k_iccr <- reactive(input$k_iccrg)
        k_iccg <- reactive(input$k_iccrg)
        n_iccr <- reactive(input$n_iccr)
        n_iccg <- reactive(input$n_iccg)

      }


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
                ciwidth = mean(width_icc),
                sem_e = mean(sem),
                sem = 0,
                mse_sem = mean(mse_sem),
                variance = mean(variance)
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
                ciwidth = mean(width_icc),
                sem_e = mean(sem),
                sem = 0,
                mse_sem = mean(mse_sem),
                variance = mean(variance)
            ) %>%
            mutate(scenario = "goal")


        scenario_icc <- bind_rows(ref, goal) %>%
            mutate(SE = sqrt(mse),
                   lower = icc - (1.96 * SE),
                   upper = icc + (1.96 * SE),
                   scenario = factor(scenario, levels = c("reference", "goal")),
                   mseratio = ref$mse/goal$mse,
                   statistic = "ICC",
                   yfact = 1)
        #added for sem
        scenario_sem <- bind_rows(ref, goal) %>%
          mutate(SE = sqrt(mse_sem),
                 lower = sem - (1.96 * SE),
                 upper = sem + (1.96 * SE),
                 scenario = factor(scenario, levels = c("reference", "goal")),
                 mseratio = ref$mse_sem/goal$mse_sem,
                 statistic = "SEM",
                 yfact = sqrt(variance))

        if(input$statistic == "ICC"){
          scenario <- scenario_icc}

        if(input$statistic == "SEM"){
          scenario <- scenario_sem}

    #})
        output$variableselection2 <- renderText({
          paste(
          paste("Input ICC:", input$correlation_iccrg),
          paste("Variance:", input$variance_iccrg),
          paste("Method:", input$method_iccrg),
          paste("Raters with deviation:", input$systdif_iccrg),
          paste("n_ref: ", n_iccr()),
          paste("n_goal: ", n_iccg()),
          paste("k_ref: ", k_iccr()),
          paste("k_goal: ", k_iccg()),
          paste("mse_ref: ", ref$mse),
          paste("mse_goal: ", goal$mse)

          )
        })

        #added for sem
        output$variableselection3 <- renderText({
          paste(
            paste("Input ICC:", input$correlation_iccrg),
            paste("Variance:", input$variance_iccrg),
            paste("Method:", input$method_iccrg),
            paste("Raters with deviation:", input$systdif_iccrg),
            paste("n_ref: ", n_iccr()),
            paste("n_goal: ", n_iccg()),
            paste("k_ref: ", k_iccr()),
            paste("k_goal: ", k_iccg()),
            paste("mse_ref_sem: ", ref$mse_sem),
            paste("mse_goal_sem: ", goal$mse_sem)

          )
        })

    output$mseratio_icc <- renderPlot({

    ggplot(scenario, aes(x = scenario, y = icc))+
        # geom_point() +
        geom_line(data = scenario, aes(x = scenario, y = lower, group = 1), lty = "dashed") +
        geom_line(data = scenario, aes(x = scenario, y = upper, group = 1), lty = "dashed") +
        geom_errorbar(aes( x = scenario, ymin = lower, ymax = upper), width = 0.2)+
        ylim(-(0.6*scenario$yfact[1]), (0.6*scenario$yfact[1])) +
        ylab(paste("Confidence interval for", scenario$statistic[1], sep = " ")) +
        annotate(geom= "text", label= paste("MSE ratio = ", round(scenario$mseratio[1],2)), x = 2.2, y = 0.45)



    })

    output$MSEratio <- renderText({
        if(input$design == "sample size"){
           statement <-  paste0("The MSE ratio is the required increase in repeated measurements to achieve the precision for the design with a sample size of ", input$n_iccg, " (indicated as target), with an actual sample size of ", input$n_iccr, " (indicated as adapted design). Accordingly, the number of repeated measurements (", input$k_iccrg, ") needs to be multiplied by ", round(scenario$mseratio[1],2), ", and change to ",round(as.numeric(input$k_iccrg) * scenario$mseratio[1]), " to have a precision close to the precision with a sample size of ", input$n_iccg,  ". When it is not possible to increase the number of repeated measurements accordingly, the precision as expected under the target design will decrease in the adapted design.")
        }
        if(input$design == "raters"){
           statement <-  paste0("The MSE ratio is the required sample size increase to achieve the precision for the design with ", input$k_iccg, " raters (indicated as target), with only ", input$k_iccr, " raters (indicated as adapted design). Accordingly, the sample size of ", input$n_iccrg, " needs to be multiplied by ", round(scenario$mseratio[1],2), " and change to ", round(as.numeric(input$n_iccrg) * scenario$mseratio[1]), " to have a precision close to the precision with ", input$k_iccg, " repeated measurements. When it is not possible to increase sample size accordingly, the precision as expected under the target design will decrease in the adapted design.")
        }

        statement
    })
    })



    # settings page ----

    # background page ----

})


