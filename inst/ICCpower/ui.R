#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#



# Define UI for application that draws a histogram
ui <- navbarPage(
    theme = shinytheme("cerulean"), collapsible = TRUE,
    "ICC power      ",
    tabPanel("Simulation",
             sidebarLayout(
            # MSE ratio's paper
                 sidebarPanel( #shinyjs::useShinyjs(),
                     tags$h2("Study Design"),
                     tags$p("Select study design options"),
                     radioButtons("icc",
                                  label = "Type of ICC",
                                  choices = c("oneway", "agr", "cons")),
                     radioButtons("correlation",
                                  label = "Expected correlation between raters",
                                  choices = c(0.6, 0.7, 0.8),
                                  selected = 0.7),
                     radioButtons("variance",
                                  label = "Expected variance in scores",
                                  choices = c(1, 10, 100)),
                     radioButtons("systdif",
                                  label = "1 or 2 raters with systematic differences",
                                  choices = c(1,2)),
                     radioButtons("startn",
                                 label = "Sample size to start with",
                                 choices = c(10,25,50,100,200)),

                     radioButtons("startk",
                                  label = "Raters to start with",
                                  choices = c(2,3,4,5,6)),
                     #              div(id = "main_start0",
                     #     textOutput("agemos")
                     # )%>% shinyjs::hidden(),
                     # #tags$br(),
                     #actionButton("start", "Start"),
                     #tags$br(),
                     #tags$br()
                     width = 3),

                 mainPanel(
                     #textOutput("variableselection"),
                     #dataTableOutput("ratdf_k"),
                     fluidRow(
                         box(width = 12,
                             h2("Visualisation of precision of ICC and SEM"),
                             p("The plots below show the precision in terms of mean squared error (MSE) for different combinations of sample size (n) and number of raters (k)."))
                     ),
                     fluidRow(
                         shinydashboard::box(plotOutput("simresulticc")),
                         shinydashboard::box(plotOutput("simresultsem"))
                     ),
                     fluidRow(
                       box(width = 12,
                           h2("Visualisation of MSE ratio's for ICC"),
                           p("The plots below show the required increase in sample size (left) or number of raters(right), when a similar precision of a higher number of raters (left) or sample size (right) is wanted."))
                     ),
                     fluidRow(
                         shinydashboard::box(plotOutput("mseratio_n")),
                         shinydashboard::box(plotOutput("mseratio_k"))
                     )
                 )
             )
    ),
    tabPanel("Background"),
    tabPanel("About")
    #     #simulation page ----
    # tabPanel("Simulate",
    #          sidebarLayout(
    #
    #              sidebarPanel( #shinyjs::useShinyjs(),
    #                            tags$h2("Study Design"),
    #                            tags$p("Select study design options"),
    #                            radioButtons("icc",
    #                                      label = "Type of ICC",
    #                                      choices = c("ICConeway", "ICCagreement", "ICCconsistency")),
    #                            numericInput("correlation",
    #                                         label = "Expected correlation between raters",
    #                                         value = 0.7,
    #                                         min = 0, max = 1),
    #                            numericInput("variance",
    #                                         label = "Expected variance in scores",
    #                                         value = 1),
    #                            numericInput("mean",
    #                                         label = "Expected mean of score",
    #                                         value = 1,
    #                                         ),
    #                            sliderInput("k",
    #                                        label = "number of raters",
    #                                        min = 1, max = 20,
    #                                        value = c(2, 10),
    #                                        step = 1),
    #                            sliderInput("n",
    #                                        label = "sample size",
    #                                        min = 0, max = 1000,
    #                                        value = c(10, 200)),
    #                            numericInput("nlength",
    #                                         "steps in sample size sequence")
    #
    #                           div(id = "main_start0",
    #                                textOutput("agemos")
    #                            )%>% shinyjs::hidden(),
    #                            #tags$br(),
    #                            #actionButton("start", "Start"),
    #                            #tags$br(),
    #                            #tags$br()
    #                            width = 3),
    #
    #              mainPanel(
    #
    #          )
    # ),
    #result page ----
    # tabPanel("D-score",
    #          div(id = "results",
    #              sidebarLayout(
    #                  sidebarPanel(
    #                      tableOutput("checkanswers")
    #
    #                  ),
    #                  mainPanel(
    #                      fluidRow(
    #                          plotOutput("scoreplot"),
    #                          box(width = 6,
    #                              tableOutput("scoretab"),
    #                          )),
    #                      fluidRow(
    #                          downloadButton(
    #                              outputId = "downloadReport",
    #                              label = "Download Report"
    #                          ))
    #                  )
    #                  #add download button to download results
    #              )
    #          ) %>% shinyjs::hidden()
    # ),
    # #settings page ----
    # tabPanel("Settings",
    #          ##add choice of itembank
    #          fluidRow(
    #              box(width = 12,
    #                  tags$h3("Itembank selection"),
    #                  tags$p("Select one of the available itembanks in the box below. The GSED LF is the GSED long form database, with directly observed items. The GSED SF is the GSED short form, with caregiver reported items. The GCGD 165 is an itembank with 165, mostly directly observed, items selected by the Global Child Development Group. Van Wiechen is the Dutch Developmental Inventory (Dutch questions)"),
    #                  selectInput("itembank",
    #                              label = "Select itembank",
    #                              choices = c("GSED LF",
    #                                          "GSED SF",
    #                                          "GCDG 165",
    #                                          "Van Wiechen",
    #                                          "GSED LF stream A",
    #                                          "GSED LF stream B",
    #                                          "GSED LF stream C"),
    #                              selected = "GSED LF"),
    #                  ##add choice of stop rule
    #                  tags$h3("Stop rule"),
    #                  tags$p("Below the settings for the stop rule can be defined. By default the test stops, when the Standard error of measurement (SEM) of the D-score is below 1. A higher SEM threshold leads to a shorter test, but with a higher measurement error around the D-score. A lower SEM threshold means a smaller error around the D-score but also a longer test (i.e. more questions asked."),
    #                  numericInput("semrule",
    #                               label = "Threshold for SEM as stopping rule",
    #                               value = 1,
    #                               min = 0, max = 10,
    #                               step = 0.1
    #                  ),
    #                  ##add choice of item leniency
    #                  tags$h3("Item leniency"),
    #                  tags$p("The leniency for the items is the probability for passing the next question, given the previous answers. The default leniency is set at 0.5. This means that the probability for passing the next item is 0.5. When the leniency is set to a higher probability, the items are easier to pass (given previous answers), but it means that there are more items needed to finish the test."),
    #                  sliderInput("leniency",
    #                              label = "Leniency for selecting the next item",
    #                              min = 0,
    #                              max = 1,
    #                              value = 0.5,
    #                              step = 0.05)
    #              )
    #          )
    # ),
     #info page ----
 #   tabPanel("Information")
    # navbarMenu("Background",
    #            #checklist info ----
    #            tabPanel("Itembanks"),
    #            #calculation info ----
    #            tabPanel("D-score"),
    #            #interpretation info ----
    #            tabPanel("Interpretation")),
    # tabPanel("About",
    #          fluidRow(
    #              box(
    #                  tags$h2("Demo application for D-score adaptive test"),
    #                  tags$p("This is a demo version developed to demonstrate how an adaptive test works for measuring D-score. This version is developed for demonstration and illustrative purposes and should not be used in research or practice in its current form")
    #              ),
    #              box(width = 12, title = "About the authors",
    #                  tags$a(img(src="TNO_zwart.jpg", width="10%"),href="https://www.tno.nl/nl/"),tags$p("Department Child Health"),
    #                  tags$br(),
    #                  tags$h4("Iris Eekhout"),
    #                  tags$div("Email: iris.eekhout@tno.nl"),
    #                  tags$div("Website: ",tags$a("www.iriseekhout.com", href="https://www.iriseekhout.com")),
    #                  tags$div("Github: ", tags$a("iriseekhout", href="https://github.com/iriseekhout"))
    #
    #              ),
    #              box(width= 12,
    #                  tags$h4("GSED consortium"),
    #                  tags$div("The data and instrument content and information was developed by the GSED consortium.")
    #              )
    #          )
 #)

)
