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
    theme = shinytheme("cerulean"),
    collapsible = TRUE,
    "ICC power      ",
    tabPanel("Home",
             tags$iframe(src = "./home.html",
                         width = "100%",
                         frameborder = 0, scrolling = "auto", style = "height: 100vh;"
             )
    ),

    tabPanel("Study Results",
             sidebarLayout(
                 # MSE ratio's paper
                 sidebarPanel(
                     shinyjs::useShinyjs(),
                     tags$h2("Study Design"),
                     tags$p("Below you can select one of the conditions to compare the simulation results for. If you choose 'none' you can select the setting for each of the four conditions."),

                     radioButtons(
                         "compare",
                         label = "Condition to compare",
                         choices = c("methods","correlation","variance", "deviation", "none"),
                         inline = TRUE
                     ),
                     br(),
                     tags$h3("Fixed conditions:"),
                     div(id = "methods_buttons",
                         radioButtons(
                             "method",
                             label = "Type of ICC",
                             choices = c("oneway", "agreement", "consistency")
                         ))%>% shinyjs::hidden(),
                     div(id = "correlation_buttons",
                         radioButtons(
                             "correlation",
                             label = "Expected correlation between raters",
                             choices = c(0.6, 0.7, 0.8),
                             selected = 0.7
                         )) %>% shinyjs::hidden(),
                     div(id = "variance_buttons",
                         radioButtons("variance",
                                      label = "Expected variance in scores",
                                      choices = c(1, 10, 100)
                         )) %>% shinyjs::hidden(),
                     div(id = "systdif_buttons",
                         radioButtons("systdif",
                                      label = "0, 1 or 2 raters with systematic differences",
                                      choices = c(0, 1, 2)
                         )) %>% shinyjs::hidden(),
                     width = 3
                 ),

                 mainPanel(
                     #dataTableOutput("ratdf_k"),
                     fluidRow(box(
                         width = 12,
                         h1("Visualisation of simulation results"),
                         #    # p(
                         #   "The input parameter can be selected from the left-hand panel. For simulations results shown below, the following input paramers are selected:"
                         # ),
                         # textOutput("variableselection")
                     )),
                     fluidRow(
                         width = 12,
                         box(width = 12,
                             h2("Results for ICC estimations"),
                             p("")),
                         tabBox(
                             width = 10,
                             # The id lets us use input$tabset1 on the server to find the current tab
                             id = "icc",
                             tabPanel("Bias for ICC", plotOutput("biasicc")),
                             tabPanel("MSE for ICC", plotOutput("mseicc")),
                             tabPanel("Coverage of CI for ICC", plotOutput("covicc")),
                             tabPanel("Width of CI for ICC", plotOutput("widthicc"))
                         )
                     ),
                     fluidRow(
                         width = 12,
                         box(width = 12,
                             h2("Results for SEM estimations"),
                             p("")),
                         tabBox(
                             width = 10,
                             #side = "right",
                             tabPanel("Bias for SEM", plotOutput("biassem")),
                             tabPanel("MSE for SEM", plotOutput("msesem"))
                         )
                     ),
                     #fluidRow(
                     #  box(width = 12,
                     #      h2("Visualisation of MSE ratio's for ICC"),
                     #      p("The plots below show the required increase in sample size (left) or number of raters(right), when a similar precision of a higher number of raters (left) or sample size (right) is wanted."))
                     #),
                     #fluidRow(
                     #    shinydashboard::box(plotOutput("mseratio_n")),
                     #    shinydashboard::box(plotOutput("mseratio_k"))
                     #)
                 )
             )),
    tabPanel(
        "Choice assistant",
        sidebarLayout(
            # MSE ratio's paper
            sidebarPanel(
                tags$h2("Study Design"),
                tags$p(
                    "First indicate the approach for the power calculation. Then, depending on the method, parts of the study study design can be specified."
                ),

                # hiertussen nog een conditionele vraag over wel van de onderste drie parameters onderzocht moet worden. Voor welke design parameter wil je het effect op de sample size bekijken? Afhankelijk van dit antwoord, worden de andere twee vast gezet voor beide scenarios - wel gekozen maar gelijk over de twee scenarios, de ander kan dan varieren tussen referentie en goal. Deze input werkt ook in de interpretatie die in een textoutput moet komen.
                radioButtons(
                    "power",
                    label = "Calculate power parameters by",
                    choices = c("CI width", "CI lower", "MSE ratio"),
                    selected = "0",
                    inline = TRUE
                ),
                #input power ci lower ----
                conditionalPanel(
                    condition = "output.powercilow",
                    sliderInput(
                        "icc_e",
                        label = "Expected ICC",
                        min = 0.1,
                        max = 1,
                        value = 0.7
                    ),
                    sliderInput(
                        "cilower",
                        label = "Lower end of the Confidence interval for ICC (needs to be lower than expected ICC)",
                        min = 0.1,
                        max = 1,
                        value = 0.55
                    ),
                    sliderInput(
                        "alphalevel",
                        label = "Confidence interval proportion",
                        min = 0.90,
                        max = 0.99,
                        value = 0.95
                    ),
                    sliderInput(
                        "betapower",
                        label = "Power, also called assurance probability.",
                        min = 0.80,
                        max = 0.99,
                        value = 0.80
                    ),
                    sliderInput(
                        "raterrange",
                        label = "Number of raters",
                        min = 1,
                        max = 99,
                        step = 1,
                        value = c(2, 6)
                    )

                ),
                #input power ci width ----
                conditionalPanel(
                    condition = "output.powerci",
                    checkboxGroupInput(
                        "method_iccrgw",
                        label = "Type of ICC",
                        choices = c("oneway", "agreement", "consistency"),
                        inline = TRUE
                    ),
                    checkboxGroupInput(
                        "correlation_iccrgw",
                        label = "Expected correlation between raters",
                        choices = c(0.6, 0.7, 0.8),
                        selected = c(0.6, 0.7, 0.8),
                        inline = TRUE
                    ),
                    checkboxGroupInput(
                        "variance_iccrgw",
                        label = "Expected variance in scores",
                        choices = c(1, 10, 100),
                        selected = 1,
                        inline = TRUE
                    ),
                    radioButtons(
                        "systdif_iccrgw",
                        label = "0, 1 or 2 raters with systematic differences",
                        choices = c(0, 1, 2),
                        inline = TRUE
                    ),
                    sliderInput(
                        "ciwidthw",
                        label = "Width of the 95% Confidence interval for ICC",
                        min = 0.1,
                        max = 1,
                        value = 0.3
                    )

                ),
                #input power mse ratio ----
                conditionalPanel(
                    condition = "output.powermse",
                    p(
                        "Either sample size or number of raters can be varied between the reference and goal. For that condition the MSE ratio is calculated. The MSE ratio can be used to estimate the required sample size/rater increase to reach the goal."
                    ),
                    selectInput(
                        "design",
                        label = "Design aspect to vary between reference and goal",
                        choices = c("raters", "sample size"),
                        selected = character(0)
                    ),
                    #vary for raters ----
                    # conditionalPanel(
                    checkboxGroupInput(
                        "method_iccrg",
                        label = "Type of ICC",
                        choices = c("oneway", "agreement", "consistency"),
                        select = "agreement",
                        inline = TRUE
                    ),
                    checkboxGroupInput(
                        "correlation_iccrg",
                        label = "Expected correlation between raters",
                        choices = c(0.6, 0.7, 0.8),
                        selected = c(0.6, 0.7, 0.8),
                        inline = TRUE
                    ),
                    checkboxGroupInput(
                        "variance_iccrg",
                        label = "Expected variance in scores",
                        choices = c(1, 10, 100),
                        selected = 1,
                        inline = TRUE
                    ),
                    radioButtons(
                        "systdif_iccrg",
                        label = "0, 1 or 2 raters with systematic differences",
                        choices = c(0, 1, 2),
                        inline = TRUE
                    ),
                    shinyjs::useShinyjs(),
                    div(id = "designk", ##vary for raters
                        radioButtons(
                            "n_iccrg",
                            label = "Sample size",
                            choices = c(10, 20, 25, 30, 40, 50, 100, 200),
                            selected = "50",
                            inline = TRUE
                        ),
                        radioButtons(
                            "k_iccr",
                            label = "number of raters in reference",
                            choices = c(2, 3, 4, 5, 6),
                            selected = "2",
                            inline = TRUE
                        ),
                        radioButtons(
                            "k_iccg",
                            label = "number of raters for goal",
                            choices = c(2, 3, 4, 5, 6),
                            selected = "3",
                            inline = TRUE
                        )
                    ) %>% hidden(),
                    div(id = "designn", ## vary for sample size
                        radioButtons(
                            "k_iccrg",
                            label = "number of raters",
                            choices = c(2, 3, 4, 5, 6),
                            inline = TRUE
                        ),
                        radioButtons(
                            "n_iccr",
                            label = "Sample size in reference",
                            choices = c(10, 20, 25, 30, 40, 50, 100, 200),
                            selected = "50",
                            inline = TRUE
                        ),
                        radioButtons(
                            "n_iccg",
                            label = "Sample size goal",
                            choices = c(10, 20, 25, 30, 40, 50, 100, 200),
                            selected = "100",
                            inline = TRUE
                        )
                    ) %>% hidden()
                )
        ),

        # power outputs ----
        mainPanel(
            conditionalPanel(
                condition = "output.startpage",
                h2("Power estimations"),
                div(
                    p("On this page power calculations can be performed using three different strategies."
                    ),
                    br(),
                    strong("MSE ratio: "), "the MSE ratio procedure uses the simulation study to estimate to what extend a current reference design needs to be updated to achieve a similar precision as a goal design.",
                    br(),
                    strong("CI lower: "), "the CI lower procedure uses a formula presented in Zou (2011) to estimate the sample size required given the icc, lower limit of the icc and the number of raters. With this method it is possible to give a range of rater numbers in order to visualise the required sample size for different rater conditions."), em("Note that this method was developed for the ICC oneway and the estimated sample size is therefore conservative when applied for ICC agreement or consistency."),
                br(),
                strong("CI width:"), "the CI width procedure also uses teh simulations study. The specified width of the confidence interval is used to determine what conditions of raters and sample size can achieve that CI width under the referenced design conditions."
            ),
            conditionalPanel(
                condition = "output.powermse",
                fluidRow(
                    textOutput("variableselection2"),#for testing
                    h3("Power by MSE"),
                    p("The figure below shows the confidence interval computed via the mean squared error for the reference situation and the goal scenario."),
                    plotOutput("mseratio_icc")),
                fluidRow(
                    h3("MSE ratio"),
                    p( "The MSE ratio relates directly to the sample size. The Mean squared error is a combination of the squared standard error and the squared bias. In the situation of virtually no bias, the square root of the MSE equals the standard error. The standard error decreases by the square root of the sample size. Accordingly, the MSE is directly related to the sample size; the MSE decreases by the sample size."
                    ),
                    h3("Simulation results"),
                    p( "The simulation results can be used to compare different study design scenario's. By computing the ratio of the MSE in two different scenarios, let's say a reference and a goal scenario, the ratio indicates the required ratio difference in sample for the scenario's to achieve equal precision."
                    ),
                    ##hier dan nog uitleg toevoegen over hoe de MSE ratio geinterpreteerd moet worden. Dit moet helemaal geleid worden met de input parameters.
                    h3("Interpreting MSE ratio"),
                    textOutput("MSEratio")
                )
            ),
            conditionalPanel(condition = "output.powerci",
                             fluidRow(
                                 h2("Power by Confidence interval width"),
                                 p("The plot below shows the sample size and rater combinations that satisfy the confidence interval width requirement, given the scenario specified."),
                                 #plotOutput("widthcond"),
                                 plotlyOutput("widthmap")
                             )),
            conditionalPanel(condition = "output.powercilow",
                             h2("Power by lower end of Confidence Interval"),
                             p("The plot below shows the required sample size for the number of raters on the x-axis that are needed for the scenario with a given ICC to obtain a confidence interval with a lower end as indicated."), em("Note that this method was developed for the ICC oneway and the estimated sample size is therefore conservative when applied for ICC agreement or consistency."),
                             fluidRow(plotlyOutput("n_icc_plot"))),

        )
    )
),

navbarMenu("About",
           tabPanel("Background",
                    p(
                        "On this page add background info on calculations, links to package and documentation."
                    )
           ),
           tabPanel("Authors",
                    fluidRow(box(
                        #tags$h1("About"),
                        width = 12,
                        # box(width= 300,
                        #     tags$a(img(src="TNO_zwart.jpg", width="10%"),href="https://www.tno.nl/nl/"),
                        #     tags$h4("Department Child Health")),
                        box(
                            width = 10,
                            tags$h4("Iris Eekhout"),
                            tags$div("Email: iris.eekhout@tno.nl"),
                            tags$div(
                                "Website: ",
                                tags$a("www.iriseekhout.com", href = "https://www.iriseekhout.com")
                            ),
                            tags$div(
                                "Github: ",
                                tags$a("iriseekhout", href = "https://github.com/iriseekhout")
                            )
                        )
                    )))
)
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
