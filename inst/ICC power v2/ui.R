
ui <-tagList(
    tags$head(tags$script(src="js/index.js")),

    navbarPage(
    theme = shinytheme("cerulean"),
    collapsible = TRUE,
    "ICC power      ",
    tabPanel("Home",
             #tags$iframe(src = "./home.html",
            #             width = "100%",
            #             frameborder = 0, scrolling = "auto", style = "height: 100vh;"
            tags$head(tags$script(HTML('
        var fakeClick = function(tabName) {
          var dropdownList = document.getElementsByTagName("a");
          for (var i = 0; i < dropdownList.length; i++) {
            var link = dropdownList[i];
            if(link.getAttribute("data-value") == tabName) {
              link.click();
            };
          }
        };
      '))),
            fluidPage(
              fluidRow(box(h1("Sample size choice assistent"),
                           p("The tools on this webpage help you to choose the sample size that matches with your study goals and study design. There are three different approaches, each suited for a different phase in the study."))),
              fluidRow(box(h2("MSE ratio"),
                           "Brief explanation of method and when to use it.. by clicking on the box you will be directed to the MSE ratio page directly."

                           , onclick = "fakeClick('MSE ratio')")),
              fluidRow(box(h2("CI width"),
                               "Brief explanation of method and when to use it.. by clicking on the box you will be directed to the CI width page directly.",
                               onclick = "fakeClick('CI width')")),
              fluidRow(box(h2("CI lower"),
                           "Brief explanation of method and when to use it.. by clicking on the box you will be directed to the CI lower page directly.", onclick = "fakeClick('CI lower')")),

              br(), br(),

              fluidRow(box(h2("Simulation study"),
                           p("Explanation of simulation study and that you can reach it by clicking on this box.", onclick = "fakeClick('Simulation results')")))
            )


    ),

    navbarMenu("Sample size choice assistant",
               tabPanel("MSE ratio",
                        sidebarLayout(
                          # MSE ratio's paper
                          sidebarPanel(

                            p("The MSE ratio can be computed to optimize the design with respect to the ICC statistic or for the SEM statistic."),
                            radioButtons(
                              "statistic",
                              label = "MSE ratio procedure for ICC or for SEM.",
                              choices = c("ICC", "SEM"),
                              selected = "ICC",
                              inline = T
                            ),
                            p("Either the sample size or the number of repeated measurements can be varied between the adapted (what you have collected so far) and target design (the chosen design at the start of the study, referring to the target sample size and number of repeated measurements). For that condition the MSE ratio is calculated. The MSE ratio can be used to estimate the required sample size/rater increase to reach the goal."
                            ),

                            br(),
                            em("Target design refers to the intended number of patients or number of repeated measurements chosen at the start of the study, i.e. your goal; adapted design refers to the number of patients or repeated measurements included so far."),
                            selectInput(
                              "design",
                              label = "Design aspect to vary between adapted (current) design and target (goal) design",
                              choices = c("raters", "sample size"),
                              selected = character(0)
                            ),
                            #vary for raters ----
                            # conditionalPanel(
                            tags$h2("Study Design"),
                            tags$p("Define the design of your study below:"),
                            radioButtons(
                              "method_iccrg",
                              label = "Type of ICC",
                              choices = c("oneway", "agreement", "consistency"),
                              select = "agreement",
                              inline = TRUE
                            ),
                            radioButtons(
                              "correlation_iccrg",
                              label = "Expected correlation between repeated measurements",
                              choices = c(0.6, 0.7, 0.8),
                              selected = c(0.7),
                              inline = TRUE
                            ),
                            radioButtons(
                              "variance_iccrg",
                              label = "Expected variance in scores",
                              choices = c(1, 10, 100),
                              selected = 1,
                              inline = TRUE
                            ),
                            radioButtons(
                              "systdif_iccrg",
                              label = "Systematic differences expected between 0, 1 or 2 of the repeated measurements",
                              choices = c(0, 1, 2),
                              inline = TRUE
                            ),
                            shinyjs::useShinyjs(),
                            div(id = "designk", ##vary for raters
                                radioButtons(
                                  "n_iccrg",
                                  label = "Sample size (patients)",
                                  choices = c(10, 20, 25, 30, 40, 50, 100, 200),
                                  selected = "50",
                                  inline = TRUE
                                ),
                                radioButtons(
                                  "k_iccr",
                                  label = "number of repeated measurements in adapted (current) design",
                                  choices = c(2, 3, 4, 5, 6),
                                  selected = "2",
                                  inline = TRUE
                                ),
                                radioButtons(
                                  "k_iccg",
                                  label = "number of repeated measurements in target (goal) design",
                                  choices = c(2, 3, 4, 5, 6),
                                  selected = "3",
                                  inline = TRUE
                                )
                            ) %>% hidden(),
                            div(id = "designn", ## vary for sample size
                                radioButtons(
                                  "k_iccrg",
                                  label = "number of repeated measurements",
                                  choices = c(2, 3, 4, 5, 6),
                                  inline = TRUE
                                ),
                                radioButtons(
                                  "n_iccr",
                                  label = "Sample size in adapted (current) design",
                                  choices = c(10, 20, 25, 30, 40, 50, 100, 200),
                                  selected = "50",
                                  inline = TRUE
                                ),
                                radioButtons(
                                  "n_iccg",
                                  label = "Sample size in target (goal) design",
                                  choices = c(10, 20, 25, 30, 40, 50, 100, 200),
                                  selected = "100",
                                  inline = TRUE
                                )
                            ) %>% hidden()
                          )

                          ,
                          mainPanel(
                            ## results for MSE ratio
                            h3("Power by MSE procedure"),
                            p("The figure below shows the confidence interval computed via the mean squared error for the adapted design and the target design (> link Mokkink et al.,  2022)."),
                            plotOutput("mseratio_icc"),
                          fluidRow(
                            h3("MSE ratio"),
                            p( "The MSE ratio relates directly to the sample size, and the number of repeated measurements. The mean squared error is a combination of the squared standard error and the squared bias (see Mokkink et al.). In the situation of virtually no bias, the square root of the MSE equals the standard error. The standard error decreases by the square root of the number of repeated measurements. Accordingly, the MSE is directly related to the sample size and the number of repeated measurements; the MSE decreases by the sample size."
                            ),
                            h3("Simulation results"),
                            p( "The simulation results can be used to compare different study design scenario's. By computing the ratio of the MSE in two different scenarios, that is the adapted (current) and target (defined at the start of the study) design, the ratio indicates the required change in sample size or repeated measurements for the scenario's to achieve equal precision. The decreased precision of the adapted design shows the loss in precision relative to the target design, when the sample size or repeated measurements cannot be further increased."
                            ),
                            ##hier dan nog uitleg toevoegen over hoe de MSE ratio geinterpreteerd moet worden. Dit moet helemaal geleid worden met de input parameters.
                            h3("Interpreting MSE ratio"),
                            textOutput("MSEratio")
                          )
                          )

                        )

                        ),
               tabPanel("CI width",

                        sidebarLayout(
                          # CI widthinput
                          sidebarPanel(
                        tags$h2("Study Design"),
                        tags$p("Define the design of your study below:"),
                        radioButtons(
                          "method_iccrgw",
                          label = "Type of ICC",
                          choices = c("oneway", "agreement", "consistency"),
                          selected = "oneway",
                          inline = TRUE
                        ),
                        radioButtons(
                          "correlation_iccrgw",
                          label = "Expected correlation between repeated measurments",
                          choices = c(0.6, 0.7, 0.8),
                          selected = c(0.7),
                          inline = TRUE
                        ),
                        radioButtons(
                          "variance_iccrgw",
                          label = "Expected variance in scores",
                          choices = c(1, 10, 100),
                          selected = 1,
                          inline = TRUE
                        ),
                        radioButtons(
                          "systdif_iccrgw",
                          label = "Systematic differences expected between 0, 1 or 2 of the repeated measurements",
                          choices = c(0, 1, 2),
                          inline = TRUE
                        ),
                        sliderInput(
                          "ciwidthw_icc",
                          label = "Target width of the 95% Confidence interval of ICC.",
                          min = 0,
                          max = 1,
                          value = 0.3
                        ),
                        #update slider with variance unit
                        uiOutput("slider_ciwidthw_sem")

               ),
               mainPanel(
                 fluidRow(
                   h2("Power by Confidence interval width"),
                   p("The CI width procedures can be used when designing the study, i.e. before the start of the data collection, to determine the precision of the target design. In the CI width procedure a pre-specified width of the confidence interval (e.g. 0.3 for ICC) is set to determine what conditions of sample size and repeated measurements can achieve that specific CI width under the specified design conditions. The CI width procedure can be used for the precision of the ICC or the SEM. This way, various designs can be considered to decide on the most efficient target design (i.e. the chosen design that will be described in the study protocol)."),
                   br(),
                   p("The plot below shows the sample size and rater combinations that satisfy the confidence interval width requirement, given you specified scenario. The ICC plot is shown in the ICC-tab and the plot for SEM in the SEM-tab."),
                   br(),
                   em(">> Scroll over the figure to find the exact results"),
                   tabsetPanel(type = "tabs",
                               tabPanel("ICC", plotlyOutput("widthmap_icc")),
                               tabPanel("SEM", plotlyOutput("widthmap_sem"))
                   )
                   #plotlyOutput("widthmap")
                 )
               )
                        )
               ),

               tabPanel("CI lower",

                        sidebarLayout(
                          # MSE ratio's paper
                          sidebarPanel(
                          tags$h2("Study Design"),
                          tags$p("Define the design of your study below:"),
                          sliderInput(
                            "icc_e",
                            label = "Expected ICC",
                            min = 0.1,
                            max = 1,
                            value = 0.7
                          ),
                          sliderInput(
                            "cilower",
                            label = "Accepted lower limit of the Confidence interval for ICC (needs to be lower than expected ICC)",
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
                            label = "Number of repeated measurments",
                            min = 1,
                            max = 99,
                            step = 1,
                            value = c(2, 6)
                          )
                          ),
                          mainPanel(
                            ## results for CI lower
                            h2("Power by lower end of Confidence Interval"),
                            p("The lower limit of the CI is based on an analytical approach. The CI lower limit procedure is a known method in the literature and uses a formula for the confidence intervals presented in ", a("Zou (2011)", href = "https://doi.org/10.1002/sim.5466"), " to estimate the sample size required given the assumed ICC one-way, lower limit of the ICC one-way and the number of raters that will be involved. The advantage of this method is that it can be used beyond the specified conditions that are used in this simulation study. However, this method is based on the ICC one-way, and cannot be generalized to the other types of ICCs."),
                            p("The plot below shows the required sample size (y-axis) for the number of repeated measurements on the x-axis that are needed for your scenario with a given ICC to obtain a confidence interval with a lower end as indicated in your scenario."),
                            strong("Confidence interval proportion:"), "Probability limits for the confidence interval, most common is 95% or 99%.",
                            strong("Power:", "Defined as the assurance probability, which is the probabilitiy of achieving the desired precision level, i.e. lower limit of the confidence interval."),
                            em("Note that this method was developed for the ICC oneway and the estimated sample size is therefore conservative when applied for ICC agreement or consistency."),
                            fluidRow(plotlyOutput("n_icc_plot"))
                            )



                        )
               )
               ),
##### simulation results ----
    tabPanel("Simulation results",
             sidebarLayout(
                 # MSE ratio's paper
                 sidebarPanel(
                     shinyjs::useShinyjs(),
                     tags$h2("Study Design"),
                     tags$p("Below you can select whether you want to compare the simulation results of the conditions of specific variables. If you choose 'none', you will see results of the selected values of conditions only."),

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
                             label = "Expected correlation between repeated measurements",
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
                                      label = "Systematic differences between 0, 1 or 2 of the repeated measurements",
                                      choices = c(0, 1, 2)
                         )) %>% shinyjs::hidden(),
                     width = 3
                 ),

                 mainPanel(
                     #dataTableOutput("ratdf_k"),
                     fluidRow(box(
                         width = 12,
                         h1("Simulation results"),
                          p(
                            "The methods and general results of the simulation study can be found in Mokkink et al. <link>"
                          )
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
                             tabPanel("MSE for SEM", plotOutput("msesem")),
                             tabPanel("Width of CI for SEM", plotOutput("widthsem"))
                         )
                     )
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
#### design choice assistant ----
    tabPanel(
        "Design choice assistant", value = "design",

                h2("Power estimations"),
                div(
                    p("The Design choice assistant informs your choice on sample size and number of repeated measurements in a study on reliability and measurement error. You can choose between three different approaches, depending on the phase you are in, or the model that you prefer."
                    ),
                    br(),
                strong("Confidence Interval (CI) width procedure:"), "you can use this procedure when you are designing your study. this procedure is based on the results of the simulation study. The specified width of the confidence interval (CI) is used to determine what conditions of sample size and repeated measurements can achieve that CI width under the chosen design conditions."
            ,
            br(),
            strong("Confidence Interval (CI) lower limit procedure: "), "you can choose this procedure when you are designing a study and want to apply the one-way random effects model. The CI lower procedure is based on  a formula presented in ", a("Zou (2011)", href = "https://doi.org/10.1002/sim.5466"), "to estimate the sample size required. This model requires somewhat larger sample sizes, so it will give an conservative recommendations for use in other effect models.  With this method it is possible to give a range of numbers of repeated measurements in order to visualize the required sample size for different numbers of repeated measurements.",
            br(),
                    strong("MSE ratio procedure: "), "When there is a need to change or reconsider the target design of the study, e.g. the patient recruitment is slow or one of the raters drops out, you can use the MSE ratio procedure. The MSE ratio procedure is based on the results of the simulation study to estimate to what extend a current adapted design (e.g. the number of patients or repeated measurements included so far in the study) needs to be updated to achieve a similar precision as the target design (i.e. the intended number of patients or repeated measruements chosen at the start of the study)." )),

           # br(),
         #   "We specifically used the results for ICC estimations for the recommendations, as these required (in addition to MSE values) confidence intervals, which we could not obtain for the SEM estimations. In addition, because we saw the same trends in MSE values for ICC and SEM, and we recommend always reporting both parameters in a study, we focus only on ICC estimations."


#### FAQ ----
tabPanel("FAQ & links",
         fluidRow(
             column(width = 8, offset = 2,
           h1("Frequently Asked Questions"),
         br(),
           h4("Where can I find more information?"),
           p("> link paper"),
           h4("Where can I find information to decide which ICC or SEM formula matches my design?"),
           p("> link paper"),
           h4("Where can I find help to decide on the number of patients and repeated measurements in my study"),
         tags$div("The", tags$a("choice assistent", onclick = "customHref('design')"), " in this application can guide you with this decision."),
           h4("How can I estimate the ICCs and SEMs in R?"),
           tags$div("The R package", tags$a("Agree", href = "https://github.com/iriseekhout/Agree"), "includes all functions to obtain the ICCs and SEMs for different types of ICCs. The Agree package is publically available and can be installed in R using the following code: ", tags$code("remotes::install_github(repo = 'iriseekhout/Agree')")),
           h4("How can I estimate the ICCs and SEMs in SPSS?"),
           p("")
           )
         )
         ),
## about ----
tabPanel("About the team",
           tags$iframe(src = "./about.html",
                       width = "100%",
                       frameborder = 0, scrolling = "auto", style = "height: 100vh;"
           )
           )
## end about ----
)
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

#)
