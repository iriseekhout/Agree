library(shinyWidgets)
library(shinyBS)

ui <-tagList(
    tags$head(tags$script(src="js/index.js"),
              useShinydashboard()#,
      #         tags$script(HTML('
      #   var fakeClick = function(tabName) {
      #     var dropdownList = document.getElementsByTagName("a");
      #     for (var i = 0; i < dropdownList.length; i++) {
      #       var link = dropdownList[i];
      #       if(link.getAttribute("data-value") == tabName) {
      #         link.click();
      #       };
      #     }
      #   };
      # '))
              ),

    navbarPage(
    theme = shinytheme("flatly"),
    collapsible = TRUE,
    "ICC power      ",

    ####HOME ----
    tabPanel("Home",
            fluidPage(
              fluidRow(
                box( width = 9,
                h1("Sample Size Decision Assistant"),
                p("The sample size decision assistant helps you to decide on the number of study participants and repeated measurements for your study on reliability or measurement error. This tool helps to trade-off ", strong("efficiency"), " and ", strong("precision."))#,
               # div("One of the decisions you need to make when designing you study on reliability or measurement error is about ", strong("how many patients"), " you need to include in your study and ", strong("how often"), " you need to measure them, for example by different raters in an inter-rater design, or on different occasions in a test-retest design. Naturally, you want to include ",strong("as efficiently as possible"),". You don't want to include too many patients or repeat measurements, because that can be too burdensome for patients and expensive for you as researchers. But you also don't want to have too few patients or repeated measurements, because then you get inaccurate results.")
                )
                ),
              fluidRow(
                column(width = 3,
                       strong("Are you in the design phase of your study?")),
                column(width = 3,
                     strong("Are you designing a one-way reliability study?")),
                column(width = 3,
                   strong("Is data collection in you study (s)low?"))
              ),
              fluidRow(
                box(title =p("Confidence Interval width",
                             style="color:white",
                             onclick = "customHref('CI width')"),
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    collapsed = TRUE,
                    div(strong("Design phase:"), " use the Confidence Interval (CI) width procedure. Suitable recommendations for sample size and repeated measurements are returned, based on your acceptable width of the confidence intervals and your assumptions about the measurement instrument. ", a("Click here to move to CI width page.", onclick = "customHref('CI width')")),
                    status = "success",
                    width = 3
                ),
                box(title =p("Confidence Interval lower-limit",
                             style="color:white",
                             onclick = "customHref('CI lower')"),
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    collapsed = TRUE,
                    div(strong("One-way random effects model:"), " Use the Confidence Interval (CI) lower limit procedure. The one-way effects model requires somewhat larger sample sizes, so recommendations are conservative. Where the other two procedures are restricted within the conditions chosen in the", a("simulation studies", onclick = "customHref('Simulation results')"), "this procedure can be used for any conditions as it is based on an analytical approach. ", a("Click here to move to CI lower", onclick = "customHref('CI lower')")),
                    status = "success",
                    width = 3
                ),
                box(
                  title = p("MSE ratio",
                            style="color:white",
                            onclick = "customHref('MSE ratio')"),
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  collapsed = TRUE,
                  div(strong("Slow inclusion:"), " Use the MSE ratio procedure. This gives (1) insight in the decrease of precision due to lower sample sizes, or (2) provides you with insight in how to adapt your design to obtain an acceptable width of the confidence interval. ", a("Click here to move to MSE ratio", onclick = "customHref('MSE ratio')")),
                 status = "success",
                 width = 3)


              ),

              br(), br(),

              fluidRow(
                box(title = p("Simulation study & how to cite", style="color:white",
                                     onclick = "customHref('Simulation results')"),
                           solidHeader = TRUE,
                           collapsible = TRUE,
                           collapsed = TRUE,
                           div("The CI width procedure and the MSE ratio procedure are based on simulation studies (Mokkink et al. 2022). All specific results of the simulations are shown ",a("here", onclick="customHref('Simulation results')"),". The CI lower limit procedure can be used in situations beyond the conditions considered in the simulation studies.", br(),br(), strong("Citation for the app:"), br(),
                              "Mokkink, L.B., de Vet, H., Diemeer, S. et al. Sample size recommendations for studies on reliability and measurement error: an online application based on simulation studies. Health Serv Outcomes Res Method (2022).", a("https://doi.org/10.1007/s10742-022-00293-9",href = "https://doi.org/10.1007/s10742-022-00293-9") ),
                           status = "primary",
                           width = 9),
                box(title = p("Design issues beyond sample size", style="color:white",
                              onclick = "customHref('FAQ & links')"),
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    collapsed = TRUE,
                    div("For assistance in comprehensively formulating your reserach question, and making other design choices, such as which patients and professionals should be included, and how to build the appropriate statistical formula for your study, we refer to this article < link>."),
                    status = "primary",
                    width = 9)
                )
            )


    ),
    navbarMenu("Sample Size Decision assistant",

               ####CIWidth----------
               tabPanel("CI width",
                        fluidPage(
                          fluidRow(
                          box(title = h2("Confidence Interval (CI) width procedure"),
                              width = 12,
                              solidHeader = TRUE,
                              collapsible = TRUE,
                              collapsed = FALSE,
                              div(strong("Phase of use:"), " design phase of your study, i.e. before the start of the data collection",br(), strong("Goal:"), " determine the precision of your target sample size to report in your study protocol: what conditions of sample size of patients and repeated measurements can achieve your acceptable CI width?",br(), strong("Pre-specifications:"), " acceptable width of the confidence interval, ICC or SEM model that you will use, expected ICC/SEM value",
                                br(),  strong("Reach of procedure:"), " feasible for all conditions described in the simulation study <link ref> as it is based on the results of the ",a("simulation", onclick="customHref('Simulation results')") )
                              )
                          ),


                      sidebarLayout(
                          # CI widthinput
                        sidebarPanel(
                            tags$h3("Input settings"),

                            tags$div("Define the design of your study below",tags$sup("?"), id = "ciwsetting"),
                            bsTooltip("ciwsetting", "You need to specify some design choices, and make an assumptions on the correlation between the repeated measurements (e.g. based on previously reported ICCs), the presence of a systematic difference (e.g. between raters), and expected variance in score (…).", placement = "top", trigger = "hover"),
                            radioButtons(
                              "method_iccrgw",
                              label = "Type of ICC or SEM",
                              choices = c("oneway", "agreement", "consistency"),
                              selected = "oneway",
                              inline = TRUE
                            ),
                            bsTooltip("method_iccrgw", "SEM is expressed in the unit of measurement of the instrument.", placement = "top", trigger = "hover"),
                            radioButtons(
                              "correlation_iccrgw",
                              label = "Expected correlation between repeated measurements",
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
                              value = 0.3),
                              bsTooltip("ciwidthw_icc", "The width depends of the unit of measurements. As the range of the ICC is always between 0-1, the range of the target width is fixed, and it is set default at 0.3.", placement = "top", trigger = "hover", options = list(container = "body")),

                            #update slider with variance unit
                            uiOutput("slider_ciwidthw_sem"),
                            bsTooltip("slider_ciwidthw_sem", "The width depends of the unit of measurements. The SEM depends on the unit of measurement, and the default setting for the target width of the 95% CI width changes across conditions v.", placement = "top", trigger = "hover", options = list(container = "body"))

                          ),
                          mainPanel(
                            fluidRow(
                              h3("Recommendations"),
                              p("Scroll over the plot below to find the exact results for the CI width per combination of the sample size and number of repeated measurements. Colored boxed indicate combinations that satisfy your acceptable width for the confidence interval, given your specified scenario. The ICC plot is shown in the ICC-tab and the plot for SEM in the SEM-tab."),
                              tabsetPanel(type = "tabs",
                                          tabPanel("ICC", plotlyOutput("widthmap_icc")),
                                          tabPanel("SEM", plotlyOutput("widthmap_sem"))
                              )
                            )
                          )
                        )
)

               ),
#### CILOWER-----
tabPanel("CI lower",
         fluidPage(
           fluidRow(
             box(title = h2("Confidence Interval (CI) lower limit procedure"),
                 width = 12,
                 solidHeader = TRUE,
                 collapsible = TRUE,
                 collapsed = FALSE,
                 div(strong("Phase of use:"), " design phase of your study, i.e. before the start of the data collection, and you want to apply the ", strong("one-way random effects model"),".",br(), strong("Goal:"), " determine the precision of your target sample size (to report in the study protocol); what conditions of sample size and repeated measurements can achieve your acceptable CI width in your one-way random effects analysis",br(), strong("Pre-specifications:"), " acceptable lower limit of the 95% confidence interval, expected ICC/SEM value.",
                     br(),  strong("Reach of procedure:"), " any conditions in a one-way random effects model, as it is based on an analytical approach", a("Zou (2011)", href = "https://doi.org/10.1002/sim.5466"))
             )
           ),

         sidebarLayout(

           sidebarPanel(
             tags$h3("Input settings"),
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
             bsTooltip("alphalevel", "Probability limits for the confidence interval, most common is 95% or 99%.", placement = "top", trigger = "hover", options = list(container = "body")),
             sliderInput(
               "betapower",
               label = "Power, also called assurance probability.",
               min = 0.80,
               max = 0.99,
               value = 0.80
             ),
             bsTooltip("betapower", "Assurance probability: the probability of achieving the desired precision level, i.e. lower limit of the confidence interval.", placement = "top", trigger = "hover", options = list(container = "body")),
             sliderInput(
               "raterrange",
               label = "Number of repeated measurments",
               min = 1,
               max = 99,
               step = 1,
               value = c(2, 6)
             ),
             bsTooltip("raterrange", "The target number of repeated measurements (i.e. as described in the protocol).", placement = "top", trigger = "hover", options = list(container = "body"))
           ),
           mainPanel(
             ## results for CI lower
             h3("Recommendations"),
             p("The plot below shows the required sample size (y-axis) for the number of repeated measurements on the x-axis that are needed for your scenario with a given ICC to obtain a confidence interval with a lower end as indicated in your scenario."),
             em("Note that this method was developed for the ICC oneway and the estimated sample size is therefore conservative when applied for ICC agreement or consistency."),
             fluidRow(plotlyOutput("n_icc_plot"))
           )


         )
         )
),
    #####MSE ratio-----
               tabPanel("MSE ratio",
                        fluidPage(
                          fluidRow(
                            box(title = h2("MSE ratio procedure"),
                                width = 12,
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                collapsed = FALSE,
                                div(strong("Phase of use:"), " during data collection when inclusion is (s)low, and you realize that the intended sample size of patients and the number of repeated measurements (as described in your protocol) cannot be reached.",br(), strong("Goal:"), " (1) To estimate the decrease in precision is when you compare the current design (i.e. the number of patients or repeated measurements included so far in the study) to the target design (i.e. as described in the protocol). (2) To understand how the current design can be updated in terms of either number of patients or number of repeated measurements to achieve a similar precision as the target design.",br(), strong("Pre-specifications:"), " ICC or SEM model that you will use, expected ICC/SEM value, expected variance in scores.",
                                    br(),  strong("Reach of procedure:"), " feasible for all conditions described in the simulation study <link ref> as it is based on the results of the ",a("simulation", onclick="customHref('Simulation results')"))
                            )
                          ),


                        sidebarLayout(
                          # MSE ratio's paper
                          sidebarPanel(

                            radioButtons(
                              "statistic",
                              label = "Main interest in ICC or SEM?",
                              choices = c("ICC", "SEM"),
                              selected = "ICC",
                              inline = T
                            ),

                            br(),
                            # conditionalPanel(
                            tags$h3("Input settings"),
                            tags$p("Define the design of your study below:"),
                            radioButtons(
                              "method_iccrg",
                              label = "Type of ICC or SEM",
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

                            selectInput(
                              "design",
                              label = "What aspect of the design is lagging?",
                              choices = c("repeated measurements", "sample size"),
                              selected = character(0)
                            ),

                            shinyjs::useShinyjs(),
                            div(id = "designk", ##vary for raters
                                strong("Repeated measurement are lagging, specify the current and goal numbers."),
                                br(),
                                radioButtons(
                                  "k_iccr",
                                  label = "number of repeated measurements in adapted (current) design",
                                  choices = c(2, 3, 4, 5, 6),
                                  selected = "2",
                                  inline = TRUE
                                ),
                                radioButtons(
                                  "k_iccg",
                                  label = "number of repeated measurements in target (goal) design per protocol",
                                  choices = c(2, 3, 4, 5, 6),
                                  selected = "3",
                                  inline = TRUE
                                ),
                                radioButtons(
                                  "n_iccrg",
                                  label = "Sample size (patients)",
                                  choices = c(10, 20, 25, 30, 40, 50, 100, 200),
                                  selected = "50",
                                  inline = TRUE
                                )
                            ) %>% hidden(),
                            div(id = "designn", ## vary for sample size
                                strong("Sample size is lagging, specify the size and target size."),
                                br(),
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
                                ),
                                radioButtons(
                                  "k_iccrg",
                                  label = "number of repeated measurements",
                                  choices = c(2, 3, 4, 5, 6),
                                  inline = TRUE
                                )
                            ) %>% hidden()
                          )

                          ,
                          mainPanel(
                            ## results for MSE ratio

                            h3("Precision loss"),
                            strong("Target input:"),
                            textOutput("mseratio_targetinput"),
                            strong("Adapted input:"),
                            textOutput("mseratio_adaptedinput"),

                           # p("The figure below shows the confidence interval computed via the mean squared error for the adapted design and the target design (> link Mokkink et al.,  2022)."),
                            plotOutput("mseratio_icc", height = "200px", width = "75%"),

                          fluidRow(
                            h3("Recommendations"),
                            plotOutput("mseratio_plot", height = "100px", width = "75%"),

                            box(title = "MSE ratio interpretation",
                                collapsible = T,
                                collapsed = F,
                                solidHeader = T,
                                width = 12,
                                textOutput("MSEratio")
                                ),
                            box(title = "Explanations",
                                collapsible = T,
                                collapsed = T,
                                solidHeader = T,
                                width = 12,
                                h4("MSE ratio"),
                            p( "The MSE ratio relates directly to the sample size, and the number of repeated measurements. The mean squared error is a combination of the squared standard error and the squared bias (see Mokkink et al.). In the situation of virtually no bias, the square root of the MSE equals the standard error. The standard error decreases by the square root of the number of repeated measurements. Accordingly, the MSE is directly related to the sample size and the number of repeated measurements; the MSE decreases by the sample size."
                            ),
                            h4("Simulation results"),
                            p( "The simulation results can be used to compare different study design scenario's. By computing the ratio of the MSE in two different scenarios, that is the adapted (current) and target (defined at the start of the study) design, the ratio indicates the required change in sample size or repeated measurements for the scenario's to achieve equal precision. The decreased precision of the adapted design shows the loss in precision relative to the target design, when the sample size or repeated measurements cannot be further increased."
                            )
                            )

                          )

                        )
)
                        )
)
               ),
##### Simulation results ----
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
   # # tabPanel(
   #      "Design choice assistant", value = "design",
   #
   #              h2("Power estimations"),
   #              div(
   #                  p("The Design choice assistant informs your choice on sample size and number of repeated measurements in a study on reliability and measurement error. You can choose between three different approaches, depending on the phase you are in, or the model that you prefer."
   #                  ),
   #                  br(),
   #              strong("Confidence Interval (CI) width procedure:"), "you can use this procedure when you are designing your study. this procedure is based on the results of the simulation study. The specified width of the confidence interval (CI) is used to determine what conditions of sample size and repeated measurements can achieve that CI width under the chosen design conditions."
   #          ,
   #          br(),
   #          strong("Confidence Interval (CI) lower limit procedure: "), "you can choose this procedure when you are designing a study and want to apply the one-way random effects model. The CI lower procedure is based on  a formula presented in ", a("Zou (2011)", href = "https://doi.org/10.1002/sim.5466"), "to estimate the sample size required. This model requires somewhat larger sample sizes, so it will give an conservative recommendations for use in other effect models.  With this method it is possible to give a range of numbers of repeated measurements in order to visualize the required sample size for different numbers of repeated measurements.",
   #          br(),
   #                  strong("MSE ratio procedure: "), "When there is a need to change or reconsider the target design of the study, e.g. the patient recruitment is slow or one of the raters drops out, you can use the MSE ratio procedure. The MSE ratio procedure is based on the results of the simulation study to estimate to what extend a current adapted design (e.g. the number of patients or repeated measurements included so far in the study) needs to be updated to achieve a similar precision as the target design (i.e. the intended number of patients or repeated measruements chosen at the start of the study)." )),

           # br(),
         #   "We specifically used the results for ICC estimations for the recommendations, as these required (in addition to MSE values) confidence intervals, which we could not obtain for the SEM estimations. In addition, because we saw the same trends in MSE values for ICC and SEM, and we recommend always reporting both parameters in a study, we focus only on ICC estimations."


#### FAQ ----
tabPanel("FAQ & links",
         fluidPage(
         fluidRow(

         h1("Frequently Asked Questions"),
         br(),
         fluidRow(
         box(
           title = "Where can I find more information?",
           collapsible = TRUE,
           collapsed = TRUE,
           solidHeader = TRUE,
           status = "success",
           p("> link paper with more info")

         )),
         fluidRow(
         box(
           title = "Where can I find information to decide which ICC or SEM formula matches my design?",
           collapsible = TRUE,
           collapsed = TRUE,
           solidHeader = TRUE,
           status = "success",
           p("> link paper with more info")

         )),
         fluidRow(
         box(
           title = "How can I estimate the ICCs and SEMs in R?",
           collapsible = TRUE,
           collapsed = TRUE,
           solidHeader = TRUE,
           status = "success",
           tags$div("The R package", tags$a("Agree", href = "https://github.com/iriseekhout/Agree"), "includes all functions to obtain the ICCs and SEMs for different types of ICCs. The Agree package is publically available and can be installed in R using the following code: ", tags$code("remotes::install_github(repo = 'iriseekhout/Agree')"))
         ))

         ))

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
