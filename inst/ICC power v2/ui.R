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
    "ICC & SEM power  ",

    ####HOME ----
    tabPanel("Home",
            fluidPage(
              fluidRow(
                box( width = 10,
                h1("Sample Size Decision Assistant"),
                p("The sample size decision assistant helps you to decide on the number of study participants (patients) and repeated measurements (for example number of raters) for your study on reliability (ICC) or measurement error (SEM) in a crossed design. This tool helps to trade-off ", strong("efficiency"), " and ", strong("precision."))#,
               # div("One of the decisions you need to make when designing you study on reliability or measurement error is about ", strong("how many patients"), " you need to include in your study and ", strong("how often"), " you need to measure them, for example by different raters in an inter-rater design, or on different occasions in a test-retest design. Naturally, you want to include ",strong("as efficiently as possible"),". You don't want to include too many patients or repeat measurements, because that can be too burdensome for patients and expensive for you as researchers. But you also don't want to have too few patients or repeated measurements, because then you get inaccurate results.")
                )
                ),
              fluidRow(
                column(width = 5,
                       strong("You are writing your protocol and you need to decide upon the sample size of patients and number of repeated measurements")),

                column(width = 5,
                   strong("You started collecting your data, and your inclusion is (s)low"))
              ),
              fluidRow(
                box(title =p("Confidence Interval width",
                             style="color:white",
                             onclick = "customHref('CI width')"),
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    collapsed = TRUE,
                    div("You’ll get tailored recommendations for your sample size and number of repeated measurements. ", a("Click here to go to CI width page.", onclick = "customHref('CI width')")),
                    status = "success",
                    width = 5
                ),

                box(
                  title = p("MSE ratio",
                            style="color:white",
                            onclick = "customHref('MSE ratio')"),
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  collapsed = TRUE,
                  div("This gives you (1) insight in the decrease of precision due to lower sample sizes, and (2) provides you with insight in how to adapt your design to obtain an acceptable precision. ", a("Click here to go to MSE ratio", onclick = "customHref('MSE ratio')")),
                 status = "success",
                 width = 5)


              ),

              br(), br(),


              fluidRow(
                box(title =p("Is your study beyond the scope of our simulation conditions?",
                             style="color:white",
                             onclick = "customHref('CI lower')"),
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    collapsed = TRUE,
                    div(strong("Confidence interval lower-limit procedure:"), " Based on an analytical approach you’ll get  tailored recommendations for your one-way effects models only. ", a("Click here to go to CI lower", onclick = "customHref('CI lower')")),
                    status = "primary", width = 10
                ),
                box(title = p("Simulation study & how to cite", style="color:white",
                                     onclick = "customHref('Simulation results')"),
                           solidHeader = TRUE,
                           collapsible = TRUE,
                           collapsed = TRUE,
                           div("The CI width procedure and the MSE ratio procedure are based on simulation studies (Mokkink et al. 2022). All specific results of the simulations are shown ",a("here", onclick="customHref('Simulation results')"),". The CI lower limit procedure can be used in situations beyond the conditions considered in the simulation studies.", br(),br(), strong("Citation for the app:"), br(),
                              "Mokkink, L.B., de Vet, H., Diemeer, S. et al. Sample size recommendations for studies on reliability and measurement error: an online application based on simulation studies. Health Serv Outcomes Res Method (2022).", a("https://doi.org/10.1007/s10742-022-00293-9",href = "https://doi.org/10.1007/s10742-022-00293-9") ),
                           status = "primary",
                           width = 10),
                box(title = p("Design issues beyond sample size", style="color:white",
                              onclick = "customHref('FAQ & links')"),
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    collapsed = TRUE,
                    div("For assistance in comprehensively formulating your reserach question, and making other design choices, such as which patients and professionals should be included, and how to build the appropriate statistical formula for your study, we refer to our paper: ", a("Studies on Reliability and Measurement Error of Measurements in Medicine – From Design to Statistics Explained for Medical Researchers",href = "https://doi.org/10.2147/PROM.S398886") ),
                    status = "primary",
                    width = 10)
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
                                br(),  strong("Reach of procedure:"), " feasible for all conditions described in the ", a("simulation study",href = "https://doi.org/10.1007/s10742-022-00293-9"),  " as it is based on the results of the ",a("simulation", onclick="customHref('Simulation results')") )
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
                              selected = "agreement",
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

    #####MSE ratio-----
               tabPanel("MSE ratio",
                        fluidPage(
                          fluidRow(
                            box(title = h2("MSE ratio procedure"),
                                width = 12,
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                collapsed = FALSE,
                                div(strong("Phase of use:"), " during data collection when inclusion is (s)low, and you realize that the intended sample size of patients and the number of repeated measurements (as described in your protocol) cannot be reached.",br(), strong("Goal:"), " (1) Precision loss: estimate the decrease in precision when you compare the current design (i.e. the number of patients or repeated measurements included so far in the study) to the target design (i.e. as described in the protocol). (2) Recommendations: understand how the current design can be updated in terms of either number of patients or number of repeated measurements to achieve a similar precision as the target design.",br(), strong("Pre-specifications:"), " ICC or SEM model that you will use, expected ICC/SEM value, expected variance in scores.",
                                    br(),  strong("Reach of procedure:"), " feasible for all conditions described in the ",a("simulation study",href = "https://link.springer.com/article/10.1007/s10742-022-00293-9"),  " as it is based on the results of the ",a("simulation", onclick="customHref('Simulation results')"))
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
                              label = "What aspect of the design is reduced?",
                              choices = c("repeated measurements", "sample size"),
                              selected = character(0)
                            ),

                            shinyjs::useShinyjs(),
                            div(id = "designk", ##vary for raters
                                strong("Repeated measurement are reduced, specify the current and target (i.e. by procotol) numbers."),
                                br(),
                                br(),
                                radioButtons(
                                  "k_iccr",
                                  label = "number of repeated measurements in current design",
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
                                strong("Sample size is reduced, specify the current sample size and target size."),
                                br(),
                                radioButtons(
                                  "n_iccr",
                                  label = "Sample size in current design",
                                  choices = c(10, 20, 25, 30, 40, 50, 100, 200),
                                  selected = "50",
                                  inline = TRUE
                                ),
                                radioButtons(
                                  "n_iccg",
                                  label = "Sample size in target (goal) design by protocol",
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
                            strong("Target design input:"),
                            textOutput("mseratio_targetinput"),
                            strong("Current design input:"),
                            textOutput("mseratio_currentinput"),

                           # p("The figure below shows the confidence interval computed via the mean squared error for the current design and the target design (> link Mokkink et al.,  2022)."),
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
                            p( "The simulation results can be used to compare different study design scenario's. By computing the ratio of the MSE in two different scenarios, that is the current and target (defined at the start of the study) design, the ratio indicates the required change in sample size or repeated measurements for the scenario's to achieve equal precision. The decreased precision of the current design shows the loss in precision relative to the target design, when the sample size or repeated measurements cannot be further increased."
                            )
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
                          div(
                            "The methods and general results of the simulation study can be found in ", a("Mokkink et al.",href = "https://link.springer.com/article/10.1007/s10742-022-00293-9"),"."
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
                 title = "Where can I find more information on the background and results of this app?",
                 collapsible = TRUE,
                 collapsed = TRUE,
                 solidHeader = TRUE,
                 status = "success",
                 div("The app and the recommendations that you get are based on a simulation study published in", a("Health Services and Outcomes Research Methodology", href = "https://link.springer.com/article/10.1007/s10742-022-00293-9"))

               )),
             fluidRow(
               box(
                 title = "Where can I find information to decide which ICC or SEM formula matches my design?",
                 collapsible = TRUE,
                 collapsed = TRUE,
                 solidHeader = TRUE,
                 status = "success",
                 div("Many different designs are possible for studies on reliability and measurement error of measurement instruments. To decide on the design and subsequently on the appropriate ICC and SEM formula we refer to our paper published in", a("Patient Related Outcome Measures", href = "https://doi.org/10.2147/PROM.S398886")), br(),
                 div("In this app we provide recommendations for one-way random effects model (ICC (1.1)), two-way random effects models for agreement (ICC (2.1)), or the two-way mixed effects model for consistency (ICC (3.1)). By default, the two-way random effects model for agreement is selected, as in most cases in medicine this model is preferred over the other two models.")

               )),
             fluidRow(
               box(
                 title = "How can I estimate the ICCs and SEMs in R?",
                 collapsible = TRUE,
                 collapsed = TRUE,
                 solidHeader = TRUE,
                 status = "success",
                 tags$div("The R package", tags$a("Agree", href = "https://github.com/iriseekhout/Agree"), "includes all functions to obtain the ICCs and SEMs for different types of ICCs. The Agree package is publically available and can be installed in R using the following code: ", tags$code("remotes::install_github(repo = 'iriseekhout/Agree')"))
               )),
             fluidRow(
               box(
                 title = "What is an oneway, agreement or consistency type of ICC or SEM?",
                 collapsible = TRUE,
                 collapsed = TRUE,
                 solidHeader = TRUE,
                 status = "success",
                 tags$div("The oneway refers to the ICC or SEM based on an one-way random effects model (ICC (1.1)).", br(), "The agreement type refers to the ICC or SEM based on a two-way random effects models for agreement (ICC (2.1)) where all error is taken into account, thus also systematic differences between repeated measurements.", br(), "The consistency type refers to the ICC or SEM based on a two-way mixed effects model for consistency (ICC (3.1)); it excludes any systematic error between the repeated measurements and is comparable to a Peason correlation coefficient.", br(),
                          "For more information we refer to", a("Mokkink et al. 2023", href = "https://doi.org/10.2147/PROM.S398886"))
               )),
             fluidRow(
               box(
                 title = "Which procedure should I choose?",
                 collapsible = TRUE,
                 collapsed = TRUE,
                 solidHeader = TRUE,
                 status = "success",
                 tags$div("In this app you can choose between three procedures to get recommendations for your sample size and number of repeated measurements. You can use the CI width procedure when you are designing your study. If you are not sure on specific conditions or settings, you could try several scenarios. If you have conditions which are outside the scope of our simulated conditions (for example an expected correlation between raters below 0.6), or you want to get more specific recommendations about sample sizes between 50 and 200 people,  you can use the analytical approach, i.e. the CI lower limit. The CI lower limit approach gives recommendation for estimating the ICC based on a one-way random effects model, and these recommendations are generally more conservative for situations where a two-way effects model is used. In a two-way model the patient variation is estimated with more precision by disentangling variance from other sources from the scores.", br(),
                          "You can use the MSE ratio procedure when you have started the data collection, but inclusion is running slow. With this procedure, you can compare your target design (i.e. as described in the protocol) with the current situation (that is the data you were able to collect so far), and see how the precision is affected. The procedure also provides you with alternative recommendations to achieve the same precision as in the target design.")
               )),
             fluidRow(
               box(
                 title = "How do you know the expected correlation between repeated measurements?",
                 collapsible = TRUE,
                 collapsed = TRUE,
                 solidHeader = TRUE,
                 status = "success",
                 tags$div("In both the CI width procedure and in the MSE ratio procedure you need to state the expected correlation between the repeated measurements. The correlation r is equivalent to the ICC when no systematic difference between the measurements occur or when these are ignored, i.e. Pearson correlation or ICC consistency. The expected correlation is NOT the correlation you hope for, but rather the most realistic correlation for your specific instrument.", br(),
                          "You can base your expectations for this correlation on previously reported ICCs, for example results of studies on a different language version of a PROM, or on results obtained with the instrument in a slightly other patient population. If such studies don’t exist, you can obtain recommendations in multiple scenarios (such as with an correlation of 0.7 and 0.8) and find out the sensitivity of the different correlations for recommended sample sizes and number of repeated measurements.")
               )),
             fluidRow(
               box(
                 title = "How do you know whether you should expect a systematic differences expected between 0, 1 or 2 of the repeated measurements",
                 collapsible = TRUE,
                 collapsed = TRUE,
                 solidHeader = TRUE,
                 status = "success",
                 tags$div("You basically need to do a best guess. You can use information from previous research, done in studies on similar measurement instruments (e.g. see whether raters in general systematically differ with the same type of instrument), or on the same construct (e.g. see whether the patients usually change between repeated measurements of the same construct measured with the same or a similar measurement instrument). If you are not sure, you can work out different scenarios (for example with and without a systematic difference), to find out how much a systematic difference influences the precision of the ICC or SEM estimation, and subsequently influences the recommended sample size and number of repeated measurements.")
               )),
             fluidRow(
               box(
                 title = "Which ‘Target width of the 95% Confidence interval of ICC’ should I choose?",
                 collapsible = TRUE,
                 collapsed = TRUE,
                 solidHeader = TRUE,
                 status = "success",
                 tags$div("The 95% confidence interval of the ICC point estimation informs you on the precision of the ICC estimation. The width depends on the unit of measurement, and it refers to the range of the ICC, which is between 0 and 1.", br(),
                          "As default in this app, we used a target width of the confidence interval around the ICC point estimate of 0.3. That means that if we are satisfied with a 95% CI that has 0.15 below the ICC and 0.15 above the ICC. For example, suppose the true ICC is 0.7, with an appropriate width of 0.3, the recommendations for number of sample size and repeated measurements will give you a point estimate of the ICC within the range of 0.55 and 0.85.", br(),
                         "You can choose to be more strict and select a more narrow target width. For example, Zou (2012) used 0.2 as a target with for the 95% confidence interval.")
               )),
             fluidRow(
               box(
                 title = "Which ‘Target width of the 95% Confidence interval of SEM’ should I choose?",
                 collapsible = TRUE,
                 collapsed = TRUE,
                 solidHeader = TRUE,
                 status = "success",
                 tags$div("The 95% confidence interval of the SEM point estimation informs you on the precision of the SEM estimation. The width depends on the unit of measurement, and it refers to the range of the scores of the measurement. The default settings of the target width of the 95% CI of the SEM differs across the variance conditions. When the variance in the scores increase, then the SEM increases by definition and so does the target width of the conficence interval around SEM. Per default, the 95% CI width is 0.3 when the variance is 1. The default target with increases with the unit of measurement set (as the variance),")
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


