dashboardPage(skin = "blue",
              dashboardHeader(title = "gPKPDviz v3.3.0", titleWidth = 200),
              dashboardSidebar(
                width = 200,
                sidebarMenu(
                  id = "genpksidebarmenu",
                  menuItem("Model", 
                           tabName = "model",
                           icon = icon("list-alt")),
                  menuItem("Population",
                           tabName = "tabsettings", 
                           icon = icon("users")),
                  menuItem("Dosing", 
                           tabName = "tabdosing",
                           icon = icon("eyedropper")),
                  menuItem("Simulate",
                           tabName = "simulation",
                           icon = icon("equalizer", lib = "glyphicon")),
                  menuItem("Results",
                           tabName = "results",
                           icon = icon("bar-chart")),
                  tags$br(),
                  actionButton("sessionInfoButton", "Session", class = "btn-small"),
                  tags$br(),
                  tags$br(),
                  fluidRow(
                    column(
                      width = 7,
                      offset = 1,
                      uiOutput("constantsUI")
                    )
                  )
                )
              ),
              
              dashboardBody(
                tags$head(
                  tags$link(rel="shortcut icon", href="favicon.ico")
                ),
                
                includeCSS("genpk.css"),
                includeScript("genpk.js"),
                
                # Hidden items to control display
                conditionalPanel(
                  "2==1",
                  checkboxInput("viewModelBuildButton", "", FALSE),
                  checkboxInput("viewModelSummary", "", FALSE),
                  checkboxInput("viewPop", "", FALSE),
                  checkboxInput("viewDosing", "", FALSE),
                  checkboxInput("viewPopDataSummary", "", FALSE),
                  checkboxInput("viewDosingSummary", "", FALSE),
                  checkboxInput("viewSimTab", "", FALSE),
                  checkboxInput("viewResTab", "", FALSE),
                  checkboxInput("viewSimRes", "", FALSE),
                  checkboxInput("viewModelSims", "", FALSE),
                  checkboxInput("userHasOverlayData", FALSE),
                  checkboxInput("viewModelSimulateButton", "", FALSE),
                  checkboxInput("hideUserDefinedTimes", "", FALSE)
                ),
                tabItems(
                  # Opening parens ----------------------------------------------------------
                  
                  # Model Tab ---------------------------------------------------------------
                  tabItem(
                    tabName = "model",
                    conditionalPanel(
                      "2==1",
                      # "input.viewModelSummary",
                      fluidRow(
                        column(
                          width = 12,
                          tags$div(
                            class = "alert alert-info alert-dismissible",
                            role = "alert",
                            tagList(
                              tags$button(
                                type="button",
                                class="close",
                                `data-dismiss`="alert",
                                `aria-label`="Close",
                                tags$span(
                                  `aria-hidden` ="true",
                                  "x"
                                )
                              ),  icon("users"), "Population ", " and ", icon("eyedropper"), "Dosing tabs now available"
                            )
                          )
                        )
                      )
                    ),
                    fluidRow(
                      column(
                        width = 4,
                        box(
                          title = "Model Input",
                          width=NULL,
                          solidHeader = TRUE,
                          status = "primary",
                          radioButtons(
                            inputId = "modelLoadType",
                            label = "Model Input",
                            selected = "useLibrary",
                            choices = c("Models Library" = "useLibrary",
                                        "Upload Model" = "uploadModel"),
                            inline = TRUE
                          ),
                          tags$br(),
                          conditionalPanel(
                            condition = "input.modelLoadType == 'useLibrary'",
                            selectInput(
                              inputId = "modelSelection",
                              label = "Select from Models Library",
                              choices = c("", gsub(".cpp", "", models)),
                              width = "100%"
                            )
                            
                          ),
                          conditionalPanel(
                            condition = "input.modelLoadType == 'uploadModel'",
                            fileInput(
                              inputId = "modelUploadFile",
                              label = "Upload Model (c++ file)",
                              accept = c(".cpp")
                            )
                          )
                        ),
                        conditionalPanel(
                          "input.viewModelSummary",
                          box(
                            width = NULL,
                            title = "Options",
                            status = "primary",
                            solidHeader = TRUE,
                            fluidRow(
                              column(
                                width = 2,
                                conditionalPanel(
                                  "!input.compileOnEdit",
                                  tags$i(class="fa fa-gear pull-right",
                                         style="font-size:24px")
                                ),
                                conditionalPanel(
                                  "input.compileOnEdit",
                                  tags$i(class="fa fa-gear fa-spin pull-right",
                                         style="font-size:24px")
                                )
                              ),
                              column(
                                width = 4,
                                class = "pull-left",
                                checkboxInput(
                                  "compileOnEdit",
                                  "Compile on edit",
                                  value = TRUE
                                )
                              ),
                              column(
                                width = 6,
                                actionButton(
                                  class = "btn-block",
                                  inputId = "buildModel", 
                                  label = "Compile",
                                  icon = icon("cog")
                                )
                              )
                            ),
                            fluidRow(
                              column(
                                width = 6,
                                downloadButton(
                                  class = "btn-block",
                                  outputId = "downloadModel",
                                  label = "Model Code"
                                )
                              ),
                              column(
                                width = 6,
                                actionButton(
                                  class = "btn-block",
                                  inputId = "updateCovariates", 
                                  label = HTML("Covariates &<br>Time Unit"),
                                  icon = icon("refresh")
                                )
                              )
                            ),
                            tags$br(),
                            selectInput(
                              "appSeedType",
                              "Seed",
                              choices = c("Random" = "Random", "User Specified" = "user")
                            ),
                            tags$i("*Set seed to a specific value to make results reproducible"),
                            conditionalPanel(
                              condition = "input.appSeedType == 'user'",
                              numericInput(
                                "appSeed",
                                "Seed Value",
                                value = 99
                              ),
                              actionButton(
                                class = "btn-block",
                                "setSeed",
                                "Set Seed",
                                icon = icon("gavel")
                              )
                            )
                          )
                        ),
                        conditionalPanel(
                          "input.viewModelSummary",
                          box(
                            width = NULL,
                            title = "Model Summary",
                            status = "info",
                            solidHeader = TRUE,
                            uiOutput("modelSummaryUI")
                          )
                        )
                      ),
                      conditionalPanel(
                        "input.viewModelSummary",
                        column(
                          width = 8,
                          box(
                            width = NULL,
                            title = "Model (Editable)",
                            status = "info",
                            solidHeader = TRUE,
                            uiOutput("modelName"),
                            aceEditor(outputId = "modelCodeEditor",
                                      value = "",
                                      mode = "c_cpp",
                                      height = "950px",
                                      readOnly = FALSE)
                          )
                        )
                      )
                    )
                  ),
                  # Population Tab ----------------------------------------------------------
                  tabItem(
                    tabName = "tabsettings",
                    conditionalPanel(
                      "input.viewPop",
                      conditionalPanel(
                        "input.modelCovariates != 'no_covariates'",
                        fluidRow(
                          column(
                            width = 12,
                            box(
                              title = "Data Input Type",
                              width=NULL,
                              solidHeader = TRUE,
                              status = "primary",
                              radioButtons(
                                inputId = "popDataType",
                                label = "Population Data",
                                choices = c("Simulate Dataset" = "simPop",
                                            "Upload Dataset" = "uploadPop"),
                                inline = TRUE
                              )
                            )
                          )
                        ),
                        fluidRow(
                          column(
                            width = 12,
                            conditionalPanel(
                              "input.popDataType == 'uploadPop'",
                              box(
                                title = "Upload Data Set",
                                width = NULL,
                                solidHeader = TRUE,
                                status="primary",
                                uiOutput('uploadedPopDataInput')
                              )
                            ),
                            conditionalPanel(
                              "input.popDataType == 'simPop'",
                              conditionalPanel(
                                "!input.nPopDataSim",
                                fluidRow(
                                  column(
                                    width = 1,
                                    tags$i(class="fa fa-spinner fa-spin pull-right",
                                           style="font-size:24px")
                                  ),
                                  column(
                                    width = 11,
                                    style="font-size:24px",
                                    "Loading interface to simulate population..."
                                  )
                                )
                                
                              ),
                              fluidRow(
                                column(
                                  width = 3,
                                  uiOutput("covariateCatTypesUI")
                                ),
                                column(
                                  width = 4,
                                  uiOutput("covariateContTypesUI")
                                ),
                                column(
                                  width = 3,
                                  box(
                                    width = NULL,
                                    title = "Additional Options",
                                    checkboxInput(
                                      inputId = "splitCovariates",
                                      label = "Split Continuous Covariates by Categorical Covariates",
                                      value = FALSE
                                    ),
                                    "Correlation Matrix",
                                    rhandsontable::rHandsontableOutput("correlationMatrix", width = "90%")
                                  )
                                ),
                                column(
                                  width = 2,
                                  box(
                                    width = NULL,
                                    title = "Generate Data",
                                    solidHeader = TRUE,
                                    status = "primary",
                                    textInput(
                                      inputId = "nPopDataSim",
                                      label = "Number of Subjects to Simulate",
                                      width = "100%",
                                      value = "100",
                                      placeholder = "100"
                                    ),
                                    actionButton(
                                      class = "btn-block btn-lg",
                                      inputId = "generatePopData",
                                      label = tagList(tags$i(class="fa fa-gear fa-spin",
                                                             style="font-size:24px"), "Simulate"),
                                      `data-dismiss`="modal"#,
                                      #icon = icon("equalizer", lib = "glyphicon")
                                    )
                                  )
                                )
                              )
                            )
                          )
                        ),
                        conditionalPanel(
                          "input.viewPopDataSummary",
                          fluidRow(
                            column(
                              width = 9,
                              box(
                                title = "Population Data",
                                solidHeader = TRUE,
                                status = "primary",
                                width = NULL,
                                fluidRow(
                                  column(
                                    width = 6,
                                    uiOutput("popDataSummary")
                                  ),
                                  column(
                                    width = 6, 
                                    tabBox(
                                      width = NULL,
                                      tabPanel(
                                        "Data",
                                        DTOutput("cov_ex_data"),
                                        tags$hr(),
                                        fluidRow(
                                          column(
                                            width = 2,
                                            offset = 10,
                                            downloadButton(class = 'pull-right btn-lg',
                                                           outputId = 'downloadPopData',
                                                           label = 'Download Data')
                                          )
                                        )
                                      ),
                                      tabPanel("Baseline Values",
                                               plotOutput("cov_ex_data_baseline_hist", height = 600)
                                      ),
                                      tabPanel("Values Across Time",
                                               plotOutput("cov_ex_data_time_var", height = 600)
                                      )
                                    )
                                  )
                                )
                              )
                            ),
                            column(
                              width = 3,
                              box(
                                width = NULL,
                                solidHeader = TRUE,
                                status = "primary",
                                title = "Sample From Initial Population Data",
                                tags$br(),
                                uiOutput("numPatientsFull"),
                                tags$br(),
                                tags$br(),
                                textInput(
                                  inputId = "sampleFromPopSize",
                                  label = "Number Subjects to Sample",
                                  value = ""
                                ),
                                conditionalPanel(
                                  "input.sampleFromPopSize",
                                  actionButton(
                                    class = 'btn-lg btn-block',
                                    inputId = "sampleFromPop",
                                    label = tagList(
                                      tags$i(class="fa fa-circle-o-notch fa-spin", style = "font-size:24px"),
                                      "Sample"
                                    )
                                  )
                                ),
                                conditionalPanel(
                                  "!input.sampleFromPopSize",
                                  tags$b(style="color:blue", "Enter Number of Samples")
                                ),
                                tags$br(),
                                fluidRow(
                                  column(
                                    width = 12,
                                    HTML("<span style='font-size:12px'><b>*With replacement</b> used when <b>Number Subjects to Sample</b> > <b>Number Subjects in Initial Data</b></span>")
                                  )
                                )
                              ),
                              box(
                                width = NULL,
                                solidHeader = TRUE,
                                status = "primary",
                                title = "Create Bin Columns for Continuous Covars",
                                uiOutput("continuousCovariateBinningUI")
                              )
                            )
                          )
                        )
                      ),
                      conditionalPanel(
                        "input.modelCovariates == 'no_covariates'",
                        fluidRow(
                          column(
                            width = 4,
                            box(
                              width = NULL,
                              title = "Number of Subjects (no covariates)",
                              status = "primary",
                              solidHeader = TRUE,
                              numericInput(
                                inputId = "nPatientsNoCovars",
                                label = "Number of Subjects in Population",
                                value = 100
                              ),
                              actionButton(
                                class = "btn-block",
                                inputId = "lockInNoCovarPop",
                                label = "Apply",
                                icon = icon("gavel")
                              )
                            )
                          )
                        ),
                        conditionalPanel(
                          "input.lockInNoCovarPop > 0",
                          fluidRow(
                            column(
                              width = 4,
                              box(
                                width = NULL,
                                title = "Population Data (no covariates)",
                                status = "info",
                                solidHeader = TRUE,
                                DTOutput("cov_ex_data_no_covar")
                              )
                            )
                          )
                        )
                      )
                    ),
                    conditionalPanel(
                      "!input.viewPop",
                      fluidRow(
                        column(
                          width = 12,
                          tags$div(
                            class = "alert alert-info alert-dismissible",
                            role = "alert",
                            tagList(
                              tags$button(
                                type="button",
                                class="close",
                                `data-dismiss`="alert",
                                `aria-label`="Close",
                                tags$span(
                                  `aria-hidden` ="true",
                                  "x"
                                )
                              ),  "This tab is not available until the  ", icon("list-alt"), "Model tab is complete."
                            )
                          )
                        )
                      )
                    )
                  ),
                  
                  # Dosing Tab --------------------------------------------------------------
                  tabItem(
                    tabName = "tabdosing",
                    conditionalPanel(
                      "input.viewDosing",
                      fluidRow(
                        column(
                          width = 12,
                          box(
                            title = "Data Input Type",
                            width = NULL,
                            solidHeader = TRUE,
                            status = "primary",
                            radioButtons(
                              inputId = "doseDataType",
                              label = "Dose Schedule",
                              choices = c("Build Using Interface" = "buildDose",
                                          "Upload Schedule" = "uploadDose"),
                              inline = TRUE
                            )
                          )
                        )
                      ),
                      conditionalPanel(
                        condition = "input.doseDataType == 'uploadDose'",
                        fluidRow(
                          column(
                            width = 12,
                            uiOutput("dosingScheduleUploadUI")
                          )
                        )
                      ),
                      conditionalPanel(
                        condition = "input.doseDataType == 'buildDose'",
                        fluidRow(
                          column(
                            width = 5,
                            box(
                              width = NULL,
                              solidHeader = TRUE,
                              collapsible = FALSE,
                              status = "primary",
                              title = "Construct Dose Interventions (up to three unique)",
                              tabBox(
                                width = NULL,
                                tabPanel("Intervention A",
                                         .d1$make_ui(title="Period A intervention",collapsed=FALSE)
                                ),
                                tabPanel("Intervention B",
                                         .d2$make_ui(title="Period B intervention",collapsed=FALSE)
                                ),
                                tabPanel("Intervention C",
                                         .d3$make_ui(title="Period C intervention",collapsed=FALSE)
                                )
                              )
                            )
                            # )
                          ),
                          # fluidRow(
                          column(
                            width = 7,
                            box(
                              width = NULL,
                              title = "Intervention Sequence",
                              solidHeader = TRUE,
                              status = "primary",
                              fluidRow(
                                column(
                                  width = 4,
                                  .p1$make_ui("First Intervention", 1)
                                ),
                                conditionalPanel(
                                  condition = "input.first__period > 0",
                                  column(
                                    width = 4,
                                    .p2$make_ui("Second Intervention", 0)
                                  )),
                                conditionalPanel(
                                  condition = "input.second__period > 0",
                                  column(
                                    width = 4,
                                    .p3$make_ui("Third Intervention", 0)
                                  ))),
                              tags$hr(),
                              fluidRow(
                                column(
                                  width = 4,
                                  offset = 8,
                                  actionButton(
                                    class = "btn-lg btn-block",
                                    inputId = "lockInDoses",
                                    label = tagList(
                                      tags$i(class="fa fa-gear fa-spin", style = "font-size:24px"),
                                      HTML("Apply")
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      ),
                      conditionalPanel(
                        "input.viewDosingSummary",
                        fluidRow(
                          column(
                            width = 12,
                            box(
                              width = NULL,
                              title = "Applied Dosing Sequence Table",
                              solidHeader = TRUE,
                              status = "info",
                              fluidRow(
                                column(
                                  width = 12,
                                  DT::dataTableOutput("doseTable")
                                )
                              ),
                              tags$br(),
                              fluidRow(
                                column(
                                  width = 12,
                                  uiOutput("totalNDoses")
                                )
                              )
                            )
                          )
                        )
                      )
                    ),
                    conditionalPanel(
                      "!input.viewDosing",
                      fluidRow(
                        column(
                          width = 12,
                          tags$div(
                            class = "alert alert-info alert-dismissible",
                            role = "alert",
                            tagList(
                              tags$button(
                                type="button",
                                class="close",
                                `data-dismiss`="alert",
                                `aria-label`="Close",
                                tags$span(
                                  `aria-hidden` ="true",
                                  "x"
                                )
                              ),  "This tab is not available until the  ", icon("users"), "Population tab is complete."
                            )
                          )
                        )
                      )
                    )
                  ),
                  # Simulation Tab ----------------------------------------------------------
                  tabItem(
                    tabName = "simulation",
                    conditionalPanel(
                      "input.viewSimTab",
                      fluidRow(
                        column(
                          width = 9,
                          box(
                            width = NULL,
                            title = "Simulation Specifications",
                            status = "primary",
                            solidHeader = TRUE,
                            fluidRow(
                              column(
                                width = 4,
                                # tags$b("Number of IDs to Simulate (per Pop. data):"),
                                uiOutput("numIDsSim")
                              ),
                              column(
                                width = 4,
                                numericInput(
                                  inputId = "obs_interval",
                                  label = "Sampling Interval",
                                  min = 0,
                                  max = Inf,
                                  value = .5,
                                  step = .5,
                                  width = "100%"
                                ),
                                numericInput(
                                  inputId = "max_obs_time",
                                  label = "Sampling Duration",
                                  min = 1,
                                  max = 365, 
                                  value = 5,
                                  width = "100%"
                                )
                              ),
                              column(
                                width = 4,
                                selectInput(
                                  inputId = "obs_time_unit",
                                  label = "In",
                                  choices = c("days" = "day", "hours" = "hour"),
                                  selected = "hour"
                                ),
                                selectInput(
                                  inputId = "max_obs_time_unit",
                                  label = "In",
                                  choices = c("weeks" = "week", "days" = "day", "hours" = "hour"),
                                  selected = "day"
                                )
                              )
                            ),
                            fluidRow(
                              column(
                                offset = 4,
                                width = 8,
                                box(
                                  width = NULL,
                                  status = "primary",
                                  title = "Additional (Optional) Inputs",
                                  collapsible = TRUE,
                                  collapsed = TRUE,
                                  conditionalPanel(
                                    "!input.hideUserDefinedTimes",
                                  fluidRow(
                                    column(
                                      width = 4,
                                      textInput(
                                        inputId = "user_sample_times",
                                        label = "User Specified Sampling Times (space separated)",
                                        placeholder =  "0 1 2 5 8 12 24"
                                      )
                                    ),
                                    column(
                                      width = 4,
                                      selectInput(
                                        inputId = "user_sample_times_relative_to",
                                        label = "Relative To",
                                        choices = c("Time Zero (each value used once)" = "time_zero",
                                                    "Most Recent Dose (each value repeats for each dose)" = "last_dose")
                                      )
                                    ),
                                    column(
                                      width = 4,
                                      selectInput(
                                        inputId = "user_sample_times_unit",
                                        label = "In",
                                        choices = c("hours" = "hour", "days" = "day")
                                      )
                                    )
                                  )
                                  ),
                                conditionalPanel(
                                  "input.hideUserDefinedTimes",
                                  fluidRow(
                                    column(
                                      width = 12,
                                      tags$b("**Non-dosing rows used as additional observation time points in the simulated data.**")
                                  )
                                  )
                                ),
                                  fluidRow(
                                    column(
                                      width = 6,
                                      checkboxInput(
                                        "doZeroRe",
                                        "PRED Simulation (No random effects or residual error)",
                                        value = FALSE
                                      )
                                    )
                                  ),
                                  fluidRow(
                                    column(
                                      width = 6,
                                      sliderInput(
                                        "aTolValue",
                                        "Absolute Solver Tolerance Value (1e-'Value')",
                                        min = 1,
                                        max = 50,
                                        step = 1,
                                        value = 30
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        ),
                        column(
                          width = 3,
                          box(
                            width = NULL,
                            title = "Run Simulation",
                            status = "primary",
                            solidHeader = TRUE,
                            fluidRow(
                              column(
                                width = 12,
                                textInput(
                                  inputId = "sim_id",
                                  value = "Sim-1",
                                  width = "100%",
                                  label = "Simulation Identifier (SIM_ID)")
                              )
                            ),
                            tags$hr(),
                            fluidRow(
                              column(
                                width = 12,
                                actionButton(
                                  class = "btn-lg btn-block",
                                  inputId = "simulate",
                                  label = tagList(
                                    tags$i(class="fa fa-cog fa-spin", style = "font-size:24px"),
                                    "Simulate"
                                  )
                                )
                              )
                            ),
                            tags$br()
                          )
                        )
                      ),
                      conditionalPanel(
                        condition = "input.viewModelSims",
                        fluidRow(
                          column(
                            width = 12,
                            box(
                              width = NULL,
                              status = "info",
                              solidHeader = TRUE,
                              title = "Simulation History",
                              fluidRow(
                                column(
                                  width = 10,
                                  checkboxGroupInput(
                                    inputId = "simPlotIds",
                                    label = "Simulation IDs",
                                    choices = NULL,
                                    inline = TRUE
                                  )
                                ),
                                column(
                                  width = 2,
                                  actionButton(
                                    class = "pull-right btn-block btn-lg",
                                    "clean_plot_id",
                                    "Clear Checked",
                                    icon = icon("window-close-o")
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    ),
                    conditionalPanel(
                      "!input.viewSimTab",
                      fluidRow(
                        column(
                          width = 12,
                          tags$div(
                            class = "alert alert-info alert-dismissible",
                            role = "alert",
                            tagList(
                              tags$button(
                                type="button",
                                class="close",
                                `data-dismiss`="alert",
                                `aria-label`="Close",
                                tags$span(
                                  `aria-hidden` ="true",
                                  "x"
                                )
                              ),  "This tab is not available until the  ", icon("list-alt"), "Model, ", icon("users"), "Population, ", " and ", icon("eyedropper"), "Dosing tabs are complete."
                            )
                          )
                        )
                      )
                    )
                  ),
                  
                  # Results Tab -------------------------------------------------------------
                  tabItem(
                    tabName = "results",
                    conditionalPanel(
                      "!input.viewResTab",
                      fluidRow(
                        column(
                          width = 12,
                          tags$div(
                            class = "alert alert-info alert-dismissible",
                            role = "alert",
                            tagList(
                              tags$button(
                                type="button",
                                class="close",
                                `data-dismiss`="alert",
                                `aria-label`="Close",
                                tags$span(
                                  `aria-hidden` ="true",
                                  "x"
                                )
                              ),  "This tab is not available until the  ", icon("equalizer", lib = "glyphicon"), "Simulate tab is complete."
                            )
                          )
                        )
                      )
                    ),
                    conditionalPanel(
                      "input.viewResTab",
                      fluidRow(
                        column(
                          width = 12,
                          box(
                            width = NULL,
                            title = "Data",
                            status = "info",
                            solidHeader = TRUE,
                            shinycssloaders::withSpinner(DTOutput("simData"))
                          )
                        )
                      )
                    ),
                    conditionalPanel(
                      "!input.viewSimRes",
                      fluidRow(
                        column(
                          width = 1,
                          tags$i(class="fa fa-spinner fa-spin pull-right",
                                 style="font-size:24px")
                        ),
                        column(
                          width = 11,
                          style="font-size:24px",
                          "Loading Simulation Results Interface..."
                        )
                      )
                    ),
                    conditionalPanel(
                      "input.viewSimRes",
                      fluidRow(
                        column(
                          width = 12,
                          box(
                            width = NULL,
                            title = "Visualizations and Summaries",
                            status = "info",
                            solidHeader = TRUE,
                            conditionalPanel(
                              "input.updateResults == 0",
                              tags$div(
                                class = "yellowselect alert",
                                HTML('Press <i class="fa fa-spin fa-refresh"></i> Update Results to generate results')
                              )
                            ),
                            fluidRow(
                              column(
                                offset = 5,
                                width = 4,
                                tags$div(
                                  class = "blueselect alert",
                                  fluidRow(
                                    column(
                                      width = 12,
                                      tags$h5("Filtering the ",
                                              tags$b('Data'),
                                              " (above) applies to results")
                                    )
                                  )
                                )
                              ),
                              column(
                                width = 3,
                                actionButton(class = "btn-block btn-lg",
                                             style = "color: #fff;background: #00c0ef;background-color: #00c0ef;",
                                             "updateResults",
                                             label = HTML('<i class="fa fa-spin fa-refresh"></i> Update Results')
                                )
                              )
                            ),
                            conditionalPanel(
                              "input.updateResults > 0",
                              tabsetPanel(
                                tabPanel(
                                  "Simulations",
                                  fluidRow(
                                    column(
                                      width = 6,
                                      radioButtons(
                                        inputId = "simulationsPlotPage",
                                        label = "Page",
                                        choices = ""#,
                                        #inline = TRUE
                                      )
                                    ),
                                    column(
                                      width = 6,
                                      tabBox(
                                          width = NULL,
                                          tabPanel(
                                            "Options",
                                            fluidRow(
                                              column(
                                                width = 3,
                                                numericInput(
                                                  "simulationsPlotNPerPage",
                                                  "Max N Per Page", 
                                                  value = 9, 
                                                  min = 1, 
                                                  step = 1
                                                )
                                              ),
                                              column(
                                                width = 3,
                                                numericInput(
                                                  "simulationsPlotNRowPerPage",
                                                  "N Row Per Page", 
                                                  value = 3, 
                                                  min = 1, 
                                                  step = 1
                                                )
                                              ),
                                              column(
                                                width = 6,
                                                checkboxInput(
                                                  "simulationsPlotIncludeDoses",
                                                  "Show dose events",
                                                  value = FALSE
                                                )
                                              )
                                            )
                                          ),
                                          tabPanel(
                                            "Display",
                                            fluidRow(
                                              column(
                                                width = 4,
                                                checkboxGroupInput(
                                                  inputId = "SimulationDisplay",
                                                  "Simulations",
                                                  choices = c(
                                                    "Lines" = "lines",
                                                    "Percentiles" = "percentiles",
                                                    "Points" = "points"
                                                  ),
                                                  selected = "lines"
                                                )
                                              ),  
                                              column(
                                                width = 4,
                                                checkboxGroupInput(
                                                  inputId = "User_SpecifiedDisplay",
                                                  "User Specified",
                                                  choices = c(
                                                    "Lines" = "lines",
                                                    "Percentiles" = "percentiles",
                                                    "Points" = "points"
                                                  ),
                                                  selected = "points"
                                                )
                                              ),
                                              column(
                                                width = 4,
                                                checkboxGroupInput(
                                                  inputId = "User_OverlayDisplay",
                                                  "User Overlay",
                                                  choices = c(
                                                    "Lines" = "lines",
                                                    "Percentiles" = "percentiles",
                                                    "Points" = "points"
                                                  ),
                                                  selected = "lines"
                                                )
                                              )
                                            ),
                                            fluidRow(
                                              column(
                                                width = 10,
                                                sliderInput(
                                                  inputId = "simulationsPlotRibbonPercentile",
                                                  label = "Percentile Interval",
                                                  min = .5,
                                                  max = 1,
                                                  step = .05,
                                                  value = .9
                                                )
                                              )
                                            ),
                                            fluidRow(
                                              column(
                                                width = 12,
                                                checkboxInput(
                                                  inputId = "simulationsPlotIncludeMedian",
                                                  label = "Include Median Line With Percentiles (dashed)",
                                                  value = TRUE
                                                )
                                              )
                                            ),
                                            fluidRow(
                                              column(
                                                width = 12,
                                                numericInput(
                                                  inputId = "simulationsPlotRef",
                                                  label = "Reference line",
                                                  value = 0,
                                                  width = "50%"
                                                )
                                              )
                                            )
                                          ),
                                          tabPanel(
                                            "Axis",
                                            radioButtons(inputId="simulationsPlotScale", 
                                                         label = "Y-Axis Scale", 
                                                         choices = c("Linear", "Log"), inline = TRUE),
                                            checkboxGroupInput(
                                              inputId = "simulationsPlotAxisFree",
                                              label = "Summarize by Plots (Axis Limits Overrides this Option)",
                                              choices = c("Share X-Axis" = "share_x",
                                                          "Share Y-Axis" = "share_y"),
                                              selected = c("share_x", "share_y"),
                                              inline = TRUE
                                            ),
                                            fluidRow(
                                              column(
                                                width = 6,
                                                numericInput(
                                                  inputId = "simulationsPlotXMin",
                                                  label = "X-Axis Min",
                                                  value = NULL
                                                )
                                              ),
                                              column(
                                                width = 6,
                                                numericInput(
                                                  inputId = "simulationsPlotXMax",
                                                  label = "X-Axis Max",
                                                  value = NULL
                                                )
                                              )
                                            ),
                                            fluidRow(
                                              column(
                                                width = 6,
                                                numericInput(
                                                  inputId = "simulationsPlotYMin",
                                                  label = "Y-Axis Min",
                                                  value = NULL
                                                )
                                              ),
                                              column(
                                                width = 6,
                                                numericInput(
                                                  inputId = "simulationsPlotYMax",
                                                  label = "Y-Axis Max",
                                                  value = NULL
                                                )
                                              )
                                            )
                                          )
                                      )
                                    )
                                  ),
                                  fluidRow(
                                    column(
                                      width = 2,
                                      box(
                                        width = NULL,
                                        status = "info",
                                        title = "Configure",
                                        radioButtons(
                                          inputId = "simulationsPlotDataTimeUnit",
                                          label = "Time Unit (X-Axis)",
                                          choices = c("Hour" =  "hour",
                                                      "Day" = "day",
                                                      "Week" = "week", 
                                                      "Month" = "month")
                                        ),
                                        selectInput(
                                          inputId = "simulationsPlotDataVar",
                                          label = "Variable(s) (Y-Axis)",
                                          choices = NULL,
                                          selected = NULL,
                                          multiple = TRUE
                                        ),
                                        checkboxGroupInput(
                                          inputId = "simulationsPlotDataSummarize",
                                          label = "Summarize by",
                                          choices = NULL#,
                                          # selected = NULL,
                                          # multiple = TRUE
                                        ),
                                        selectInput(
                                          inputId = "simulationsPlotDataColor",
                                          label = "Color by",
                                          choices = NULL,
                                          selected = NULL
                                        )
                                      )
                                    ),
                                    column(
                                      width = 10,
                                      shinycssloaders::withSpinner(uiOutput("simulationsPlotUI"))
                                    )
                                    
                                  )
                                ),
                                tabPanel(
                                  "Summary Stats",
                                  tags$br(),
                                  fluidRow(
                                    column(
                                      width = 7,
                                      box(
                                        width = NULL,
                                        title = "Configure",
                                        status = "info",
                                        fluidRow(
                                          column(
                                            width = 2,
                                            checkboxGroupInput(
                                              inputId = "summaryStatsPlotDataStats",
                                              label = HTML("Stats<br>Display"),
                                              choices = summaryStatsPlotDataStatsChoices,
                                              selected = summaryStatsPlotDataStatsChoices[c(1,3)]
                                            )
                                          ),
                                          column(
                                            width = 2,
                                            checkboxGroupInput(
                                              inputId = "summaryStatsPlotDataStatsType",
                                              label = HTML("Stats<br>Type"),
                                              choices = summaryStatsPlotDataStatsTypeChoices,
                                              selected = summaryStatsPlotDataStatsTypeChoices[1]
                                            )
                                          ),
                                          column(
                                            width = 3,
                                            selectInput(
                                              inputId = "summaryStatsPlotDataVar",
                                              label = "Variable",
                                              choices = NULL,
                                              selected = NULL
                                            )
                                          ),
                                          column(
                                            width = 3,
                                            checkboxGroupInput(
                                              inputId = "summaryStatsPlotDataSummarize",
                                              label = "Summarize by",
                                              choices = NULL#,
                                              # selected = NULL,
                                              #multiple = TRUE
                                            )
                                          ),
                                          conditionalPanel(
                                            "input.summaryStatsPlotDataSummarize.length > 0",
                                            column(
                                              width = 2,
                                              radioButtons(
                                                inputId = "summaryStatsPlotDataColor",
                                                label = "Color by",
                                                choices = "SIM_ID"
                                              )
                                            )
                                          )
                                        )
                                      )
                                    ),
                                    column(
                                      width = 5,
                                      box(
                                        width = NULL,
                                        collapsible = TRUE,
                                        collapsed = TRUE,
                                        status="info",
                                        title = "Summary & Plot Options",
                                        tabsetPanel(
                                          tabPanel(
                                            "General",
                                            checkboxInput(
                                              inputId="summaryStatsPlotJitter",
                                              label = "Include points (jitter)",
                                              value = TRUE
                                            )
                                          ),
                                          tabPanel(
                                            "Thresholds",
                                            tags$br(),
                                            fluidRow(
                                              tagList(
                                                lapply(
                                                  threshStats, function(x){   
                                                    column(
                                                      width = 6,
                                                      textInput(
                                                        inputId = paste0(x, "Thresh"),
                                                        label = paste(toupper(x), " Threshold"),
                                                        value = "",
                                                        placeholder = "e.g. 5"
                                                      )
                                                    )
                                                  }
                                                )
                                              )
                                            )
                                          ),
                                          tabPanel(
                                            "Axis",
                                            tags$br(),
                                            radioButtons(inputId="summaryStatsPlotLog",
                                                         label = "Y-Axis Scale",
                                                         choices = c("Linear", "Log"), inline = TRUE)
                                          )
                                        )
                                      )
                                    )
                                  ),
                                  fluidRow(
                                    column(
                                      width = 12,
                                      tags$h3("Summary Stats Table")
                                    )
                                  ),
                                  fluidRow(
                                    column(
                                      width = 12,
                                      div(DTOutput("summaryStatsAvgs"), style = "font-size:130%")
                                    )
                                  ),
                                  tags$hr(),
                                  fluidRow(
                                    column(
                                      width = 12,
                                      tags$h3("Visualization"),
                                      plotOutput("summaryStatsPlot", 
                                                 height = "900px",
                                                 width = "100%")
                                    )
                                  ),
                                  tags$hr(),
                                  fluidRow(
                                    column(
                                      width = 12,
                                      tags$h3("Subject (ID) Level Data"),
                                      DTOutput("summaryStatsData")
                                    )
                                  )
                                ),
                                tabPanel(
                                  "Upload Overlay Data",
                                  fluidRow(
                                    column(
                                      width = 5,
                                      tags$br(),
                                      uiOutput("overlayDataInfo")
                                    ),
                                    column(
                                      width = 7,
                                      tags$br(),
                                      tags$br(),
                                      fileInput(
                                        inputId = "overlayData", 
                                        label = "Select file (csv)",
                                        # width = "100%",
                                        accept = c(
                                          "text/csv",
                                          "text/comma-separated-values,text/plain",
                                          ".csv")
                                      )
                                    )
                                  )
                                ),
                                tabPanel(
                                  "View Inputs",
                                  tags$br(),
                                  fluidRow(
                                    column(
                                      width = 6,
                                      selectInput(
                                        "simIDViewInput",
                                        "Select SIM_ID to View",
                                        choices = NULL
                                      )
                                    )
                                  ),
                                  fluidRow(
                                    column(
                                      width = 12,
                                      tabBox(
                                        width = NULL,
                                        tabPanel(
                                          "Model Code",
                                          fluidRow(
                                            column(
                                              width = 12,
                                              aceEditor(outputId = "modelCodeInputs",
                                                        value = "",
                                                        mode = "c_cpp",
                                                        height = "950px",
                                                        readOnly = TRUE)
                                            )
                                          )
                                        ),
                                        tabPanel(
                                          "Population Data",
                                          fluidRow(
                                            column(
                                              width = 12,
                                              DTOutput("populationDataInputs")
                                            )
                                          )
                                        ),
                                        tabPanel(
                                          "Dosing Table",
                                          fluidRow(
                                            column(
                                              width = 12,
                                              DTOutput("doseTableInputs")
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                ),
                                tabPanel(
                                  "Download",
                                  tags$br(),
                                  fluidRow(
                                    column(
                                      width = 6,
                                      box(
                                        width = NULL,
                                        title = "Select SIM_ID(s) Inputs To Download",
                                        status = "info",
                                        checkboxGroupInput(
                                          "downloadSelectedSimID",
                                          "Simulation IDs",
                                          choices = NULL
                                        )
                                      )
                                    ),
                                    column(
                                      width = 6,
                                      box(
                                        width = NULL,
                                        status = "info",
                                        title = "Download Results and Selected Inputs",
                                        conditionalPanel(
                                          "input.downloadSelectedSimID.length > 0",
                                          textInput("zipName", label = "File Name", value = "GenPK-results"),
                                          conditionalPanel(
                                            "input.zipName",
                                            downloadLink(class = 'btn btn-default btn-lg',
                                                         "exportResultsDownload",
                                                         "Download Zip")
                                          )
                                        ),
                                        conditionalPanel(
                                          "input.downloadSelectedSimID.length == 0",
                                          tags$i(style='color:red', "Select at least one SIM_ID")
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
)
