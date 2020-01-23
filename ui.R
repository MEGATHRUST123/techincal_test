
ui <- dashboardPage(
  dashboardHeader(title = "Capacity Study and Planning", titleWidth = 300),
  dashboardSidebar(width = 150,
                   tags$head(tags$style(HTML('.skin-blue .main-header .logo {
                                             background-color: #000000;
                                             }
                                             .skin-blue .main-header .logo:hover {
                                             background-color: #000000;
                                             }
                                             .skin-blue .main-header .navbar {
                                             background-color: #000000;
                                             }        
                                             .skin-blue .main-sidebar {
                                             background-color: #000000;
                                             }
                                             .skin-blue .main-header .navbar {
                                             background-color: #000000;
                                             }        
                                             /* main sidebar */
                                             .skin-blue .main-sidebar {
                                             background-color: #000000;
                                             }'))),
    sidebarMenu(
      menuItem(text = "Capacity Study",tabName = "study",icon = icon("buromobelexperte")),
      #menuItem(text = "Capacity Forecst",tabName = "forecast",icon = icon("buromobelexperte")),
      menuItem(text = "FTE Forecasting", tabName = "FTEF", icon = icon("chart-line"))
                   )
                   ),
    dashboardBody(
      tags$style(type="text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden;}"
      ),
    tabItems(
      # Page one - capacity study ----
      tabItem(tabName = "study",
              # First Row of the dasboard - current capacity based on 12 months's benchmark
              tags$style(HTML(".small-box.bg-blue{
                              padding: 10px;
                              background-color: #a2dcfc !important; color: #ffffff !important;
                              }")),
              tags$style(HTML(".small-box.bg-aqua{
                              padding: 10px;
                              background-color: #75CEFF !important; color: #ffffff !important;
              }")),
              tags$style(HTML(".small-box.bg-light-blue{
                              padding: 10px;
                              background-color: #3398C6 !important; color: #ffffff !important;
              }")),
              tags$style(HTML(".small-box.bg-yellow{
                              padding: 10px;
                              background-color: #fcaeae !important; color: #ffffff !important;
              }")),
              tags$style(HTML(".small-box.bg-orange{
                              padding: 10px;
                              background-color: #d12a2a !important; color: #ffffff !important;
              }")),
              tags$style(HTML(".small-box.bg-red{
                              padding: 10px;
                              background-color: #b00202 !important; color: #ffffff !important;
              }")),
              tags$style(HTML(".small-box.bg-lime{
                              padding: 10px;
                              background-color: #e1dee3 !important; color: #ffffff !important;
              }")),
              tags$style(HTML(".small-box.bg-navy{
                              padding: 10px;
                              background-color: #BCBABE !important; color: #ffffff !important;
              }")),
              tags$style(HTML(".small-box.bg-black{
                              padding: 10px;
                              background-color: #817f82 !important; color: #ffffff !important;
              }")),
              tags$style(HTML(".small-box.bg-fuchsia{
                              padding: 10px;
                              background-color: #ffffff !important; color: #ffffff !important;
              }")),
              tags$style(HTML(".small-box.bg-purple{
                              padding: 10px;
                              background-color: #11d4c7 !important; color: #ffffff !important;
              }")),
              tags$style(HTML(".small-box.bg-olive{
                              padding: 10px;
                              background-color: #caebcb !important; color: #000000 !important;
              }")),
              tags$style(HTML(".small-box.bg-maroon{
                              padding: 10px;
                              background-color:#793030 !important; color: #ffffff !important;
                              }")),
              tags$style(design_box(background_color = "#ffffff",header_color = "#ededed",type = "info")),
              
              fluidRow(
                boxPlus(
                  title = HTML("<b>Where are we now?</b>"),
                  closable = FALSE,
                  width = 12,
                  status = "primary", 
                  solidHeader = FALSE, 
                  collapsible = TRUE,
                  enable_label = TRUE,
                  label_text = "Expand box to select parameters for analysis",
                  label_status = "primary",
                  HTML('<b>Select Country, IBGO Function, IBGO Team and Analysis Period to begin the analysis</b>'),
                  fluidRow(style = "padding-bottom: 5px;",
                           column(width = 3, selectInput(inputId = "current_cntry", 
                                                         label = HTML("<span style='color: black'>Country</span>"),
                                                         choices = unique(staff_hours_og$Country),
                                                         selected = "SG",multiple = F)),
                           column(width = 3, selectInput(inputId = "current_funcs", 
                                                         label = HTML("<span style='color: black'>IBGO Function</span>"),
                                                         choices = "", selected = "", multiple = F)),
                           column(width = 3, selectInput(inputId = "current_team", 
                                                         label = HTML("<span style='color: black'>IBGO Team</span>"),
                                                         choices = "", selected = "", multiple = F)),
                           column(width = 3, selectInput(inputId = "current_period", 
                                                         label = HTML("<span style='color: black'>Analysis Period</span>"), 
                                                         choices = list("Latest Month" = 1, "Three Months" = 3, "Six Months" = 6,
                                                                        "Twelve Months" =12,"Full 2018" = 2018),
                                                         selected = 1,multiple = F))
                  )
                )),
              fluidRow(
                uiOutput("current_box"),
                uiOutput("current_cap"),
                uiOutput("current_without_ot"),
                uiOutput("current_standard")
              ),
              fluidRow(
                boxPlus(title = HTML('<b><font style = "color:black">Monthly Trend</font></b>'),
                        collapsible = T,
                        collapsed = T,
                        closable = F,
                        solidHeader = T, 
                        width = 12,
                        enable_label = T,
                        label_text = "Expand box to view monthly trend graphs",
                        fluidRow(plotlyOutput(outputId = "plot1",width = "1160px")),
                        fluidRow( 
                          dropdownButton(
                            tags$h3("List of Input"),
                            selectInput(inputId = 'bar', 
                                        label = 'Bar plot', 
                                        choices = c("capacity","working hours","OT",
                                                    "efficiency","utilisation","absentiseem")),
                            selectInput(inputId = 'line', 
                                        label = 'Line plot', 
                                        choices = names(iris), 
                                        selected = names(iris)[[2]]),
                            circle = TRUE, status = "danger", icon = icon("gear"), width = "300px",
                            tooltip = tooltipOptions(title = "Click to see inputs !")),
                          plotlyOutput(outputId = "plot2",width = "1170px"))
                )
              ),
              fluidRow(
                boxPlus(
                  title = HTML("<b>How can we optimise?</b>"),
                  closable = FALSE,
                  width = 12,
                  status = "danger", 
                  solidHeader = FALSE, 
                  collapsible = TRUE,
                  enable_label = TRUE,
                  label_text = "Expand box to select parameters for benchmark analysis",
                  label_status = "danger",
                  HTML('<b>Select Country, IBGO Function, IBGO Team and Benchmark Period to begin the analysis</b>'),
                  fluidRow(style = "padding-bottom: 5px;",
                           column(width = 3, selectInput(inputId = "bm_cntry", 
                                                         label = HTML("<span style='color: black'>Country</span>"),
                                                         choices = unique(staff_hours_og$Country),
                                                         selected = "HK",multiple = F)),
                           column(width = 3, selectInput(inputId = "bm_funcs", 
                                                         label = HTML("<span style='color: black'>IBGO Function</span>"),
                                                         choices = unique(staff_hours_og$OpsFunc[staff_hours_og$Country=="HK"]), selected = "CASH", multiple = F)),
                           column(width = 3, selectInput(inputId = "bm_team", 
                                                         label = HTML("<span style='color: black'>IBGO Team</span>"),
                                                         choices = unique(staff_hours_og$Team[staff_hours_og$Country=="HK" & staff_hours_og$OpsFunc=="CASH"]), selected = "Time_Deposit", multiple = F)),
                           column(width = 3, selectInput(inputId = "bm_period", 
                                                         label = HTML("<span style='color: black'>Benchmark Period</span>"), 
                                                         choices = list("Latest Month" = 1, "Three Months" = 3, "Six Months" = 6, 
                                                                        "Twelve Months" =12,"Full 2018" = 2018),
                                                         selected = 1,multiple = F))
                  )
                )),
              fluidRow(
                uiOutput("bm_box"),
                uiOutput("bm_cap"),
                uiOutput("bm_without_ot"),
                uiOutput("bm_standard")
                
              ),
              # Historical Best data 
              fluidRow(
                uiOutput("his_box"),
                uiOutput("his_cap"),
                uiOutput("his_without_ot"),
                uiOutput("his_standard")
                
              ),
              fluidRow(
                boxPlus(
                  title = HTML("<b>What Should we do next?</b>"),
                  closable = FALSE,
                  width = 12,
                  status = "success", 
                  solidHeader = FALSE, 
                  collapsible = TRUE,
                  enable_label = TRUE,
                  label_text = "Expand box to select parameters for Sensitivity analysis",
                  label_status = "success",
                  HTML('<b>Select either Efficiency, Utilisation or Over time for Sensitivity Analysis</b>'),
                  p(' '),
                  fluidRow(style = "padding-bottom: 5px;",
                           column(width = 3, materialSwitch(inputId = "sa_eff", 
                                                            label = "Efficiency", 
                                                            status = "success",value = F)),
                           column(width = 3, materialSwitch(inputId = "sa_ut", 
                                                            label = "Utilisation", 
                                                            status = "success",value = F)),
                           column(width = 3, materialSwitch(inputId = "sa_ot", 
                                                            label = "Over Time", 
                                                            status = "success",value = F))
                  ),
                  fluidRow(
                    column(width = 3,
                           knobInput(
                             inputId = "sim_eff",label = "Efficiency:",value = 80,min = 0,step = 0.1,height = "90%",
                             max= 100, displayPrevious = TRUE, lineCap = "round",fgColor = "#428BCA",
                             inputColor = "#428BCA",width = "100%",cursor = T
                           ),hr(),
                           numericInput(inputId = "sim_eff_num", label="Indicate Efficiency", value = "",min = 0, max = 100,
                                        width = NULL)),
                    column(width = 3,
                           knobInput(
                             inputId = "sim_uti",label = "Utilisation:",value = 90,min = 0,step = 0.1,height= "90%",
                             max = 100,displayPrevious = TRUE,lineCap = "round",fgColor = "#428BCA",
                             inputColor = "#428BCA",width = "100%"
                           ),hr(),
                           numericInput(inputId = "sim_ut_num", label="Indicate Utilisation",value = "", min = 0, max = 100,
                                        width = NULL)
                    ),
                    column(width = 3,
                           knobInput(
                             inputId = "sim_ot", label = "Average Workout Hrs with OT:",value = 9.5, step = 0.1,height = "90%",
                             min = 0,max = 14,displayPrevious = TRUE, lineCap = "round",fgColor = "#428BCA",
                             inputColor = "#428BCA",width = "100%"
                           ),hr(),
                           numericInput(inputId = "sim_ot_num", label="Indicate OT",value = "", min = 0, max = 100, width = NULL)
                    ),
                    column(width = 3,
                           uiOutput(outputId = "sim_capacity")
                    )
                    
                  )
                  
                )
              )
            ),
      # Page three - Capacity Forecasting ----
      tabItem(tabName = "FTEF",
          headerPanel("Capacity Forecasting"),
          fluidRow(
            boxPlus(
              title = "Forecasting Selection", 
              status = "primary",width = 12,
              collapsible = T,
              collapsed = F,
              closable = F,
              enable_label = T,
              label_text = "Expand box to select forecasting inputs",
              label_status = "primary",
              column(width = 2, selectInput(inputId="forecast_cntry",
                                            label = "IBGO Country", 
                                            choices = unique(staff_hours_og$Country),
                                            selected = "SG",multiple = F)),
              column(width = 2, selectInput(inputId="forecast_func", 
                                            label = "IBGO Function", 
                                            choices = c("All",unique(staff_hours_og$OpsFunc[staff_hours_og$Country=="SG"])), 
                                            selected = "TRADE_MT", multiple = FALSE)),
              column(width = 2,selectInput(inputId="forecast_team", 
                                           label = "IBGO Team", 
                                           choices = c("All",unique(staff_hours_og$Team[staff_hours_og$Country =="SG" & staff_hours_og$OpsFunc == "TRADE_MT" ])), selected = "All", multiple = FALSE)),
              column(width = 3, selectInput(inputId = "horizon", label = "Prediction Horizon",
                                             choices = c("3 Months" = "3",
                                                         "6 Months" = "6",
                                                         "1 year" = "12",
                                                         "2 year" = "24"),
                                             selected = "24")),
              column(width = 3,selectInput(inputId = "parameter",
                                           label = "Parameters",
                                           choices = c("Capacity Parameters"=1, 
                                                       "Benchmark Parameters"=2,  
                                                       "Historical Best Parameters"=3),
                                           selected = 1))
              )),
   
            fluidRow(
            tabBox(title = "Capacity Forecasting Plots", width = 12, height = "600px",
                   tabPanel(title = "Monthly Forecast",
                            fluidRow(
                            column(HTML("<div style = 'margin-left:20px;'><h1> Monthly Workout Hours Forecast </h1></div>"),width = 6),
                            column(
                              tags$div(
                                style = "margin-top:20px;",
                              dropdownButton(
                                tags$div(
                                  style = "color: black !important;",
                                  h3("Download Dataset"),
                                  selectInput("dataset","Choose a dataset:", 
                                              choices = c("Forecasted Workout Hours" = 1,"Forecasted FTE" = 2)), 
                                  downloadButton("downloadData","Download")
                                  ),
                                circle = TRUE, 
                                status = "danger",
                                icon = icon("gear"),
                                size = "sm",
                                tooltip = tooltipOptions(title = "Click to download!")
                                )),width = 6)
                              ),
                            plotOutput(outputId = "monthly_forecast",height = "470px")
                            ),
                   tabPanel(title = "Model Evaluation",
                            fluidRow(
                              column(HTML("<div style = 'margin-left:20px;'><h1>Forecasting Model Evaluation</h1></div>"),width = 6),
                              column(
                                tags$div(
                                  style = "margin-top:20px;",
                                  column(width = 4,
                                      selectInput(inputId = "start_month", 
                                                  label = "Model Evaluation Start Month", 
                                                  choices = format(seq(as.Date("2019-01-01"),by = "-1 month",length.out = 25),"%Y-%m"), 
                                                  selected = NULL, multiple = FALSE,
                                                  selectize = FALSE, width = NULL, size = NULL)
                                      ),
                                  column(width = 4,
                                      selectInput(inputId = "eval_horizon", 
                                                  label = "Model Evaluation Training Duration", 
                                                  choices = c("6 Months" = 6, "12 Months" = 12, "24 Months" = 24), 
                                                  selected = 24, multiple = FALSE,
                                                  selectize = FALSE, width = NULL, size = NULL)
                                  ),
                                  column(width = 4,
                                         selectInput(inputId = "pred_horizon", 
                                                     label = "Model Evaluation Prediction Horizon", 
                                                     choices = c("3 Months" = 3, "6 Months" = 6, "12 Months" = 12, "24 Months" = 24), 
                                                     selected = 12, multiple = FALSE,
                                                     selectize = FALSE, width = NULL, size = NULL)
                                  )
                                ),width = 6)
                              ),
                              plotOutput("evaluation",height = "440px")
                            )
                         )
                      ),
          fluidRow(
            uiOutput("para1"),
            uiOutput("para2"),
            uiOutput("para3"),
            uiOutput("actual_fte")
          ),
          fluidRow(
                box(title = HTML("<b>Model One -  Statistical Forecasting Model</b>"),width = 3,
                HTML('<font style>The best forecasting method was selected after fitting and testing a range of time series models:</font>'),br(),
                HTML('<font style> > ARIMA</font>'),br(),
                HTML('<font style> > Expotential Smoothing</font>'),br(),
                HTML('<font style> > Time Series Linear Model</font>'),br(),
                HTML('<font style>Model was chosen based on Mean Squared Error</font>'),br()
              ),
            uiOutput(outputId = "forecast_fte1"),
            uiOutput(outputId = "forecast_fte2"),
            uiOutput(outputId = "forecast_fte3")
          ),
          fluidRow(
            uiOutput(outputId = "model3"),
            uiOutput(outputId = "month_fte1"),
            uiOutput(outputId = "month_fte2"),
            uiOutput(outputId = "month_fte3")
          ),
          fluidRow(
            box(title = HTML("<b>Model Three -  Manager's Expectation</b>"),width = 3,
                HTML("<font style>Managers to indicate the growth rate they expect as of the latest month (Linear increase)</font>"),br(),
                sliderInput(inputId ="fte2", label = h5("Percentage Change"), min = -50, max = 50, value = 1,step = 1)
            ),
            uiOutput(outputId = "manager_fte1"),
            uiOutput(outputId = "manager_fte2"),
            uiOutput(outputId = "manager_fte3")
        )
      )
    )
  )
)

    
  
