server <- function(input,output,session){
  # Page one - Capacity Study ----
  # First row of p1 ----
  
   cc <- reactive({
     input$current_cntry
   })
  
   cf_df <- reactive({
     
     mm <- staff_hours_og$OpsFunc[staff_hours_og$Country == cc()]
     
     return(unique(mm))
   })
   
   observe({
     
     select <- c("All",cf_df())
     
     updateSelectInput(session = session, 
                       inputId = "current_funcs", 
                       label = HTML("<span style='color: black'>IBGO Function</span>"),
                       choices = select, 
                       selected = select[1])
     
   })
   
   currentfunction <- reactive({
     input$current_funcs
   })
   
   current_teams_df <- reactive({
     
     all_functions <- cf_df()
     
     selected_functions <- currentfunction()
     
     w <- ifelse(selected_functions =="All",all_functions,selected_functions)
     
     mm <- staff_hours_og %>% filter(Country %in% cc(), OpsFunc %in% w) %>% select(Team) %>% distinct()
     
     mm <- mm$Team
    
     mm
     
   })
   
   observe({
     
     teams <- c("All", current_teams_df())
     
     updateSelectInput(session = session, 
                       inputId = "current_team", 
                       label = HTML("<span style='color: black'>IBGO Function</span>"),
                       choices = teams, 
                       selected = teams[1])
     
   })
   
   currentteam <- reactive({
     
     input$current_team
     
   })

   cap_12 <- reactive({
     
    # AIM: Calculate the capacity of recent 12 months
    # User selection for country - current_cntry 
    req(input$current_cntry)
    # User selection for Function - current_funcs
    req(input$current_funcs)
    # User selection for Team - current_team 
    req(input$current_team)
    
    # If User select "All" for functions - get the list of functions for country
    funcs_filter <- switch(input$current_funcs, 
                          All ={
                            cf_df()
                          },{
                            input$current_funcs
                            }
    )
    
    # If User select "All" for teams - get the list of teams for functions and country
    
    teams_filter <- switch(input$current_team,
                           All = {
                               current_teams_df()
                             },{
                               input$current_team
                             }
                           )
    
    # Filter the daily allocation data with country, functions and teams 
    daily_df <- daily_allocation_og %>% filter(Country %in% input$current_cntry,
                                               OpsFunc %in%  funcs_filter,
                                               Team %in% teams_filter)
    
    date_filter = format(seq(as.Date("2018-01-01"),by="1 month",length.out = 12),"%Y-%m")
    
    daily_df_filtered = daily_df %>% 
                        filter(YYMM %in% date_filter) %>% 
                        select(Date,Day,WeekSun_Date,YYMM,Year,OpsFunc,
                               Team,Staff_Name,Country,Workout_Hrs,Activity_Name)
    daily_df_filtered$Staff_Name <- toupper(daily_df_filtered$Staff_Name)
    
    staff_df_filtered  <- staff_hours_og %>% filter(Country %in% input$current_cntry,
                                                    OpsFunc %in%  funcs_filter,
                                                    Team %in% teams_filter) %>% 
                                                    filter(YYMM %in% date_filter) %>% 
                                                    select(Date,Day,WeekSun_Date,YYMM,Year,OpsFunc,
                                                    Team,Staff_Name,Country,Total_Work_Hrs,
                                                    Total_CoreWork_Hrs,Total_Leave_Hrs,Paid_OT_Hrs)
      
    staff_df_filtered$Staff_Name <- toupper(staff_df_filtered$Staff_Name)

    # calculate_key - available in capacity_funs.R
    
    key <- calculate_key(daily = daily_df_filtered, staff = staff_df_filtered)
    
    # Calculate Efficiency and utilisation
    
    print("---- Calculate Current E,U,O ----")
    
    daily_total_workout <- sum(daily_df_filtered$Workout_Hrs)
    
    staff_total_workout <- sum(staff_df_filtered$Total_Work_Hrs)
    
    staff_total_corehrs <- sum(staff_df_filtered$Total_CoreWork_Hrs)
    
    staff_paid_ot <- sum(staff_df_filtered$Paid_OT_Hrs)
    
    staff_leave_hrs <- sum(staff_df_filtered$Total_Leave_Hrs)
    
    avg_workout_hrs <- (staff_total_workout+staff_leave_hrs)/key
    
    benchmark_table <- data.frame(Efficiency = round(daily_total_workout/staff_total_corehrs,3), 
                                 Utilisation = round(staff_total_corehrs/staff_total_workout,3),
                                 OT_imp = round((((staff_total_workout+staff_leave_hrs)/key)-8.5),3),
                                 OT_paid= round(staff_paid_ot/key,3),
                                 actual_workout_hrs = round(avg_workout_hrs,3),
                                 abs = staff_leave_hrs/(staff_total_workout+staff_leave_hrs), 
                                 stringsAsFactors = F) %>%
                                 select(Efficiency,Utilisation,OT_imp,OT_paid,abs,actual_workout_hrs)
    
    return(benchmark_table)
    
  }) 
  
   current_cap <- reactive({
    
    req(input$current_period)
    
    req(input$current_cntry)
    
    req(input$current_funcs)
    
    req(input$current_team)
    
    # input = data.frame(current_period=1, current_cntry = "SG", current_funcs = "All", current_team = "All",stringsAsFactors = F) 
    
    funcs_filter <- switch(input$current_funcs, 
                          All ={
                            cf_df()
                          },{
                            input$current_funcs
                          }
    )
    
    daily_df  <- switch(input$current_team,
                       All = {
                         daily_allocation_og %>% filter(Country %in% input$current_cntry, OpsFunc %in% funcs_filter)
                       },
                       {
                         daily_allocation_og %>% filter(Country %in% input$current_cntry, OpsFunc %in% funcs_filter, Team %in% input$current_team)
                       }
    )
    
    # Analysis from the most recent month
    current_month <- max(daily_df$YYMM)
    
    # Create the date filter 
    current_date <- as.Date(paste0(current_month,"-01"),"%Y-%m-%d")

    # Get the past months data based on user selection
    if(input$current_period == 2018){
      date_filter = format(seq(as.Date("2018-01-01"),by="1 month",length.out = 12),"%Y-%m")
    }else{
      date_filter = format(seq(current_date,by = "-1 month", length.out = as.numeric(input$current_period)),"%Y-%m")
    }
    
    # filter period
    daily_df_current <- daily_df %>% 
                        filter(YYMM %in% date_filter) %>% 
                        select(Date,Day,WeekSun_Date,YYMM,
                               Year,OpsFunc,Team,Staff_Name,
                               Country,Workout_Hrs,Activity_Name)
    
    daily_df_current$Staff_Name <- toupper(daily_df_current$Staff_Name)
    

    staff_df <- switch(input$current_team,
                       All = {
                         staff_hours_og %>% filter(Country %in% input$current_cntry, 
                                                   OpsFunc %in% funcs_filter)
                       },
                       {
                         staff_hours_og %>% filter(Country %in% input$current_cntry, 
                                                   OpsFunc %in% funcs_filter, 
                                                   Team %in% input$current_team)
                       }
                      )
  
    staff_df$Staff_Name <- toupper(staff_df$Staff_Name)
    
    staff_df_current <- staff_df %>% 
                        filter(YYMM %in% date_filter) %>% 
                        select(Date,Day,WeekSun_Date,YYMM,
                               Year,OpsFunc,Team,Staff_Name,
                               Country,Total_Work_Hrs,Total_CoreWork_Hrs,
                               Total_Leave_Hrs,Paid_OT_Hrs)
    
    common_dates <- intersect(daily_df_current$Date,staff_df_current$Date)
    
    current_data <- full_join(daily_df_current,staff_df_current,
                              by=c("Staff_Name","Date","OpsFunc",
                                   "Team","WeekSun_Date","Country",
                                   "Day","YYMM","Year"))
    
    current_data <- distinct(current_data)
    
    current_data[is.na(current_data)] <- 0
    
    # condition for the filtering 
    
    current_data <- current_data[current_data$Workout_Hrs > 0 ,]
    
    bm_combined_table <- cap_12()
    
    # calculate the capacity 
    print("---- Calculate capacity ----")
    
    bm_capacity_with_ot <- capacity_new(x = current_data, bm = bm_combined_table, o = bm_combined_table$actual_workout_hrs)
    
    bm_capacity_without_ot <- capacity_new(x = current_data, bm = bm_combined_table, o = bm_combined_table$actual_workout_hrs - bm_combined_table$OT_paid)
    
    bm_capacity_standard <- capacity_new(x = current_data, bm = bm_combined_table, o = 8.5)
    
    out <- list(cap_with_ot = bm_capacity_with_ot, 
               cap_without_ot = bm_capacity_without_ot, 
               cap_standard = bm_capacity_standard)
    
    return(out)
  }) 
  
   output$current_cap <- renderUI({
    # Display the current capacity - working hours with OT
    df <- current_cap()[["cap_with_ot"]]
    
    bm <- cap_12()
    
    cap <- df$capacity
    
    avg <- bm$actual_workout_hrs
    
    valueBox(value =HTML(paste0('<font style="font-size:55px">',round(cap,1),'</font>')),subtitle = HTML(paste0('<font style="font-size:20px">Capacity with OT </font>',br(),"Average working Hours: ",round(avg,1),br(),"Actual FTE: ",round(df$actual_FTE,1),br(),"Implied FTE: ",round(df$implied_FTE,1))),icon = icon("user-clock"),color = "blue",width = 3)
    
  })
  
   output$current_without_ot <- renderUI({
    
    # Display the current capacity - working hours without OT
    
    df <- current_cap()[["cap_without_ot"]]
    
    bm <- cap_12()
    
    cap <- df$capacity
    
    avg <- bm$actual_workout_hrs - bm$OT_paid
    
    valueBox(value =HTML(paste0('<font style="font-size:55px">',round(cap,1),'</font>')),subtitle = HTML(paste0('<font style="font-size:20px">Capacity without OT</font>',br(),"Average working Hours: ",round(avg,1),br(),"Actual FTE: ",round(df$actual_FTE,1),br(),"Implied FTE: ",round(df$implied_FTE,1))),icon = icon("user"),color = "aqua",width = 3)
  })
  
   output$current_standard <- renderUI({
    
    # Display the current capacity - standard working hours
    
    df <- current_cap()[["cap_standard"]]
    
    bm <- cap_12()
    
    cap <- df$capacity
    
    avg <- 8.5
    
    valueBox(value =HTML(paste0('<font style="font-size:55px">',round(cap,1),'</font>')),subtitle = HTML(paste0('<font style="font-size:20px">Capacity</font>',br(),"Average working Hours: ",round(avg,1),br(),"Actual FTE: ",round(df$actual_FTE,1),br(),"Implied FTE: ",round(df$implied_FTE,1))),icon = icon("user-circle"),color = "light-blue",width = 3)
    
  })
  
   output$current_box <- renderUI({
    
    bm <- cap_12()
    
    e <- bm$Efficiency *100
    
    u <- bm$Utilisation *100
    
    a <- round(bm$abs*100,1)
    
    o <- round(bm$OT_paid,1)
    
    w <- paste0(" Country- ",input$current_cntry,", ", "Function- ", input$current_funcs,", ", "Team- ",input$current_team, ", ", "Analysis Period- ",input$current_period)
    
    box(width = 3,solidHeader = T,
        HTML(paste0('<i class="glyphicon glyphicon-cog" style="font-size: 1.5em;"></i><font style="font-size:20px;font-family:"Times New Roman" ;font-weight: 400>'," Capacity Parameters",'</font>')), br(),
        HTML('<font style="font-size:12px"> Calculated based on 12 Ms data (2018) <font>'),br(),br(),
        HTML(paste0('<font style="font-size:15px">'," Efficiency: ",e,"%",'</font>')),br(),
        HTML(paste0('<font style="font-size:15px">'," Utilisation: ",u,"%",'</font>')),br(),
        HTML(paste0('<font style="font-size:15px">'," Absetiseem: ",a,"%",'</font>')),br(),
        HTML(paste0('<font style="font-size:15px">'," Over Time(OT): ",o," hrs",'</font>')),br(),
        HTML(paste0('<i class="fas fa-info-circle" style="font-size: 1em;color:red"></i><font style="font-size:10px">',w,'</font>'))
    )
    
  })
  
  # Second Row of p1 ----
  
  monthly_capacity <- reactive({
    
    req(input$current_period)
    
    req(input$current_cntry)
    
    req(input$current_funcs)
    
    req(input$current_team)
    
    print(" ---- Monthly Capacity ---- ")
    
    #### Debugging ####
    
    # input <- data.frame(current_period = 3, current_cntry="SG", current_funcs="CASH", current_team="EWSS",stringsAsFactors = F)
    
    # Benchmark comes from benchmark selected by user
    bm <- cap_12()
    
    bm_list <- replicate(bm,n = 12,simplify = F)
  
    # input = data.frame(date_month = "Mar-2019",current_cntry = "CN", current_funcs = "All", current_team = "All", current_period = 1,stringsAsFactors = F)
    
    # Create filter for fucntions and teams 
    funcs_filter <- switch(input$current_funcs, 
                          All ={
                            s = daily_allocation_og %>% filter(Country %in% input$current_cntry) %>% 
                                                        select(OpsFunc) %>% distinct()
                            s[["OpsFunc"]]
                            },{
                            input$current_funcs
                            }
    )
    daily_df  <- switch(input$current_team,
                       All = {
                         daily_allocation_og %>% filter(Country %in% input$current_cntry, 
                                                        OpsFunc %in% funcs_filter)
                       },
                       {
                         daily_allocation_og %>% filter(Country %in% input$current_cntry, 
                                                        OpsFunc %in% funcs_filter, 
                                                        Team %in% input$current_team)
                       }
    )
    daily_df$Staff_Name <- toupper(daily_df$Staff_Name)
    
    # Analysis from the most recent month
    current_month <- max(daily_df$YYMM)
    
    # Create the date filter 
    current_date <- as.Date(paste0(current_month,"-01"),"%Y-%m-%d")
    
    # Get the past months data
    if(input$current_period == 2018){
      date_filter <- format(seq(as.Date("2018-01-01"),by="1 month",
                                length.out = 12),"%Y-%m")
    }else{
      date_filter <- format(seq(current_date,by = "-1 month", 
                                length.out =as.numeric(12)),"%Y-%m")
    }
    
    daily_df <- daily_df %>% filter(YYMM %in% date_filter)
    
    
    staff_df  <- switch(input$current_team,
                       All = {
                         staff_hours_og %>% filter(Country %in% input$current_cntry, 
                                                   OpsFunc %in% funcs_filter)
                       },
                       {
                         staff_hours_og %>% filter(Country %in% input$current_cntry, 
                                                   OpsFunc %in% funcs_filter, 
                                                   Team %in% input$current_team)
                       }
    )
    
    staff_df$Staff_Name <- toupper(staff_df$Staff_Name)
    
    staff_df <- staff_df %>% filter(YYMM %in% date_filter)
    
    # Calculate Efficiency and utilisation
    print("Calculate Key")
    
    assign("daily_df",daily_df,envir = .GlobalEnv)
    
    assign("staff_df", staff_df, envir = .GlobalEnv)
    
    key_df <- full_join(daily_df,staff_df,by=c("Staff_Name","Date","OpsFunc",
                                               "Team","WeekSun_Date","Country",
                                               "Day","YYMM","Year"))
    
    key_df <- distinct(key_df)
    
    key_df[is.na(key_df)] <- 0
    
    key_og <- key_df
    
    key_df <- key_df[key_df$Workout_Hrs>0,]
    
    key_list <- split(key_df,key_df$YYMM)

    key_list_df <- lapply(key_list,key_count)
    
    key <- do.call("rbind",key_list_df)
    
    key <- key %>% select(YYMM,key) %>% distinct()
    
    key <- split(key,key$YYMM)
    
    key <- lapply(X = key, function(x) x[['key']])
    
    # monthly combined data
    current_data <- full_join(daily_df,staff_df,by=c("Staff_Name","Date","OpsFunc",
                                                     "Team","WeekSun_Date","Country",
                                                     "Day","YYMM","Year"))
    
    current_data <- distinct(current_data)
    
    current_data[is.na(current_data)] <- 0
    
    current_data <- current_data[current_data$Workout_Hrs>0,]
    
    monthly_current_data <- split(current_data,current_data$YYMM)
    
    # Monthly data 
    
    monthly_daily_df <- split(daily_df,daily_df$YYMM)
    
    monthly_staff_df <- split(staff_df,staff_df$YYMM)
    
    # prepare monthly data 

    w <- mapply(eup_cal,monthly_daily_df,monthly_staff_df,key,SIMPLIFY = F)
    
    monthly_bm <- do.call("rbind", w)

    # Use the E,U,OT of each month to calculate capacity
    cap_with_ot <- mapply(capacity_monthly,monthly_current_data,w,1,SIMPLIFY = F)
    
    cap_with_ot <- do.call("rbind",cap_with_ot)
    
    cap_without_ot <- mapply(capacity_monthly,monthly_current_data,w,2,SIMPLIFY = F)
    
    cap_without_ot <- do.call("rbind",cap_without_ot)
    
    cap_bm_with_ot <- mapply(capacity_monthly,monthly_current_data,bm_list,1,SIMPLIFY = F)
    
    cap_bm_with_ot <- do.call("rbind",cap_bm_with_ot)
    
    cap_bm_without_ot <- mapply(capacity_monthly,monthly_current_data,bm_list,2,SIMPLIFY = F)
    
    cap_bm_without_ot <- do.call("rbind",cap_bm_without_ot)
    
    out <- list(c_with_ot = cap_with_ot, 
               c_without_ot = cap_without_ot,
               c_bm_with_ot = cap_bm_with_ot, 
               c_bm_without_ot = cap_bm_without_ot, 
               monthly_benchmark = monthly_bm)
    
    return(out)
    
  }) 
  
  output$plot1 <- renderPlotly({
    library(plotly)
    
    print("Plot 1")
    
    d <- monthly_capacity()[["c_bm_with_ot"]]
  
    d <- d[order(d$YYMM),]
    
    m <- list(l = 50,r=50,b=50,t=50,pad = 4)
    
    plot_ly(d,height = 370) %>% 
      add_trace(x = ~YYMM, y = ~capacity, type = "bar", name = "Capacity",marker = list(color = "#bfe3ff")) %>%
      add_trace(x = ~YYMM, y = ~actual_FTE, type = "scatter",mode = "lines+markers",name="Actual FTE",yaxis = 'y2',marker = list(color = "#133b99"), line = list(color = "#133b99")) %>%
      add_trace(x = ~YYMM, y = ~implied_FTE, type = "scatter", mode = "lines+markers",name="Implied FTE",yaxis = 'y2',marker = list(color = "#d60007"), line = list(color = "#d60007")) %>%
      layout(title = "Monthly Capacity (One Year)", 
             xaxis = list(title = "Month"), 
             yaxis = list(side='left',title = "Capacity",showgrid = FALSE),
             yaxis2 = list(side='right',title = "FTE",overlaying = "y",showgrid = FALSE),
             autosize = T,
             legend = list(orientation = 'h', x = 0.02, y = 1.1),
             margin = m
      )
  })
  
  output$plot2 <- renderPlotly({
    
    xx <- monthly_capacity()[["monthly_benchmark"]]
  
    avg_workin <- mean(as.numeric(xx$actual_workout_hrs))
    
    xx$actual_workin_hours  <- xx$actual_workout_hrs - avg_workin
    
    xx$YYMM <- as.character(xx$YYMM)
    
    monthly_cap <- monthly_capacity()[["c_bm_with_ot"]]
    
    monthly_cap <- monthly_cap[order(monthly_cap$YYMM),]
    
    monthly_cap$YYMM <- as.character(monthly_cap$YYMM)
    
    df <- inner_join(xx,monthly_cap,by="YYMM")
    
    df <- df[order(df$YYMM),]
  
    m <- list(l = 50,r=50,b=50,t=50,pad = 4)
    
    plot_ly(df,height = 370) %>% 
      add_trace(x = ~YYMM, y = ~capacity, type = "bar",name = 'Capacity(FTE)',marker = list(color = "#bfe3ff")) %>%
      add_trace(x = ~YYMM, y = ~OT_paid, type = "scatter",mode = "lines+markers",name = 'OT', yaxis = 'y2',marker = list(color = "#d60007"), line = list(color = "#d60007")) %>%
      layout(title = "Monthly Average working Hours (One Year)", 
             xaxis = list(title = "Month"), 
             yaxis = list(side='left',title = "Capacity",showgrid = FALSE),
             yaxis2 = list(side='right',title = "Hours",overlaying = "y",showgrid = FALSE),
             autosize = T,
             legend = list(orientation = 'h',x = 0.02, y = 1.1),
             margin = m
      )
    
  })
  
  # Third Row of p1    ----
  
  bc <- reactive({
    
    input$bm_cntry
    
  })
  
  bf_df <- reactive({
    
    mm <- daily_allocation_og$OpsFunc[daily_allocation_og$Country==bc()]
    
    unique(mm)
    
  })
  
  observe({
    
    # Update benchmark capacity function selection
    
    updateSelectInput(session, inputId = "bm_funcs",choices = c("All",bf_df()), selected = "All")
  })
  
  bf <- reactive({
    
    input$bm_funcs
  })
  
  bt_df <- reactive({
    
    w <- ifelse(bf()=="All",bf_df(),bf())
    
    mm <- daily_allocation_og$Team[daily_allocation_og$Country == bc() & daily_allocation_og$OpsFunc == w]
    
    unique(mm)
  })
  
  observe({
    
    print("Update benchmark capacity team selection ----")
    
    updateSelectInput(session,inputId = "bm_team",choices = c("All",bt_df()),selected = "All")
    
  })
  
  bm_capacity <- reactive({
    
    print("----Benchmark capacity----")
    
    req(input$bm_period)
    
    req(input$bm_cntry)
    
    req(input$bm_funcs)
    
    req(input$bm_team)
    
    req(input$current_period)
    
    req(input$current_cntry)
    
    req(input$current_funcs)
    
    req(input$current_team)
    
    # input = data.frame(date_month = "Mar-2019",current_period=1,current_cntry="SG",current_funcs="CASH",current_team="All",bm_cntry = "SG", bm_funcs = "CASH", bm_team = "All", bm_period = 1,stringsAsFactors = F)
    
    print(input$current_funcs)
    
    print(cf_df())
    
    # Create filter for fucntions and teams 
    funcs_filter <- switch(input$current_funcs, 
                          All = {
                            cf_df()
                          },{
                            input$current_funcs
                          }
    )
    
    daily_df  <- switch(input$current_team,
                       All = {
                         daily_allocation_og %>% filter(Country %in% input$current_cntry, 
                                                        OpsFunc %in% funcs_filter)
                       },
                       {
                         daily_allocation_og %>% filter(Country %in% input$current_cntry, 
                                                        OpsFunc %in% funcs_filter, 
                                                        Team %in% input$current_team)
                       }
    )
    # Analysis from the most recent month
    current_month <- max(daily_df$YYMM)
    
    # Create the date filter 
    current_date <- as.Date(paste0(current_month,"-01"),"%Y-%m-%d")
    
    # Get the past months data
    if(input$current_period == 2018){
      date_filter <- format(seq(as.Date("2018-01-01"),by="1 month",length.out = 12),"%Y-%m")
    }else{
      date_filter <- format(seq(current_date,by = "-1 month", length.out = as.numeric(input$current_period)),"%Y-%m")
    }
    
    # filter period
    daily_df_current <- daily_df %>% filter(YYMM %in% date_filter) %>% 
                                    select(Date,Day,WeekSun_Date,YYMM,Year,
                                           OpsFunc,Team,Staff_Name,Country,
                                           Workout_Hrs,Activity_Name)
    
    staff_df  <- switch(input$current_team,
                       All = {
                         staff_hours_og %>% filter(Country %in% input$current_cntry,
                                                   OpsFunc %in% funcs_filter)
                       },
                       {
                         staff_hours_og %>% filter(Country %in% input$current_cntry,
                                                   OpsFunc %in% funcs_filter, 
                                                   Team %in% input$current_team)
                       }
    )
    
    staff_df$Staff_Name <- toupper(staff_df$Staff_Name)
    
    staff_df_current <- staff_df %>% filter(YYMM %in% date_filter) %>%
                                     select(Date,Day,WeekSun_Date,YYMM,Year,
                                            OpsFunc,Team,Staff_Name,Country,
                                            Total_Work_Hrs,Total_CoreWork_Hrs,
                                            Total_Leave_Hrs,Paid_OT_Hrs)
    
    current_data <- full_join(daily_df_current,staff_df_current,
                              by=c("Staff_Name","Date","OpsFunc",
                                   "Team","WeekSun_Date","Country",
                                   "Day","YYMM","Year"))
    
    current_data <- distinct(current_data)
    
    current_data[is.na(current_data)] <- 0
    
    current_data <- current_data[current_data$Workout_Hrs>0,]
    
    #########################################################################################################################################################################
    
    print("----Filter daily for Benchmark----")
    funcs_filter2 <- switch(input$bm_funcs, 
                           All ={
                             
                             bf_df()
                             
                           },{input$bm_funcs}
    )
    daily_df2 <- switch(input$bm_team,
                        All = {
                          daily_allocation_og %>% filter(Country %in% input$bm_cntry, 
                                                         OpsFunc %in% funcs_filter2)
                        },
                        {
                          daily_allocation_og %>% filter(Country %in% input$bm_cntry, 
                                                         OpsFunc %in% funcs_filter2,
                                                         Team %in% input$bm_team)
                        }
    )
    
    current_month <- max(daily_df2$YYMM)
    
    # Create the date filter 
    current_date2 <- as.Date(paste0(current_month,"-01"),"%Y-%m-%d")
    
    # Get the past months data
    if(input$bm_period == 2018){
      date_filter2 <- format(seq(as.Date("2018-01-01"),by="1 month",length.out = 12),"%Y-%m")
    }else{
      date_filter2 <- format(seq(current_date2,by = "-1 month", length.out = as.numeric(input$bm_period)),"%Y-%m")
    }
    
    daily_df2$Staff_Name <- toupper(daily_df2$Staff_Name)
    
    daily_df_filtered <- daily_df2 %>% filter(YYMM %in% date_filter2) %>% 
                                       select(Date,Day,WeekSun_Date,YYMM,Year,
                                              OpsFunc,Team,Staff_Name,Country,
                                              Workout_Hrs,Activity_Name)
    
    print("Filter staff for Benchmark----")
    
    staff_df2 <- switch(input$bm_team,
                       All = {
                         staff_hours_og %>% filter(Country %in% input$bm_cntry,
                                                   OpsFunc %in% funcs_filter2)
                       },
                       {
                         staff_hours_og %>% filter(Country %in% input$bm_cntry,
                                                   OpsFunc %in% funcs_filter2,
                                                   Team %in% input$bm_team)
                       }
    )
    staff_df2$Staff_Name <- toupper(staff_df2$Staff_Name)
    
    staff_df_filtered <- staff_df2 %>% filter(YYMM %in% date_filter2) %>% 
                                       select(Date,Day,WeekSun_Date,YYMM,Year,
                                              OpsFunc,Team,Staff_Name,Country,
                                              Total_Work_Hrs,Total_CoreWork_Hrs,
                                              Total_Leave_Hrs,Paid_OT_Hrs)
    
    # Calculate Efficiency and utilisation
    key = calculate_key(daily = daily_df_filtered, staff = staff_df_filtered)
    
    # daily allocation 6 months summary
    daily_total_workout <- sum(daily_df_filtered$Workout_Hrs)
    
    # Staff hours 6 months summary 
    staff_total_workout <- sum(staff_df_filtered$Total_Work_Hrs)
    
    staff_total_corehrs <- sum(staff_df_filtered$Total_CoreWork_Hrs)
    
    staff_paid_ot <- sum(staff_df_filtered$Paid_OT_Hrs)
    
    staff_leave_hrs <- sum(staff_df_filtered$Total_Leave_Hrs)

    avg_workout_hrs <- round((staff_total_workout+staff_leave_hrs)/key,3)
    
    combined_table <- data.frame(Efficiency = round(daily_total_workout/staff_total_corehrs,3),
                                 Utilisation = round(staff_total_corehrs/staff_total_workout,3),
                                 OT_imp = round((((staff_total_workout+staff_leave_hrs)/key)-8.5),3),
                                 OT_paid= round(staff_paid_ot/key,3),
                                 actual_workout_hrs = avg_workout_hrs,
                                 abs = staff_leave_hrs/(staff_total_workout+staff_leave_hrs),
                                 stringsAsFactors = F)
    
    bm_combined_table <- combined_table %>% select(Efficiency,Utilisation,OT_imp,OT_paid,abs,actual_workout_hrs)
    
    bm_capacity_with_ot <- capacity_new(x = current_data,bm = bm_combined_table, 
                                        o = bm_combined_table$actual_workout_hrs)
    
    bm_capacity_without_ot <- capacity_new(x = current_data,bm = bm_combined_table, 
                                           o = bm_combined_table$actual_workout_hrs - bm_combined_table$OT_paid)
    
    bm_capacity_standard <- capacity_new(x = current_data,bm = bm_combined_table, o = 8.5)
    
    out = list(bm_c1 = bm_capacity_with_ot,
               bm_c2 = bm_capacity_without_ot,
               bm =  bm_combined_table, 
               avg_wrk_hrs = avg_workout_hrs, 
               bm_standard = bm_capacity_standard)
    
    return(out)
    
  })  
  
  output$bm_cap <- renderUI({
    
    df <- bm_capacity()[["bm_c1"]]
    
    bm <- bm_capacity()[["bm"]]
    
    cap <- df$capacity
    
    avg <- bm$actual_workout_hrs
    
    valueBox(value =HTML(paste0('<font style="font-size:55px">',round(cap,1),'</font>')),subtitle = HTML(paste0('<font style="font-size:20px">Capacity with OT </font>',br(),"Average working Hours: ",round(avg,1),br(),"Actual FTE: ",round(df$actual_FTE,1),br(),"Implied FTE: ",round(df$implied_FTE,1))),icon = icon("user-clock"),color = "yellow",width = 3)
    
  })
  
  output$bm_without_ot <- renderUI({
    
    df <- bm_capacity()[["bm_c2"]]
    
    bm <- bm_capacity()[["bm"]]
    
    cap <- df$capacity
    
    avg <- bm$actual_workout_hrs - bm$OT_paid
    
    valueBox(value =HTML(paste0('<font style="font-size:55px">',round(cap,1),'</font>')),subtitle = HTML(paste0('<font style="font-size:20px">Capacity without OT</font>',br(),"Average working Hours: ",round(avg,1),br(),"Actual FTE: ",round(df$actual_FTE,1),br(),"Implied FTE: ",round(df$implied_FTE,1))),icon = icon("user"),color = "orange",width = 3)
  })
  
  output$bm_standard <- renderUI({
    
    df <- bm_capacity()[["bm_standard"]]
    
    bm <- bm_capacity()[["bm"]]
    
    cap <- df$capacity
    
    avg <- 8.5
    
    valueBox(value =HTML(paste0('<font style="font-size:55px">',round(cap,1),'</font>')),subtitle = HTML(paste0('<font style="font-size:20px">Capacity</font>',br(),"Average working Hours: ",round(avg,1),br(),"Actual FTE: ",round(df$actual_FTE,1),br(),"Implied FTE: ",round(df$implied_FTE,1))),icon = icon("user-circle"),color = "red",width = 3)
    
  })
  
  output$bm_box <- renderUI({
    
    bm <- bm_capacity()[["bm"]]
    
    e <- bm$Efficiency *100
    
    u <- bm$Utilisation *100
    
    a <- round(bm$abs*100,1)
    
    o <- round(bm$OT_paid,1)
    
    w <- paste0(" Country- ",input$current_cntry,", ", "Function- ", input$current_funcs,", ", "Team- ",input$current_team, ", ", "Analysis Period- ",input$current_period)
    
    box(width = 3,solidHeader = T,
        HTML(paste0('<i class="glyphicon glyphicon-bookmark" style="font-size: 1.5em;"></i><font style="font-size:20px;font-family:"Times New Roman" ;font-weight: 400>'," Benchmark Parameters",'</font>')), br(),
        HTML(paste0('<font style="font-size:15px">'," Efficiency: ",e,"%",'</font>')),br(),
        HTML(paste0('<font style="font-size:15px">'," Utilisation: ",u,"%",'</font>')),br(),
        HTML(paste0('<font style="font-size:15px">'," Absetiseem: ",a,"%",'</font>')),br(),
        HTML(paste0('<font style="font-size:15px">'," Over Time(OT): ",o," hrs",'</font>')),br(),
        HTML(paste0('<i class="fas fa-info-circle" style="font-size: 1em;color:red"></i><font style="font-size:10px">',w,'</font>'))
    )
    
  })
  
  # Fourth Row in p1 ----
  
  historical <- reactive({
    
    print("---- Historical reactive ----")
    
    req(input$current_cntry)
    
    req(input$current_funcs)
    
    req(input$current_team)
    
    req(input$bm_period)
    
    funcs_filter <- switch(input$current_funcs, 
                           All ={
                             cf_df()
                           },{
                             input$current_funcs
                           }
    )
    
    daily_df <- switch(input$current_team,
                       All = {
                         daily_allocation_og %>% filter(Country %in% input$current_cntry, 
                                                        OpsFunc %in% funcs_filter)
                       },
                       {
                         daily_allocation_og %>% filter(Country %in% input$current_cntry, 
                                                        OpsFunc %in% funcs_filter, 
                                                        Team %in% input$current_team)
                       }
    )
    
    daily_df$Staff_Name <- toupper(daily_df$Staff_Name)
    
    # Analysis from the most recent month
    current_month <- max(daily_df$YYMM)
    
    # Create the date filter 
    current_date <- as.Date(paste0(current_month,"-01"),"%Y-%m-%d")
    
    # Divide the data equal parts based on benchmark period 
    
    print("Filter Two years of data")
    
    # date_filter = format(seq(current_date,by = "-1 month", length.out =as.numeric(24)),"%Y-%m")
    
    date_filter <- "2018-01"
    
    daily_df <- daily_df %>% filter(YYMM %in% date_filter)
    
    staff_df <- switch(input$current_team,
                       All = {
                         staff_hours_og %>% filter(Country %in% input$current_cntry, 
                                                   OpsFunc %in% funcs_filter)
                       },
                       {
                         staff_hours_og %>% filter(Country %in% input$current_cntry,
                                                   OpsFunc %in% funcs_filter, 
                                                   Team %in% input$current_team)
                       }
    )
    
    staff_df$Staff_Name <- toupper(staff_df$Staff_Name)
    
    staff_df <- staff_df %>% filter(YYMM %in% date_filter)

    # Create a mapping table:
    
    mapping <- data.frame(YYMM  = unique(daily_df$YYMM),stringsAsFactors = F) %>% 
               arrange(desc(YYMM)) %>% 
               mutate(grouping = rep(seq(1,(length(unique(daily_df$YYMM))/as.numeric(input$bm_period))),
                                    each = input$bm_period))
    
    daily_df <- left_join(daily_df, mapping, by = "YYMM")
    
    staff_df <- left_join(staff_df, mapping, by = "YYMM")
    
    # Calculate Efficiency and utilisation

    key <- calculate_key(daily = daily_df, staff = staff_df)
    
    # monthly combined data
    
    current_daily <- daily_df %>% filter(grouping %in% 1)
    
    current_staff <- staff_df %>% filter(grouping %in% 1)
    
    current_data <- left_join(current_daily,current_staff,
                              by=c("Staff_Name","Date","OpsFunc",
                                   "Team","WeekSun_Date","Country",
                                   "Day","YYMM","Year","grouping"))
    
    current_data <- distinct(current_data)
    
    current_data[is.na(current_data)] <- 0
    
    current_data <- current_data[current_data$Workout_Hrs>0,]
    
    monthly_current_data <- rep(list(current_data),length(key))
    
    # Monthly data 
    
    monthly_daily_df <- split(daily_df,daily_df$grouping)
    
    monthly_staff_df <- split(staff_df,staff_df$grouping)
    
    # prepare monthly data 
    
    w = mapply(eup_cal,monthly_daily_df,monthly_staff_df,key,SIMPLIFY = F)
    
    monthly_bm = do.call("rbind",w)
    
    # Use the E,U,OT of each month to calculate capacity
    
    cap_with_ot = mapply(capacity_monthly,
                         monthly_current_data,
                         w,1,SIMPLIFY = F)
    
    cap_with_ot = do.call("rbind",cap_with_ot)
    
    out = list(capacity_with_ot = cap_with_ot, bm = monthly_bm)
    
    return(out)
    
  }) 
  
  monthly_best <- reactive({
    
    print(" ---- Monthly Best ----")
    
    req(input$current_period)
    
    req(input$current_cntry)
    
    req(input$current_funcs)
    
    req(input$current_team)
    
    # input = data.frame(current_cntry = "SG", current_funcs = "CASH", current_team = "SGTT DAH", current_period = 1,stringsAsFactors = F)

    # Create filter for fucntions and teams 
    funcs_filter <- switch(input$current_funcs, 
                           All ={
                             cf_df()
                           },{
                             input$current_funcs
                           }
                     )
    
    daily_df  <- switch(input$current_team,
                       All = {
                         daily_allocation_og %>% 
                           filter(Country %in% input$current_cntry,
                                  OpsFunc %in% funcs_filter)
                       },
                       {
                         daily_allocation_og %>% 
                           filter(Country %in% input$current_cntry, 
                                  OpsFunc %in% funcs_filter, 
                                  Team %in% input$current_team)
                       }
                        )
    
    daily_df$Staff_Name <- toupper(daily_df$Staff_Name)
    
    # Analysis from the most recent month
    current_month <- max(daily_df$YYMM)
    
    # Create the date filter 
    current_date <- as.Date(paste0(current_month,"-01"),"%Y-%m-%d")
    
    # Get the past months data
    if(input$current_period == 2018){
      date_filter = format(seq(as.Date("2018-01-01"),
                               by="1 month",length.out = 12),"%Y-%m")
    }else{
      date_filter = format(seq(current_date,
                               by = "-1 month", 
                               length.out = as.numeric(input$current_period)),"%Y-%m")
    }
    
    # filter period
    
    daily_df_current <- daily_df %>% filter(YYMM %in% date_filter)
    
    
    staff_df <- switch(input$current_team,
                       All = {
                         staff_hours_og %>% 
                           filter(Country %in% input$current_cntry,
                                  OpsFunc %in% funcs_filter)
                       },
                       {
                         staff_hours_og %>% 
                           filter(Country %in% input$current_cntry, 
                                  OpsFunc %in% funcs_filter, 
                                  Team %in% input$current_team)
                       }
    )
    
    staff_df$Staff_Name <- toupper(staff_df$Staff_Name)
    
    staff_df_current <- staff_df %>% filter(YYMM %in% date_filter)
    
    # Calculate Efficiency and utilisation
    key <- calculate_key(daily = daily_df_current, 
                         staff = staff_df_current)
    
    current_data <- full_join(daily_df_current,staff_df_current,
                              by=c("Staff_Name","Date","OpsFunc",
                                   "Team","WeekSun_Date","Country",
                                   "Day","YYMM","Year"))
    
    current_data <- distinct(current_data)
    
    current_data[is.na(current_data)] <- 0
    
    current_data <- current_data[current_data$Workout_Hrs>0,]
    
    # Use the best metrics across the one year
    
    monthly_data <- historical()[["bm"]]

    monthly_eff <- max(monthly_data$Efficiency)
    
    monthly_ut <- max(monthly_data$Utilisation)
    
    monthly_imp_ot <- max(monthly_data$OT_imp)
    
    monthly_paid_ot <- max(monthly_data$OT_paid)
    
    monthly_abs <- min(monthly_data$abs)
    
    monthly_hrs <- max(monthly_data$actual_workout_hrs)
    
    combined_table <- data.frame(Efficiency = monthly_eff, 
                                 Utilisation=monthly_ut, 
                                 OT_imp = monthly_imp_ot, 
                                 OT_paid = monthly_paid_ot,
                                 abs = monthly_abs, 
                                 actual_workout_hrs = monthly_hrs,
                                 stringsAsFactors = F)
    
    # calculate the capacity 
    print("Calculate capacity")
    
    capacity_with_ot <- capacity_new(x = current_data,
                                     bm = combined_table, 
                                     o = (combined_table$actual_workout_hrs))
    
    capacity_without_ot <-capacity_new(x = current_data,
                                       bm = combined_table, 
                                       o = (combined_table$actual_workout_hrs - combined_table$OT_paid))
    
    capacity_standard <- capacity_new(x = current_data,
                                      bm = combined_table, 
                                      o = 8.5)
    
    out = list(mb_cap_with_ot = capacity_with_ot, 
               mb_cap_without_ot = capacity_without_ot, 
               mb_bm = combined_table, 
               mb_standard =  capacity_standard)
    
    return(out)
    
  }) 
  
  output$his_box <- renderUI({
    
    bm <- monthly_best()[["mb_bm"]]
    # assign("mb_bm",bm,envir = .GlobalEnv)
    
    e <- bm$Efficiency *100
    
    u <- bm$Utilisation *100
    
    a <- round(bm$abs*100,1)
    
    o <- round(bm$OT_paid,1)
    
    w <- paste0(" Country- ",input$current_cntry,", ", "Function- ", input$current_funcs,", ", "Team- ",input$current_team, ", ", "Analysis Period- ",input$current_period)
    
    box(width = 3,solidHeader = T,
        HTML(paste0('<i class="glyphicon glyphicon-calendar" style="font-size: 1.5em;"></i><font style="font-size:20px;font-family:"Times New Roman" ;font-weight: 400>'," Historical Best Parameters",'</font>')), br(),
        HTML(paste0('<font style="font-size:12px"> Calculated using past 12 Months data</font>')),hr(),
        HTML(paste0('<font style="font-size:15px">'," Efficiency: ",e,"%",'</font>')),br(),
        HTML(paste0('<font style="font-size:15px">'," Utilisation: ",u,"%",'</font>')),br(),
        HTML(paste0('<font style="font-size:15px">'," Absetiseem: ",a,"%",'</font>')),br(),
        HTML(paste0('<font style="font-size:15px">'," Over Time(OT): ",o," hrs",'</font>')),br(),
        HTML(paste0('<i class="fas fa-info-circle" style="font-size: 1em;color:red"></i><font style="font-size:10px">',w,'</font>'))
    )
  })
  
  output$his_cap <- renderUI({
    
    df <- monthly_best()[["mb_cap_with_ot"]]
    
    bm <- monthly_best()[["mb_bm"]]
    
    cap <- df$capacity
    
    avg <- bm$actual_workout_hrs
    
    valueBox(value =HTML(paste0('<font style="font-size:55px">',round(cap,1),'</font>')),subtitle = HTML(paste0('<font style="font-size:20px">Capacity with OT </font>',br(),"Average working Hours: ",round(avg,1),br(),"Actual FTE: ",round(df$actual_FTE,1),br(),"Implied FTE: ",round(df$implied_FTE,1))),icon = icon("user-clock"),color = "lime",width = 3)
    
  })
  
  output$his_without_ot <- renderUI({
    
    df <- monthly_best()[["mb_cap_without_ot"]]
    
    bm <- monthly_best()[["mb_bm"]]
    
    cap <- df$capacity
    
    avg <- bm$actual_workout_hrs - bm$OT_paid
    
    valueBox(value =HTML(paste0('<font style="font-size:55px">',round(cap,1),'</font>')),subtitle = HTML(paste0('<font style="font-size:20px">Capacity without OT</font>',br(),"Average working Hours: ",round(avg,1),br(),"Actual FTE: ",round(df$actual_FTE,1),br(),"Implied FTE: ",round(df$implied_FTE,1))),icon = icon("user"),color = "navy",width = 3)
  })
  
  output$his_standard <- renderUI({
    
    df <- monthly_best()[["mb_standard"]]
    
    bm <- monthly_best()[["mb_bm"]]
    
    cap <- df$capacity
    
    avg <- 8.5
    
    valueBox(value =HTML(paste0('<font style="font-size:55px">',round(cap,1),'</font>')),subtitle = HTML(paste0('<font style="font-size:20px">Capacity</font>',br(),"Average working Hours: ",round(avg,1),br(),"Actual FTE: ",round(df$actual_FTE,1),br(),"Implied FTE: ",round(df$implied_FTE,1))),icon = icon("user-circle"),color = "black",width = 3)
    
  })
  
  # Fifth Row in p1 ----
  
  best_benchmark <- reactive({
    
    # print("best benchmark")
    bm_capacity <- bm_capacity()[["bm_c1"]]
    
    print(bm_capacity)
  
    monthly_best <- monthly_best()[["mb_cap_with_ot"]]
    
    print(monthly_best)
    
    compare_capacity <- data.frame(capacity = c(bm_capacity$capacity, 
                                                monthly_best$capacity))
    
    row.names(compare_capacity) <- c("bm","mb")
    
    # Highest Capacity
    best_capacity <- row.names(compare_capacity)[compare_capacity$capacity == max(compare_capacity$capacity)]
    
    benchmark <- switch(best_capacity,
                       bm = {bm_capacity()[["bm"]]},
                       mb = {monthly_best()[["mb_bm"]]}
    )
    
    benchmark$actual_workout_hrs <- 9.5
    
    if(input$sa_eff == T){
      benchmark$Efficiency <- 1.01 * benchmark$Efficiency 
    }
    
    if(input$sa_ut == T){
      benchmark$Utilisation <- 1.01 * benchmark$Utilisation
    }
    
    if(input$sa_ot == T){
      benchmark$actual_workout_hrs <- 1 + 9.5
    }
    
    out <- list(best_cap = max(compare_capacity$capacity), best_bm = benchmark)
    
    return(out)
    
  }) 
  
  observe({
    e <- best_benchmark()[["best_bm"]]
    
    eff <- e$Efficiency *100
    
    updateNumericInput(session, inputId = "sim_eff_num", value = eff)
  })
  
  observe({
    e <- best_benchmark()[["best_bm"]]
    
    uti <- e$Utilisation * 100
    
    updateNumericInput(session, inputId = "sim_ut_num", value = uti)
  })
  
  observe({
    best_bm <- best_benchmark()[["best_bm"]]
    
    ot <- round(best_bm$actual_workout_hrs ,3)
    
    updateNumericInput(session, inputId = "sim_ot_num", value = ot)
  })
  
  observe({
    x <- input$sim_eff_num
    
    shinyWidgets::updateKnobInput(session = session,inputId = "sim_eff",label = "Efficiency:",value = x)
  })
  
  observe({
    x <- input$sim_ut_num
    
    shinyWidgets::updateKnobInput(session = session,inputId = "sim_uti",label = "Utilisation:",value = x)
    
  })
  
  observe({
    x <- input$sim_ot_num
    
    shinyWidgets::updateKnobInput(session = session,inputId = "sim_ot",label ="Average Workout Hrs with OT:",value = x)
  })
  
  simulation <- reactive({
    
    print("---- simulation ----")
    
    req(input$sim_eff_num)
    
    req(input$sim_ut_num)
    
    req(input$sim_ot_num)
    
    req(input$current_period)
    
    req(input$current_cntry)
    
    req(input$current_funcs)
    
    req(input$current_team)
    
    sim_combined <- data.frame(Efficiency = input$sim_eff_num/100, 
                               Utilisation = input$sim_ut_num/100, 
                               hours = input$sim_ot_num, 
                               abs = 0.12,
                               stringsAsFactors = F)
    
    # Get the past months data
    # Create filter for fucntions and teams
    
    funcs_filter <- switch(input$current_funcs, 
                           All ={
                             cf_df()
                           },{
                             input$current_funcs
                           }
    )
    
    daily_df  <- switch(input$current_team,
                       All = {
                         daily_allocation_og %>% filter(Country %in% input$current_cntry, 
                                                        OpsFunc %in% funcs_filter)
                       },
                       {
                         daily_allocation_og %>% filter(Country %in% input$current_cntry, 
                                                        OpsFunc %in% funcs_filter,
                                                        Team %in% input$current_team)
                       }
    )
    
    daily_df$Staff_Name <- toupper(daily_df$Staff_Name)
    
    # Analysis from the most recent month
    current_month <- max(daily_df$YYMM)
    
    # Create the date filter 
    current_date <- as.Date(paste0(current_month,"-01"),"%Y-%m-%d")
    
    # Get the past months data
    if(input$current_period == 2018){
      date_filter <- format(seq(as.Date("2018-01-01"),by="1 month",length.out = 12),"%Y-%m")
    }else{
      date_filter <- format(seq(current_date,by = "-1 month", 
                                length.out = as.numeric(input$current_period)),"%Y-%m")
    }
    
    # filter period
    daily_df_current <- daily_df %>% filter(YYMM %in% date_filter)
    
    staff_df <- switch(input$current_team,
                       All = {
                         staff_hours_og %>% filter(Country %in% input$current_cntry, 
                                                   OpsFunc %in% funcs_filter)
                       },
                       {
                         staff_hours_og %>% filter(Country %in% input$current_cntry, 
                                                   OpsFunc %in% funcs_filter, 
                                                   Team %in% input$current_team)
                       }
    )
    staff_df$Staff_Name <- toupper(staff_df$Staff_Name)
    
    staff_df_current <- staff_df %>% filter(YYMM %in% date_filter)
    
    key <- calculate_key(daily = daily_df_current,
                        staff = staff_df_current)
    
    current_data <- full_join(daily_df_current,
                              staff_df_current,
                              by=c("Staff_Name","Date","OpsFunc","Team",
                                   "WeekSun_Date","Country","Day","YYMM",
                                   "Year"))
    
    current_data <- distinct(current_data)
    
    current_data[is.na(current_data)] <- 0
    
    current_data <- current_data[current_data$Workout_Hrs>0,]  
    
    sim_cap <- capacity_new(x = current_data, bm = sim_combined, o = sim_combined$hours)
    
    return(sim_cap)
    
  }) 
  
  output$sim_capacity <- renderUI({
    
    req(input$sim_eff_num)
    
    req(input$sim_ut_num)
    
    req(input$sim_ot_num)
    
    best_bm <- best_benchmark()
    
    best_capacity <- best_bm$best_cap
    
    e <- as.numeric(input$sim_eff_num)/100

    u <- as.numeric(input$sim_ut_num)/100
    
    o <- as.numeric(input$sim_ot_num) 
    
    d <- simulation()
    
    d <- d$capacity
    
    # find the difference between the new and old capcaity number
    change_cap <- d - best_capacity
    
    icon <- ifelse(change_cap>=0,
                   '<i class="fas fa-arrow-down" style="padding-right:0.5em; color:#4ee683;"></i>',
                   '<i class="fas fa-arrow-up" style="padding-right:0.5em;color:#e01b1b;"></i>')
    
    valueBox(value = HTML(paste0(icon,'<font style="font-family:Arial;font-size:30px;color:#000000;">',round(change_cap,1),'</font>')), color = "fuchsia",
             subtitle = HTML(paste0('<b><font style="font-family:Arial;color:#000000;">Capacity Change</font></b>',br(),paste0('<font style="color:#52606d;">Current Capacity: ',round(best_capacity,1),'</font>'),br(),paste0('<font style="color:#52606d;">New Capacity: ',round(d,1),'</font>'),br(),paste0('<font style="color:#52606d;">Efficiency: ',paste0(round(e*100,1),"%"),'</font>'),br(),paste0('<font style="color:#52606d;">Utilisation: ',paste0(round(u*100,1),"%"),'</font>'),br(),paste0('<font style="color:#52606d;">Average Workout',br(),"Hour with OT: ",round(o,1)))),
             icon = icon("sliders-h"),width = "12")
    
    
  })
  
  # Page Three - Capacity Forecasting ----
  
  # Row 1 of page 3 ----
  forecast_cntry <- reactive({
    
    input$forecast_cntry
    
  }) 
  
  forecast_func_df <- reactive({
    
    mm <- daily_allocation_og$OpsFunc[daily_allocation_og$Country == forecast_cntry()]
    
    unique(mm)
    
  })
  
  observe({
    
    print(forecast_func_df())
    
    updateSelectInput(session = session, 
                      inputId = "forecast_func",
                      choices = c("All",forecast_func_df()), 
                      selected = "All")
  })
  
  forecast_funcs <- reactive({
    
    input$forecast_func
  
    })
  
  forecast_df <- reactive({
    
    w <- ifelse(forecast_funcs()=="All",forecast_func_df(),forecast_funcs())
    
    mm <- daily_allocation_og$Team[daily_allocation_og$Country == forecast_cntry() & daily_allocation_og$OpsFunc == w]
    
    unique(mm)
    
  })
  
  observe({
    
    updateSelectInput(session,inputId="forecast_team", 
                      label = "IBGO Team", 
                      choices =c("All",forecast_df()), 
                      selected = "All")
  })
  
  forecast_team <- reactive({
    
    req(input$forecast_team)
    
    input$forecast_team
    
  })
  
  # Create a variable that stores the list of teams that needs to be filtered
  
  selected_teams <- reactive({
    
    # Filter country 
    
    teamTable <- daily_allocation_og %>% 
                 select(Country,OpsFunc,Team) %>% 
                 distinct()
    
    teamTable <- teamTable %>% filter(Country %in% input$forecast_cntry)
    
    if(input$forecast_func == "All"){
      
      functionList <- teamTable$OpsFunc[teamTable$Country %in% input$forecast_cntry]
      
      functionList <- unique(functionList)
      
    }else{
      
      functionList <- input$forecast_func
      
    }
    
    teamTable <- teamTable %>%
                 filter(OpsFunc %in% functionList)
    
    if(input$forecast_team == "All"){
      
      teamList <- teamTable$Team[teamTable$OpsFunc %in% functionList]
      
    }else{
      
      teamList <-  input$forecast_team
      
    }
    
    teamTable <- teamTable %>% filter(Team %in% teamList)
    
    return(teamTable)
    
  })
  
  dailyFiltered <- reactive({
    
    filters <- selected_teams()
    
    daily_df <- daily_allocation_og %>%
                filter(Country %in% filters$Country, 
                       OpsFunc %in% filters$OpsFunc, 
                       Team %in% filters$Team)
    
    daily_df$Staff_Name <- toupper(daily_df$Staff_Name)
    
    return(daily_df)
    
  })  
  
  staffFiltered <- reactive({
    
    filters <- selected_teams()
    
    staff_df <- staff_hours_og %>%  
                filter(Country %in% filters$Country, 
                       OpsFunc %in% filters$OpsFunc, 
                       Team %in% filters$Team)
    
    staff_df$Staff_Name <- toupper(staff_df$Staff_Name)
    
    
    return(staff_df)
    
  })
  
  model <- reactive({
    
    print(" ---- Forecasting FTE ---- ")
    
    # Create filter for fucntions and teams 
    daily_df <- dailyFiltered()
    
    staff_df <- staffFiltered()
    
    out = fte_forecast(country = unique(daily_df$Country) ,
                       functions = unique(daily_df$OpsFunc), 
                       team = unique(daily_df$Team), 
                       horizon = as.numeric(input$horizon)+2, 
                       daily = daily_df,
                       staff = staff_df)
    
    return(out)
  }) 
  
  model_eval <- reactive({
    
    req(input$eval_horizon)
    
    req(input$start_month)
    
    req(input$pred_horizon)
    
    #input = data.frame(eval_horizon = "6", pred_horizon = "3", start_month = "2018-11",stringsAsFactors = F)
    
    new_envir <- new.env()
    
    # Create multiple training and testing sets ----
    
    print("Time Series Linear Model")
  
    # Create filter for fucntions and teams 
    daily_df <- dailyFiltered()
    
    cntry <- unique(daily_df$Country)
    
    func <- unique(daily_df$OpsFunc)
    
    team <- unique(daily_df$Team)
    
    arima_evaluation_results <- model_evaluation(cntry = cntry, func = func, team= team, data = daily_df , monthsYYMM = input$start_month, horizon = input$eval_horizon, pred = input$pred_horizon, model = "arima")
    
    tslm_evaluation_results <- model_evaluation(cntry = cntry, func = func, team= team, data = daily_df , monthsYYMM = input$start_month, horizon = input$eval_horizon, pred = input$pred_horizon, model = "tslm")
    
    exp_evaluation_results <- model_evaluation(cntry = cntry, func = func, team= team, data = daily_df , monthsYYMM = input$start_month, horizon = input$eval_horizon, pred = input$pred_horizon, model = "exp")
    
    new_envir$arima_evaluation <-  arima_evaluation_results
    
    new_envir$tslm_evaluation <- tslm_evaluation_results
    
    new_envir$exp_evaluation <- exp_evaluation_results
    
    return(new_envir)
    
  })
  
  output$evaluation <- renderPlot({
    
    print(" ---- Model Evaluation Plot ---- ")
    
    yymm <- input$yymm
    
    model_evaluation <- model_eval()
    
    arima <- model_evaluation$arima_evaluation
    
    tslm <- model_evaluation$tslm_evaluation
    
    exp <- model_evaluation$exp_evaluation
    
    # Plot the graphs ----
    
    df <- arima$Data
    
    forecast <- arima$Forecast
    
    tslm_forecast <- tslm$Forecast
    
    exp_forecast <- exp$Forecast
    
    model_used <- paste("Models Evaluated- Arima:", arima$Model,", Exp:",exp$Model, ", TSLM:", tslm$Model, sep = " ")
    
    cols <- c("ARIMA"="#828282","TSLM"="#d11919","Exp"="#793030")
    
    p <- ggplot(df,aes(y = working_hrs,x = as_date(WeekSun_Date))) +
          geom_line() +
          geom_line(data = forecast, aes(y = `Point Forecast`, x = as_date(WeekSun_Date),colour = "ARIMA")) +
          geom_line(data = tslm_forecast, aes(y = `Point Forecast`, x = as_date(WeekSun_Date),colour = "TSLM")) +
          geom_line(data = exp_forecast, aes(y = `Point Forecast`, x = as_date(WeekSun_Date),colour = "Exp")) +
          geom_vline(aes(xintercept = as_date(min(forecast$WeekSun_Date)))) +
          xlab("WeekSun_Date") +
          ylab("Working_Hours") +
          ggtitle("Model Evaluation") +
          bgcolor("#f5f5f5") +
          scale_colour_manual(name="Models",values=cols) + 
          theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                plot.title = element_text(hjust = 0.5),
                plot.margin = unit(c(0,0.5,0,0.5), "cm"))
    
    # Create a table that compares the matrix of the models
    
    arima_eval <- as.data.frame(arima$Evaluation)
    
    arima_eval <- arima_eval %>% mutate(Model = "ARIMA")
    
    tslm_eval <- as.data.frame(tslm$Evaluation) 
    
    tslm_eval <- tslm_eval %>% mutate(Model = "TSLM")
    
    exp_eval <- as.data.frame(exp$Evaluation)
    
    exp_eval <- exp_eval %>% mutate(Model = "EXP")
    
    eval_df <- rbind(arima_eval,tslm_eval,exp_eval)
    
    eval_df <- eval_df %>% filter(data %in% "Test set") %>% 
                           spread(key = Evaluation ,value = value)
    
    try <- gather(eval_df,key = "Evaluation",value = "value",-data,-Model)
    
    try$value <- round(try$value,2)
    
    label <- try 
    
    label$value <- label$Evaluation
    
    label$Model <- "Evaluation"
    
    try2 <- rbind(try, label)
    
    try_try <- ggplot(try2,aes(x = Evaluation , y = factor(Model),label = format(value,nsmall=1),colour = Model)) +
              geom_text(size = 4.5) + theme_bw() + scale_y_discrete(limits = c("TSLM", "EXP", "ARIMA","Evaluation")) +
              scale_color_manual(values=c("#828282","#000000","#793030","#d11919")) + 
              ggtitle("Model Evaluation") + 
              theme(panel.grid.major = element_blank(), 
                    plot.title = element_text(hjust = 0.5),
                    legend.position = "none",
                    panel.border = element_blank(), 
                    axis.text.x = element_blank(),
                    axis.ticks = element_blank(),
                    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")) + 
                    xlab(NULL) + 
                    ylab(NULL)
    
    dff <- data.frame(x = 1, y = 1,model = model_used,stringsAsFactors = F)
    
    test_text <- ggplot(dff, aes(x = x, y = y, label = format(model, nsmall=1))) + 
                 geom_text(size = 4.5) +
                 theme(panel.grid.major = element_blank(), 
                      plot.title = element_text(hjust = 0.5),
                      legend.position = "none",
                      panel.border = element_blank(), 
                      axis.text.x = element_blank(),
                      axis.text.y = element_blank(),
                      axis.ticks = element_blank(),
                      panel.grid.minor = element_blank(),
                      panel.background = element_blank(),
                      plot.margin = margin(0.3, 0.3, 0.3, 0.3, "cm")) + 
                      xlab(NULL) + 
                      ylab(NULL) 
    
    Layout <- grid.layout(nrow = 3, ncol = 1, heights = unit(c(6,3,1), c("null","null","null")))
    
    grid.show.layout(Layout)
    
    vplayout <- function(...) {
      grid.newpage()
      pushViewport(viewport(layout = Layout))
    }
    
    subplot <- function(x, y) viewport(layout.pos.row = x,
                                       layout.pos.col = y)
    
    mmplot <- function(a, b, c) {
      vplayout()
      print(a, vp = subplot(1, 1))
      print(b, vp = subplot(2, 1))
      print(c, vp = subplot(3, 1))
    }
    mmplot(p,try_try,test_text)
  })
  
  output$forecasting_plot <- renderPlot({
    
    out <- model()
    
    plot(out$model,showgap = F, 
         xaxt='n',xlab = "Weeks",
         ylab = "Total Workout Hours", 
         main = "Capacity Planning - Weekly Total Workout Hours Forecasting")
  
    })
  
  monthly_model <- reactive({
    
    print("monthly_model")
    
    d <- model()$all_data
    
    # Create Monthly data 
    dd <- apply(X = d,1,daily_data)
    
    dailyData  <- do.call("rbind",dd)
    
    # Monthly data 
    
    dailyData$YYMM <- format(dailyData$weekending, "%Y-%m")
    
    monthly_data <- dailyData %>% 
                    group_by(YYMM,type) %>% 
                    summarise(monthly_workout_hrs = sum(workout_hrs))
    
    monthly_data <- monthly_data %>% 
                    mutate(YYMM2 = as.Date(paste0(YYMM,"-",1)))
    
    df2 <- monthly_data %>% filter(type %in% "Forecast")
  
    return(df2)
    
  }) 
  
  monthly_table <- reactive({
    
    print("monthly_table()")
    
    s <- model()[["final_pred"]]
    
    months <- model()[["end_months"]]

    months <- format(as_date(months),"%Y-%m")
    
    # Create Monthly data 
    dd <- apply(X = s,1,daily_data)
    
    dailyData <- do.call("rbind",dd)
    
    # Monthly data 
    
    dailyData $YYMM <- format(dailyData $weekending,"%Y-%m")
    
    monthly_data_before <- dailyData %>% 
                           group_by(YYMM,type) %>%
                           summarise(Forecasted_WorkoutHrs= round(sum(workout_hrs),1))
    
    monthly_data_before$YYMM <- as.character(monthly_data_before$YYMM)
    
    mm <- as.data.frame(monthly_data_before)
    
    mm <- mm[-nrow(mm),]
    
    return(mm)
    
  })
  
  # Row 2 of page 3 ----
  
  fte_calculation <- reactive({
  
    w <- switch(input$parameter,
               "1" = {
                 cap_12()
               },
               "2" = {
                 bm_capacity()[["bm"]]
               },
               "3" = {
                 monthly_best()[["mb_bm"]]
               }
    )
    return(w)
  })
  
  actual_fte <- reactive({
    
    print("---- Actual FTE ----")
  
    #input = data.frame(forecast_cntry = "SG", forecast_funcs = "CASH", forecast_team = "EWSS", fte2 = 1)
    
    daily_df <- dailyFiltered()
    
    # Analysis from the most recent month
    current_month <- max(daily_df$YYMM)
    
    # Create the date filter 
    current_date <- as.Date(paste0(current_month,"-01"),"%Y-%m-%d")
    
    # Get the past 12 months data
    date_filter <- format(seq(current_date,by = "-1 month", 
                              length.out =as.numeric(1)),"%Y-%m")
    
    daily_df <- daily_df %>% filter(YYMM %in% date_filter)
    
    staff_df <- staffFiltered()
    
    staff_df <- staff_df %>% filter(YYMM %in% date_filter)
    
    # current FTE
    print("---- current FTE ----")
    
    current_data <- full_join(daily_df,staff_df,by=c("Staff_Name","Date","OpsFunc",
                                                     "Team","WeekSun_Date","Country",
                                                     "Day","YYMM","Year"))
    
    current_data <- distinct(current_data)
    
    current_data[is.na(current_data)] <- 0
    
    current_data <- current_data[current_data$Workout_Hrs>0,]
    
    xx <- distinct(current_data %>% select(Date,Staff_Name,Team))
    
    FTE_actual = nrow(xx)/length(unique(current_data$Date))
    
    return(FTE_actual)
    
  }) # ---- working here ----
  
  # Row 2 of page 3 ----
  
  fte <- reactive({
    
    actual <- actual_fte()
    
    d <- model()$all_data
    
    # Create Monthly data 
    dd <- apply(X = d,1,daily_data)
    
    dailyData  <- do.call("rbind",dd)
    
    # Monthly data 
    dailyData $YYMM <- as.factor(format(dailyData $weekending, "%Y-%m"))
    
    monthly_data <- dailyData %>% 
                    group_by(YYMM,type) %>% 
                    summarise(monthly_workout_hrs = sum(workout_hrs))
    
    monthly_data <- monthly_data %>% 
                  mutate(YYMM2 = as.Date(paste0(YYMM,"-",1)))
    
    df1 <- monthly_data %>% filter(type %in% "Original")
    
    df2 <- monthly_data %>% filter(type %in% "Forecast") %>% 
                            filter(YYMM %in% d3$YYMM)
    
    d2$YYMM2 <- as_date(paste0(d2$YYMM,"-01"))
    
    d3$YYMM2 <- as_date(paste0(d3$YYMM,"-01"))
    
    names(df2)[3] <-  "workout_hrs"
    
    allForecast <- rbind(as.data.frame(df2),as.data.frame(d2),as.data.frame(d3))
    
    return(allForecast)
    
  })
  
  forecast_result <- reactive({
    
    # Calculate FTE with standard working hours - working hours with OT
    
    actual <- actual_fte()
    
    # parameters(p) - user's selection about the parameters to be used
    
    p <- fte_calculation()
    
    d <- monthly_table()
    
    d <- data.frame(d)
    
    d <- d %>% mutate(no_working_days = apply(.['YYMM'],1,count_business_months))
    
    work_out_hrs  <- p$actual_workout_hrs
    
    deno <- p$Efficiency * p$Utilisation * (1-p$abs) *  work_out_hrs
    
    d <- d %>% mutate(denominator = no_working_days*deno)
    
    d <- d %>% mutate(FTE = Forecasted_WorkoutHrs/denominator)
    
    d$capacity <- actual - d$FTE
    
    avg_implied_fte <- round(mean(d$FTE),1)
    
    num_1 <- mean(d$capacity)
    
    # Calculate FTE with standard working hours - working hours without OT
    
    p <- fte_calculation()
    
    d <- monthly_table()
    
    d <- data.frame(d)
    
    d <- d %>% mutate(no_working_days = apply(.['YYMM'],1,count_business_months))
    
    work_out_hrs  <- p$actual_workout_hrs - p$OT_paid
    
    deno <- p$Efficiency * p$Utilisation * (1-p$abs) * work_out_hrs
    
    d <- d %>% mutate(denominator = no_working_days*deno)
    
    d <- d %>% mutate(FTE = Forecasted_WorkoutHrs/denominator)
    
    d$capacity <- actual - d$FTE
    
    avg_implied_fte <- round(mean(d$FTE),1)
    
    num2 <- mean(d$capacity)
    
    # Calculate FTE with standard working hours - 8.5 hrs
    
    p <- fte_calculation()
    
    d <- monthly_table()
    
    d <- data.frame(d)
    
    d <- d %>% mutate(no_working_days = apply(.['YYMM'],1,count_business_months))
    
    work_out_hrs  <- 8.5
    
    deno <- p$Efficiency * p$Utilisation * (1-p$abs) *  work_out_hrs
    
    d <- d %>% mutate(denominator = no_working_days*deno)
    
    d <- d %>% mutate(FTE = Forecasted_WorkoutHrs/denominator)
    
    d$capacity <- actual - d$FTE
    
    avg_implied_fte <- round(mean(d$FTE),1)
    
    num3 <- mean(d$capacity)
    
    # Create the dataframe with the average number of FTE for the different scenarios 
    
    s <- data.frame(scenario1 = num_1, scenario2 = num2, scenario3 = num3, stringsAsFactors = F)
    
    return(s)
    
  })
  
  output$forecast_fte1 <- renderUI({
    
    actual <- actual_fte()
    
    p <- fte_calculation()
    
    d <- monthly_table()
    
    d <- data.frame(d)
    
    d <- d %>% mutate(no_working_days = apply(.['YYMM'],1,count_business_months))
    
    work_out_hrs <- p$actual_workout_hrs
    
    deno <- p$Efficiency * p$Utilisation * (1-p$abs) *  work_out_hrs
    
    d <- d %>% mutate(denominator = no_working_days*deno)
    
    d <- d %>% mutate(FTE = Forecasted_WorkoutHrs/denominator)
    
    d$capacity <- actual - d$FTE
    
    avg_implied_fte <- round(mean(d$FTE),1)
    
    num <- mean(d$capacity)
    
    model <- as.character(unique(d$model))
    
    valueBox(value = HTML(paste0('<font style="font-size:40px">',round(num,1),'</font>',br(),'<font style = "font-size:20px">Average Capacity</font>')),
             subtitle = HTML(paste0('<font style="font-size:15px">','Average Working Hours: ', round(work_out_hrs,1),'hrs',br(),'Average Implied FTE: ',avg_implied_fte,br(),'Model used: ',model,'</font>')),
             icon = icon("user-circle"),
             color = "navy",
             width = 3)
  })
  
  output$forecast_fte2 <- renderUI({
    
    actual <- actual_fte()
    
    p <- fte_calculation()
    
    d <- monthly_table()
    
    d <- data.frame(d)
    
    d <- d %>% mutate(no_working_days = apply(.['YYMM'],1,count_business_months))
    
    work_out_hrs  <- p$actual_workout_hrs - p$OT_paid
    
    deno <- p$Efficiency * p$Utilisation * (1-p$abs) * work_out_hrs
    
    d <- d %>% mutate(denominator = no_working_days*deno)
    
    d <- d %>% mutate(FTE = Forecasted_WorkoutHrs/denominator)
    
    d$capacity <- actual - d$FTE
    
    avg_implied_fte <- round(mean(d$FTE),1)
    
    num <- mean(d$capacity)
    
    model <- as.character(unique(d$model))
    
    valueBox(value =HTML(paste0('<font style="font-size:40px">',round(num,1),'</font>',br(),'<font style="font-size:20px">','Average Capacity','</font>')),subtitle = HTML(paste0('<font style="font-size:15px">', 'Average Working Hours: ', round(work_out_hrs,1),'hrs',br(),'Average Implied FTE: ',avg_implied_fte,br(),'Model used: ',model,'</font>')),icon = icon("user-circle"),color = "navy",width = 3)
  })
  
  output$forecast_fte3 <- renderUI({
    
    actual <- actual_fte()
    
    p <- fte_calculation()
    
    d <- monthly_table()
    
    d <- data.frame(d)
    
    d <- d %>% mutate(no_working_days = apply(.['YYMM'],1,count_business_months))
    
    work_out_hrs <- 8.5
    
    deno <- p$Efficiency * p$Utilisation * (1-p$abs) *  work_out_hrs
    
    d <- d %>% mutate(denominator = no_working_days*deno)
    
    d <- d %>% mutate(FTE = Forecasted_WorkoutHrs/denominator)
    
    d$capacity <-actual - d$FTE
    
    avg_implied_fte <- round(mean(d$FTE),1)
    
    num <- mean(d$capacity)
    
    model <- as.character(unique(d$model))
    
    valueBox(value =HTML(paste0('<font style="font-size:40px">',round(num,1),'</font>',br(),'<font style="font-size:20px">','Average Capacity','</font>')),subtitle = HTML(paste0('<font style="font-size:15px">', 'Average Working Hours: ', round(work_out_hrs,1),'hrs',br(),'Average Implied FTE: ',avg_implied_fte,br(),'Model used: ',model,'</font>')),icon = icon("user-circle"),color = "navy",width = 3)
  })
  
  # Row 3 of page 3 ----
  
  annual_change <- reactive({
    # Determine the annual change 
    # Current workout hours 
    print("Actual FTE")
    
    req(input$horizon)
    
    req(input$fte2)
    
    print("Filter daily allocation data ---")
    
    daily <- dailyFiltered()
    
    staff <- staffFiltered()
  
    #input = data.frame(forecast_cntry = "SG", forecast_funcs = "CASH", forecast_team = "EWSS", fte2 = 1, horizon = 6)
    
    # Analysis from the most recent month
    current_month <- max(daily$YYMM)
    
    # Create the date filter 
    current_date <- as.Date(paste0(current_month,"-01"),"%Y-%m-%d")
    
    # Get the latest month's daily workout hours 
    date_filter <- format(seq(current_date,by = "-1 month", length.out =as.numeric(1)),"%Y-%m")
    
    daily_df <- daily %>% filter(YYMM %in% date_filter)
    
    print("Current workout data ----")
    
    workout_hrs <- sum(daily_df$Workout_Hrs)
    
    print("Forecast Months ---")
    # Forecast the hours based on linear relationship
    
    daily_df_jan <- daily %>% filter(Country %in% input$forecast_cntry, OpsFunc %in% input$forecast_func, Team %in% input$forecast_team, YYMM %in% "2018-01")
    
    workout_hrs_jan <- sum(daily_df_jan$Workout_Hrs)
    
    months_since_2018 <- daily %>% filter(Date > as_date("2018-01-01"))
    
    months_since_2018 <- length(unique(months_since_2018$YYMM))
    
    months <- format(seq(current_date,by = "1 month", 
                         length.out =as.numeric(as.numeric(input$horizon)+1)),"%Y-%m")[-1]
    
    forecasted_hrs <- list()
    
    for (i in 1:length(months)){forecasted_hrs[i] = workout_hrs + (as.numeric(input$fte2)/100*workout_hrs)*i}
    
    forecasted_hrs <- unlist(forecasted_hrs)

    model3_out <- data.frame(YYMM = months,
                             type = "Adjusted",
                             workout_hrs = forecasted_hrs)
    
    return(model3_out)
  })
  
  output$manager_fte1 <- renderUI({
    
    actual <- actual_fte()
    
    assign("actual",actual,envir = .GlobalEnv)
    
    p <- fte_calculation()
    
    assign("p",p,envir = .GlobalEnv)
    
    d <-  annual_change()
    
    d <- data.frame(d)
    
    d <- d %>% mutate(no_working_days = apply(.['YYMM'],1,count_business_months))
    
    assign("d",d,envir = .GlobalEnv)
    
    work_out_hrs  <- p$actual_workout_hrs
    
    print(paste0("work_out_hrs: ",work_out_hrs))
    
    assign("work_out_hrs",work_out_hrs,envir = .GlobalEnv)
    
    deno <- p$Efficiency * p$Utilisation * (1-p$abs) *  work_out_hrs
    
    d <- d %>% mutate(denominator = no_working_days*deno)
    
    d <- d %>% mutate(FTE = workout_hrs/denominator)
    
    d$capacity <- actual - d$FTE
    
    num <- mean(d$capacity)
    
    minCapacity <- round(min(d$capacity),1)
    
    maxCapacity <- round(max(d$capacity),1)
    
    avg_implied_fte <- round(mean(d$FTE),1)
    
    minFTE <- round(min(d$FTE),1)
    
    maxFTE <- round(max(d$FTE),1)
    
    model <- as.character(unique(d$model))
    
    valueBox(value =HTML(paste0('<font style="font-size:40px">',
                                num,'</font>',br(),
                                '<font style="font-size:20px">','Average Capacity','</font>')),
             subtitle = HTML(paste0('<font style="font-size:15px">', 'Average Working Hours: ', round(work_out_hrs,1),'hrs',br(),'Average Implied FTE: ',minFTE," to ",maxFTE,'</font>')),icon = icon("user-circle"),color = "maroon",width = 3)
  })
  
  output$manager_fte2 <- renderUI({
    
    actual <- actual_fte()
    
    p <- fte_calculation()
    
    d <-  annual_change()
    
    d <- data.frame(d)
    
    d <- d %>% mutate(no_working_days = apply(.['YYMM'],1,count_business_months))
    
    work_out_hrs <- p$actual_workout_hrs - p$OT_paid
    
    deno <- p$Efficiency * p$Utilisation * (1-p$abs) * work_out_hrs
    
    d <- d %>% mutate(denominator = no_working_days*deno)
    
    d <- d %>% mutate(FTE = workout_hrs/denominator)
    
    d$capacity <- actual - d$FTE
    
    avg_implied_fte <- round(mean(d$FTE),1)
    
    num <- mean(d$capacity)
    
    model <- as.character(unique(d$model))
    
    valueBox(value =HTML(paste0('<font style="font-size:40px">',round(num,1),'</font>',br(),'<font style="font-size:20px">','Average Capacity','</font>')),subtitle = HTML(paste0('<font style="font-size:15px">','Average Working Hours: ', round(work_out_hrs,1),'hrs',br(),'Average Implied FTE: ',avg_implied_fte,'</font>')),icon = icon("user-circle"),color = "maroon",width = 3)
  })
  
  output$manager_fte3 <- renderUI({
    
    actual <- actual_fte()
    
    p <- fte_calculation()
    
    d <-  annual_change()
    
    d <- data.frame(d)
    
    d <- d %>% mutate(no_working_days = apply(.['YYMM'],1,count_business_months))
    
    work_out_hrs <- 8.5
    
    deno <- p$Efficiency * p$Utilisation * (1-p$abs) *  work_out_hrs
    
    d <- d %>% mutate(denominator = no_working_days*deno)
    
    d <- d %>% mutate(FTE = workout_hrs/denominator)
    
    d$capacity <- actual - d$FTE
    
    avg_implied_fte <- round(mean(d$FTE),1)
    
    num <- mean(d$capacity)
    
    model <- as.character(unique(d$model))
    
    valueBox(value =HTML(paste0('<font style="font-size:40px">',round(num,1),'</font>',br(),'<font style="font-size:20px">','Average Capacity','</font>')),subtitle = HTML(paste0('<font style="font-size:15px">', 'Average Working Hours: ', round(work_out_hrs,1),'hrs',br(),'Average Implied FTE: ',avg_implied_fte,'</font>')),icon = icon("user-circle"),color = "maroon",width = 3)
  })
  
  
  # Row 4 of page 3 ----
  monthly_change <- reactive({
    # Determine the annual change 
    # Current workout hours 
    print("Actual FTE --")
    
    req(input$parameter)
    
    req(input$horizon)
    
    req(input$fte2)
    
    # input = data.frame(forecast_cntry = "SG", forecast_funcs = "CASH", forecast_team = "EWSS", fte2 = 1, horizon = 6)
    
    # Find the current FTE ----
    print("Filter daily allocation data --")
    
    daily_df <- dailyFiltered()
  
    # Analysis from the most recent month
    current_month <- max(daily_df$YYMM)
    
    oldest_month <- min(daily_df$YYMM)
    
    # Create the date filter 
    current_date <- as.Date(paste0(current_month,"-01"),"%Y-%m-%d")
    
    oldest_date <- as.Date(paste0(oldest_month,"-01"),"%Y-%m-%d")
    
    # get the list of previous months 
    months <- format(seq(current_date,by = "-1 month", length.out =as.numeric(as.numeric(12))),"%Y-%m-%d")
    
    oldMonths <- format(seq(oldest_date,by = "1 month", length.out =as.numeric(as.numeric(12))),"%Y-%m-%d")
    
    date_filter <- format(as_date(months),"%Y-%m")
    
    oldDateFilter <- format(as_date(oldMonths),"%Y-%m")
    
    daily_df_new <- daily_df %>% filter(YYMM %in% date_filter)
    
    daily_df_old <- daily_df %>% filter(YYMM %in% oldDateFilter)
    
    monthly_workout_hrs <- daily_df_new %>% group_by(YYMM) %>% summarise(workout_hrs_og = sum(Workout_Hrs))
    
    oldMonthlyworkouthrs <- daily_df_old %>% group_by(YYMM) %>% summarise(workout_hrs_og = sum(Workout_Hrs))
    
    # calculate the percentage change ----
    print("calculate the percentage change --")
    
    percentageChange <- ((mean(monthly_workout_hrs$workout_hrs_og) - mean(oldMonthlyworkouthrs$workout_hrs_og))/mean(monthly_workout_hrs$workout_hrs_og))
    
    g <- percentageChange + 1
    
    # Forecasting ----
    
    print(" ---- Months -----")
  
    months <- format(seq(current_date,by = "1 month", length.out =as.numeric(as.numeric(input$horizon)+1)),"%Y-%m-%d")[-1]
    
    sameMonth <- as.POSIXlt(months)
    
    sameMonth$year <- sameMonth$year - 1
    
    sameMonth <- as.POSIXct(sameMonth)
    
    sameMonthfilter <- format(as_date(sameMonth),"%Y-%m")
    
    w <- daily_df %>% filter(YYMM %in% sameMonthfilter) %>% group_by(YYMM) %>% summarise(workout_hrs = sum(Workout_Hrs))
    
    model2Prediction <- list()
    
    for(i in months){
      # create more
      lastyymm <- as.POSIXlt(i)
      
      lastyymm$year <- lastyymm $year - 1
      
      lastyymm  <- as.POSIXct(lastyymm)
      
      lastyymm <- format(lastyymm,"%Y-%m")
      
      yymm <- format(as_date(i),"%Y-%m")
    
      prediction <- w$workout_hrs[w$YYMM %in% lastyymm]
      
      df <- data.frame(YYMM = yymm, workout_hrs = g*prediction, stringsAsFactors = F)
      
      model2Prediction[[i]]  =  df
      
      w <- rbind(w,df)
    }
    
    w <- do.call("rbind",model2Prediction)
    
    dfAttr <- w %>% select(YYMM,workout_hrs) %>% mutate(type = "Monthly_Change")
    
    dfAttr$type <- as.factor(dfAttr$type)
    
    output <- dfAttr[c("YYMM","type","workout_hrs")]
    
    out <- list(percentage_change =  percentageChange, table = output)
  
    return(out)
  })
  
  output$model3 <- renderUI({
    
    change <- monthly_change()[['percentage_change']]
    box(title = HTML("<b>Model Two -  Seasonality</b>"),width = 3,
                        HTML(paste0("<font> Percentage Change was calculated based on the historical trend and applied to same months of last year to give the forecast.",
                        br(),
                        br(),
                        paste0('<i class="fas fa-chart-area"></i> Percentage Change: ',round(change*100,1),"%"),
                        "</font>")))
  })
  
  output$month_fte1 <- renderUI({
    
    actual <- actual_fte()
    
    p <- fte_calculation()
    
    d <-  monthly_change()[['table']]
    
    d <- data.frame(d)
    
    d <- d %>% mutate(no_working_days = apply(.['YYMM'],1,count_business_months))
    
    work_out_hrs <- p$actual_workout_hrs
    
    deno <- p$Efficiency * p$Utilisation * (1-p$abs) *  work_out_hrs
    
    d <- d %>% mutate(denominator = no_working_days*deno)
    
    d <- d %>% mutate(FTE =workout_hrs/denominator)
    
    avg_implied_fte <- round(mean(d$FTE),1)
    
    d$capacity <- actual - d$FTE
    
    num <- mean(d$capacity)
    
    model <- as.character(unique(d$model))
    
    valueBox(value =HTML(paste0('<font style="font-size:40px">',round(num,1),'</font>',br(),'<font style="font-size:20px">','Average Capacity','</font>')),subtitle = HTML(paste0('<font style="font-size:15px">','Average Working Hours: ', round(work_out_hrs,1),'hrs',br(),'Average Implied FTE: ',avg_implied_fte,'</font>')),icon = icon("user-circle"),color = "orange",width = 3)
  })
  
  output$month_fte2 <- renderUI({
    
    actual <- actual_fte()
    
    p <- fte_calculation()
    
    d <-  monthly_change()[['table']]
    
    d <- data.frame(d)
    
    d <- d %>% mutate(no_working_days = apply(.['YYMM'],1,count_business_months))
    
    work_out_hrs  <- p$actual_workout_hrs - p$OT_paid
    
    deno <- p$Efficiency * p$Utilisation * (1-p$abs) *  work_out_hrs
    
    d <- d %>% mutate(denominator = no_working_days*deno)
    
    d <- d %>% mutate(FTE =workout_hrs/denominator)
    
    avg_implied_fte <- round(mean(d$FTE),1)
    
    d$capacity <- actual - d$FTE
    
    num <- mean(d$capacity)
    
    model <- as.character(unique(d$model))
    
    valueBox(value =HTML(paste0('<font style="font-size:40px">',round(num,1),'</font>',br(),'<font style="font-size:20px">','Average Capacity','</font>')),subtitle = HTML(paste0('<font style="font-size:15px">','Average Working Hours: ', round(work_out_hrs,2),'hrs',br(),'Average Implied FTE: ',avg_implied_fte,'</font>')),icon = icon("user-circle"),color = "orange",width = 3)
  })
  
  output$month_fte3 <- renderUI({
    
    actual <- actual_fte()
    
    p <- fte_calculation()
    
    d <-  monthly_change()[['table']]
    
    d <- data.frame(d)
    
    d <- d %>% mutate(no_working_days = apply(.['YYMM'],1,count_business_months))
    
    work_out_hrs <- 8.5
    
    deno <- p$Efficiency * p$Utilisation * (1-p$abs) *  work_out_hrs
    
    d <- d %>% mutate(denominator = no_working_days*deno)
    
    d <- d %>% mutate(FTE =workout_hrs/denominator)
    
    d$capacity <- actual - d$FTE
    
    avgCapacity <- round(mean(d$capacity),1)
    
    avg_implied_fte <- round(mean(d$FTE),1)
    
    minFTE <- round(min(d$FTE),1)
    
    maxFTE <- round(max(d$FTE),1)
    
    minCapacity <- round(min(d$capacity),1)
    
    maxCapacity <- round(max(d$capacity),1)
    
    model <- as.character(unique(d$model))
    
    valueBox(value =HTML(paste0('<font style="font-size:40px">',avgCapacity,'</font>',br(),'<font style="font-size:20px">','Average Capacity','</font>')),subtitle = HTML(paste0('<font style="font-size:15px">','Average Working Hours: ', round(work_out_hrs,2),'hrs',br(),'Average Implied FTE: ',avg_implied_fte,'</font>')),icon = icon("user-circle"), color = "orange", width = 3)
  })
  
  
  # Row 0 of page 3 ----
  output$para1 <- renderUI({
    
    p <- fte_calculation()
    
    valueBox(value = paste0(round(p$Efficiency*100,1),'%'), subtitle = "Efficiency",icon = icon("cog"),color = "yellow", width = 3)

  })
  
  output$para2 <- renderUI({
    
    p <- fte_calculation()
    
    valueBox(value = paste0(round(p$Utilisation*100,1),'%'), subtitle = "Utilisation",icon = icon("cube"),color = "yellow", width = 3)
    
  })
  
  output$para3 <- renderUI({
    
    p <- fte_calculation()
    
    valueBox(value = paste0(round(p$abs*100,1),'%'), subtitle = "Absenteeism",icon = icon("business-time"),color = "yellow", width = 3)
    
  })
  
  output$actual_fte <- renderUI({
    
    a <- actual_fte()
    
    valueBox(value = paste0(round(a,1)), subtitle = "Actual FTE",icon = icon("users"),color = "yellow", width = 3)
    
  })
  
  # Plot ----
  
  parameter <- reactive({
    
    w <- switch(input$parameter,
               "1" = {
                 cap_12()
               },
               "2" = {
                 bm_capacity()[["bm"]]
               },
               "3" = {
                 monthly_best()[["mb_bm"]]
               }
    )
    
    return(w)
  })
  
  forecasted_hrs <- reactive({
    
    actual <- actual_fte()
    
    # Model 1----
    d <- model()$all_data
    
    # Model 2 ----
    d2 <- annual_change()
    
    # Model 3 ---- 
    d3 <- monthly_change()[['table']]
    
    # Create Daily data 
    # Create Monthly data 
    dd <- apply(X = d,1,daily_data)
    
    dailyData  <- do.call("rbind",dd)
    
    # Monthly data 
    dailyData$YYMM <- as.factor(format(dailyData$weekending, "%Y-%m"))
    
    monthly_data <- dailyData %>% 
      group_by(YYMM,type) %>% 
      summarise(monthly_workout_hrs = sum(workout_hrs))
    
    monthly_data <- monthly_data %>% 
      mutate(YYMM2 = as.Date(paste0(YYMM,"-",1)))
    
    df1 <- monthly_data %>% filter(type %in% "Original") 
    
    df2 <- monthly_data %>%
      filter(type %in% "Forecast") %>% 
      filter(YYMM %in% d3$YYMM)
    
    d2$YYMM2 <- as_date(paste0(d2$YYMM,"-01"))
    
    d3$YYMM2 <- as_date(paste0(d3$YYMM,"-01"))
    
    # all the forecast 
    
    names(df2)[3] <- "workout_hrs"
    
    d2$type <- "Model 3"
    
    d3$type <- "Model 2"
    
    allForecast_hrs<- rbind(as.data.frame(df2),as.data.frame(d2),as.data.frame(d3))
    
    allForecast_hrs$workout_hrs <- round(as.numeric(allForecast_hrs$workout_hrs),0)
    
    allForecast_hrs$type <- gsub("Forecast","Model 1",allForecast_hrs$type)
    
    allForecast_hrs <-allForecast_hrs %>% select(YYMM,type,workout_hrs)
    
    return(allForecast_hrs)
    
  })
  
  forecasted_fte <- reactive({
    actual <- actual_fte()
    
    # Model 1----
    d <- model()$all_data
    
    # Model 2 ----
    d2 <- annual_change()
    
    # Model 3 ---- 
    d3 <- monthly_change()[['table']]
    
    # Create Daily data 
    # Create Monthly data 
    dd <- apply(X = d,1,daily_data)
    
    dailyData  <- do.call("rbind",dd)
    
    # Monthly data 
    dailyData$YYMM <- as.factor(format(dailyData$weekending, "%Y-%m"))
    
    monthly_data <- dailyData %>% 
      group_by(YYMM,type) %>% 
      summarise(monthly_workout_hrs = sum(workout_hrs))
    
    monthly_data <- monthly_data %>% 
      mutate(YYMM2 = as.Date(paste0(YYMM,"-",1)))
    
    df1 <- monthly_data %>% filter(type %in% "Original") 
    
    df2 <- monthly_data %>%
      filter(type %in% "Forecast") %>% 
      filter(YYMM %in% d3$YYMM)
    
    d2$YYMM2 <- as_date(paste0(d2$YYMM,"-01"))
    
    d3$YYMM2 <- as_date(paste0(d3$YYMM,"-01"))
    
    # all the forecast 
    
    names(df2)[3] <- "workout_hrs"
    
    d2$type <- "Model 3"
    
    d3$type <- "Model 2"
    
    allForecast_all <- rbind(as.data.frame(df2),as.data.frame(d2),as.data.frame(d3))
    
    # Average FTE Needed
    
    parameters <- parameter()
    
    # parameters <- parameter_p
    
    commonDenominator <- (1-parameters$abs) * parameters$Efficiency * parameters$Utilisation * parameters$actual_workout_hrs
    
    allForecast <- allForecast_all %>% 
      mutate(business_days = unlist(lapply(allForecast_all$YYMM,businessDays))) %>% 
      mutate(FTE = workout_hrs/(business_days*commonDenominator))
    
    dataTable_new <- allForecast %>% select(YYMM,type,FTE)
    
    dataTable_new$FTE <- round(dataTable_new$FTE,2)
    
    dataTable_new$type <- gsub("Forecast","Model 1",dataTable_new$type)
    
    return(dataTable_new)
    
  })
  
  output$monthly_forecast <- renderPlot({
    
    actual <- actual_fte()
    
    # Model 1----
    d <- model()$all_data
    
    # Model 2 ----
    d2 <- annual_change()
    
    # Model 3 ---- 
    d3 <- monthly_change()[['table']]
    
    # Create Daily data 
    # Create Monthly data 
    dd <- apply(X = d,1,daily_data)
    
    dailyData  <- do.call("rbind",dd)
    
    # Monthly data 
    dailyData$YYMM <- as.factor(format(dailyData$weekending, "%Y-%m"))
    
    monthly_data <- dailyData %>% 
                    group_by(YYMM,type) %>% 
                    summarise(monthly_workout_hrs = sum(workout_hrs))
    
    monthly_data <- monthly_data %>% 
                    mutate(YYMM2 = as.Date(paste0(YYMM,"-",1)))
    
    df1 <- monthly_data %>% filter(type %in% "Original") 
    
    df2 <- monthly_data %>%
           filter(type %in% "Forecast") %>% 
           filter(YYMM %in% d3$YYMM)
    
    d2$YYMM2 <- as_date(paste0(d2$YYMM,"-01"))
    
    d3$YYMM2 <- as_date(paste0(d3$YYMM,"-01"))
    
    # Add additional line to joint the line segment - forecast
    print("parameters of forecasted model")
    
    x_start <- monthly_data %>% filter(type %in% "Original")
    
    x_start <- max(x_start$YYMM2)
    
    y_start <- monthly_data %>% filter(type %in% "Original", YYMM2 %in% x_start)
    
    y_start <- y_start$monthly_workout_hrs
    
    x_end = monthly_data %>% filter(type %in% "Forecast")
    
    x_end = min(x_end$YYMM2)
    
    y_end_df = monthly_data %>% filter(type %in% "Forecast")
    
    y_end = y_end_df %>% filter(YYMM2 %in% x_end)
    
    y_end = y_end$monthly_workout_hrs
    
    x_last = max(df2$YYMM2)
    
    print("print df2 ---- ")
    
    y_last = df2$monthly_workout_hrs[df2$YYMM2 == x_last]
    
    # Add additional line - manager's forecast 1 
    
    y_end1 <- d2$workout_hrs[1]
    
    x_last1 <- max(d2$YYMM2)
    
    y_last1 <- d2$workout_hrs[d2$YYMM2 == x_last1]
    
    # Add additional line - manager's forecast 2
    
    y_end2 <- d3$workout_hrs[1]
    
    x_last2 <- max(d3$YYMM2)
    
    y_last2 <- d3$workout_hrs[d3$YYMM2 == x_last2]
    
    # all the forecast 
    
    names(df2)[3] <- "workout_hrs"
    
    d2$type <- "Model 3"
    
    d3$type <- "Model 2"
    
    allForecast_all <- rbind(as.data.frame(df2),as.data.frame(d2),as.data.frame(d3))
    
    # allForecast_all$type <- gsub("Forecast","Model 1",allForecast_all$type)
    
    # Average FTE Neede
    
    parameters <- parameter()
    
    # parameters <- parameter_p

    commonDenominator <- (1-parameters$abs) * parameters$Efficiency * parameters$Utilisation * parameters$actual_workout_hrs
    
    allForecast <- allForecast_all %>% 
                   mutate(business_days = unlist(lapply(allForecast_all$YYMM,businessDays))) %>% 
                   mutate(FTE = workout_hrs/(business_days*commonDenominator))
    
    assign("allForecast",allForecast,envir = .GlobalEnv)
    
    dataTable <- allForecast %>% select(YYMM,type,FTE)
    
    dataTable$FTE <- round(dataTable$FTE,2)

    labelTable <- data.frame(YYMM = unique(dataTable$YYMM)) %>% 
                  mutate(type = "YYMM") %>% 
                  mutate(FTE = unique(dataTable$YYMM))
    
    dataTable_all <- rbind(dataTable,labelTable)
    
    allForecast_hrs <- allForecast_all
    
    allForecast_hrs$workout_hrs <- round(as.numeric(allForecast_hrs$workout_hrs),0)
    
    labelTable_hrs <- labelTable
    
    allForecast_hrs$type <- gsub("Forecast","Model 1",allForecast_hrs$type)
    
    allForecast_hrs <-allForecast_hrs %>% select(YYMM,type,workout_hrs)
    
    names(labelTable_hrs)[which(names(labelTable_hrs) == "FTE")] = "workout_hrs"
    
    allForecast_hrs  <- rbind(allForecast_hrs, labelTable_hrs)

    data_table_hrs <- ggplot(allForecast_hrs,aes(x = YYMM, y = factor(type),label = format(workout_hrs,nsmall=1),colour = type)) +
      geom_text(size = 3.5) + theme_bw() + scale_y_discrete(limits = c("Model 3", "Model 2", "Model 1","YYMM")) +
      scale_color_manual(values=c("#828282","#d11919","#7a3030","#000000")) + 
      ggtitle("Forecasted Workout Hours") +
      theme(panel.grid.major = element_blank(), 
            plot.title = element_text(hjust = 0.5),
            legend.position = "none",
            panel.border = element_blank(), 
            axis.text.x = element_blank(),
            axis.ticks = element_blank(),
            plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")) + 
      xlab(NULL) + 
      ylab(NULL)
    
    dataTable_new <- dataTable_all
    
    dataTable_new$type <- gsub("Forecast","Model 1",dataTable_new$type)
    
    data_table <- ggplot(dataTable_new,aes(x = YYMM, y = factor(type),label = format(FTE,nsmall=1),colour = type)) +
                  geom_text(size = 3.5) + theme_bw() + scale_y_discrete(limits = c("Model 3", "Model 2", "Model 1","YYMM")) +
                  scale_color_manual(values=c("#828282", "#d11919", "#7a3030","#000000")) + 
                  ggtitle("Forecasted FTE") + 
                  theme(panel.grid.major = element_blank(), 
                        plot.title = element_text(hjust = 0.5),
                        legend.position = "none",
                        panel.border = element_blank(), 
                        axis.text.x = element_blank(),
                        axis.ticks = element_blank(),
                        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")) + 
                        xlab(NULL) + 
                        ylab(NULL)
    
    avgFTE <-  round(mean(allForecast$FTE),2)
    
    # confidence level of 80%
    upper<- monthly_data %>% ungroup() %>% filter(type %in% "Upper_80") %>% select(monthly_workout_hrs)
    
    upper<- upper$monthly_workout_hrs
    
    lower <- monthly_data %>% ungroup() %>% filter(type %in% "Lower_80") %>% select(monthly_workout_hrs)
    
    lower <- lower$monthly_workout_hrs
    
    monthlyPlot <-  ggplot(df1, aes(x = YYMM2, y = monthly_workout_hrs)) + geom_line(colour='black') +
                    geom_smooth(aes(x=YYMM2, y=workout_hrs),colour='#828282', data = df2, stat='identity') +
                    geom_smooth(aes(x=YYMM2, y=workout_hrs),colour='#7a3030', data = d2, stat='identity') +
                    geom_smooth(aes(x=YYMM2, y=workout_hrs),colour='#d11919', data = d3, stat='identity') +
                    geom_segment(aes(x=x_start, y=y_start, xend=x_end, yend=y_end), colour="#828282",size = 1,show.legend = T) +
                    geom_segment(aes(x=x_start, y=y_start, xend=x_end, yend=y_end1), colour='#7a3030',size = 1 ) +
                    geom_segment(aes(x=x_start, y=y_start, xend=x_end, yend=y_end2), colour='#d11919',size = 1) +
                    geom_dl(aes(x_last, y_last, label = "Model 1"), method = list(dl.trans(x = x + 0.7), dl.trans(y = y + 0.2),cex = 1.0,color="#828282"),color="#828282") +
                    geom_dl(aes(x_last1, y_last1, label = "Model 3"), method = list(dl.trans(x = x + 0.7),  dl.trans(y = y + 0.2), cex = 1.0,color="#7a3030"),color="#7a3030") +
                    geom_dl(aes(x_last2, y_last2, label = "Model 2"), method = list(dl.trans(x = x + 0.7),  dl.trans(y = y + 0.2), cex = 1.0,color="#d11919"),color="#d11919") +
                    scale_x_date(date_breaks="1 month", date_labels= "%Y-%m") +
                    geom_vline(aes(xintercept=as.numeric(df1$YYMM2[length(df1$YYMM2)])),linetype=4, colour="black") 
         
      title  <- paste0("You will need an average of ",avgFTE," FTE to manage the forecasted workout hours in the next ",input$horizon,"months")
      
      monthlyPlot <- monthlyPlot + 
                     ggtitle(title) + 
                     theme_bw() + 
                     xlab(label = "Month") +
                     bgcolor("#f5f5f5") + 
                     ylab(label = "Monthly Workout Hours") +
                     theme(panel.border = element_blank(), 
                           plot.title =  element_text(hjust = 0.5),
                          panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank(), 
                          axis.line = element_blank(),
                          axis.text.x = element_text(angle = 30, hjust = 1)
                     )
      
    Layout <- grid.layout(nrow = 3, ncol = 1, heights = unit(c(6,3,3), c("null","null", "null")))
    
    grid.show.layout(Layout)
    
    vplayout <- function(...) {
      grid.newpage()
      pushViewport(viewport(layout = Layout))
    }
    
    subplot <- function(x, y) viewport(layout.pos.row = x,
                                       layout.pos.col = y)
    
    mmplot <- function(a, b, c) {
      vplayout()
      print(a, vp = subplot(1, 1))
      print(b, vp = subplot(2, 1))
      print(c, vp = subplot(3, 1))
    }
    
    mmplot(monthlyPlot, data_table_hrs,data_table)
    
    })
  
  # Download datasets ----
  file_download <- reactive({
    
    df <- switch(input$dataset,
      "1" = {
        forecasted_hrs()
      },
      "2" = {
        forecasted_fte()
        }
      )
    return(df)
    
  })
  
  file_name <-  reactive({
      
      fileName <- switch(input$dataset,
                   "1" = {
                     "Forecasted_Workout_Hours"
                   },
                   "2" = {
                     "Forecasted_FTE"
                   }
      )
      
      fileName <- paste(fileName,Sys.Date(),sep="_")
      
      return(fileName)
      
    })
  
  output$downloadData <- downloadHandler(
    filename = function() { 
      paste(file_name(),".csv", sep="")
    },
    content = function(file) {
      write.csv(file_download(), file,row.names = F)
    })
}
