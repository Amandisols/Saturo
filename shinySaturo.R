# Load the required libraries
library(shiny)
library(readxl)
library(tidyverse)
library(soilDB)
library(ggtern)

#Add text box
#Add texture choice
#Add better table

# User interface
ui <- fluidPage(
  titlePanel("Saturo Data Validation"),  
  p("Saturo is an automated single-ring infiltrometer manufactured by Meter Group. The Saturo infiltrometer estimates field-saturated hydraulic conductivity (Kfs) and associated error (Kfs error) using the two-ponding head approach developed by Reynolds and Elrick (1990). Saturo accomplishes this by maintaining a constant pond of water on the soil surface and using a water pump and an air pump in combination to cycle between high- and low-pressure heads. Saturo uses an internal processor to perform the Kfs calculations and field data can be downloaded from the instrument in .csv or .xlsx format. Kfs and Kfs error can be estimated from each respective pressure cycle, allowing for multiple opportunities to estimate Kfs and Kfs error from a single run with the instrument. However, the Saturo is currently programmed to only use the last pressure cycle of a run to calculate the Kfs and Kfs error results. The last pressure cycle of a run can contain noisy or irregular data, resulting in erroneous estimations of Kfs and Kfs error. This document provides a method to calculate Kfs and Kfs error for every cycle of a Saturo run."),
  
  sidebarLayout(
    sidebarPanel(
      fileInput(
        inputId = "file",
        label = "Upload your Excel file:",
        accept = c(".xlsx", ".xls")  
      ),
      selectInput(inputId = "dropdown",
                  label = "Choose Texture",
                  choices = list(
                    "Clay" = "Clay",
                    "Clay Loam" = "Clay Loam",
                    "Sand" = "Sand"
                  ),
                  selected = "Clay" #default selected
                  ),
      actionButton("process", "Process Data"),
      downloadButton("downloadData", "Download Table")
    ),
    
    mainPanel(
      tableOutput("results_table"),  # Display the results table
      plotOutput("results_plot")     # Display the plot
    )
  )
)

# Define the Server
server <- function(input, output, session) {
  
  # Reactive to store the uploaded data
  uploaded_data <- reactive({
    req(input$file)  # Ensure a file is uploaded
    
    # Read data from both sheets
    sheet1 <- read_excel(input$file$datapath, sheet = "Summary",
                         col_names = c("Settings", "Value"),
                         skip = 2, col_types = c("guess", "numeric"))
    
    sheet2 <- read_excel(input$file$datapath, sheet = "Raw Data")
    

   list(sheet1 = sheet1, sheet2 = sheet2)
  })
  
  
  # Perform calculations on the data
  processed_data <- eventReactive(input$process, {
    req(uploaded_data())
    
    summary <- uploaded_data()$sheet1
    dat <- uploaded_data()$sheet2
    raw <- uploaded_data()$sheet2
    
    
    
    ######################    Extract Metadata   ############################
    # Reformat summary to wide format
    summary <- summary %>% 
      drop_na(Settings) %>%
      spread(key = Settings, value = Value)
    
    # Extract out units of measurement
    ## This is set on the control unit prior to the run. This code executes pattern matching to grab anything with paratheses into a seperate column.
    units <- data.frame(names = names(summary)) %>% 
      mutate(units = str_extract(.[[1]], "(?<=\\()([^()]*?)(?=\\)[^()]*$)")) %>%
      mutate(names = str_replace(.[[1]], " \\s*\\([^\\)]+\\)", "")) 
    
    units2 <- data.frame(names = names(dat)) %>%
      mutate(units = str_extract(.[[1]], "(?<=\\()([^()]*?)(?=\\)[^()]*$)")) %>%
      mutate(names = str_replace(.[[1]], " \\s*\\([^\\)]+\\)", "")) 
    
    units <- rbind(units, units2)
    
    #Change column names in the data sheet to take out units of measurement
    names(dat) <- units2[[1]]
    rm(units2) #remove data frame
    
    # Extract variables for calculations
    presoak <- summary[[1,16]]
    hold <- summary[[1,5]]
    depth <- summary[[1,7]] # Ring depth
    cycles <- summary[[1,10]] 
    radius <- 7.5  # Ring radius
    delta <- round((0.993 * depth) + (0.578 * radius), digits = 1)  # Calculate delta (ring geometry/shape factor)
    p_Kfs <- summary[[1,8]]
    p_Kfs_error <- summary[[1,9]]
    
    
    
    ##############    Determine High/Low pressure periods   ###################
    # Determine high/low pressure periods
    dat <- dat %>%
      mutate(setting = ifelse(Time <= presoak, "presoak", 
                              ifelse(Pressure > 6, "high", 
                                     ifelse(Pressure < 6, "low", NA) ))) 
    
    nhigh <- as.numeric(nrow(dat[dat$setting == "high",])) 
    nlow <- as.numeric(nrow(dat[dat$setting == "low",]))
    
    
    # Add cycle numbers
    dat <- dat %>%
      group_by(setting) %>%
      mutate(cycle = ifelse(setting == "presoak", 0,
                            ifelse(setting == "high", rep(c(1:cycles), each = nhigh/cycles, length.out = n()), 
                                   ifelse(setting == "low", rep(c(1:cycles), each = nlow/cycles, length.out = n()), NA)))) %>%
      ungroup() %>%
      group_by(cycle, setting) %>%
      slice(3:n()) #takes out the first 2 minutes of each pressure setting for every cycle
    
    
    
    ######################    Calculations   ############################

    ## Kfs
    Kfs <- dat %>%
      filter(setting != "presoak") %>%
      group_by(setting, cycle) %>%
      summarise(mFlux = mean(Flux),
                mPress = mean(Pressure)) %>%
      pivot_wider(names_from = setting, values_from = c(mFlux, mPress)) %>%
      mutate(Kfs = (delta * (mFlux_high - mFlux_low)) / (mPress_high - mPress_low)) %>%
      select(cycle, Kfs)
    
    
    ## Kfs error
    high <- dat %>%
      filter(setting == "high") %>%
      group_by(setting) %>%
      rename(highP = Pressure,
             highQ = Flux) %>%
      ungroup()%>%
      select(highP, highQ, cycle)
    
    low <- dat %>%
      filter(setting == "low") %>%
      group_by(setting) %>%
      rename(lowP = Pressure,
             lowQ = Flux) %>%
      ungroup()%>%
      select(lowP, lowQ)
    
    dat2 <- cbind(high, low)
    dat2 <- left_join(dat2, Kfs, by = "cycle")
    
    Kfs_error <- dat2 %>%
      mutate(row_Kfs = (delta * (highQ - lowQ)) / (highP-lowP),
             row_error = (row_Kfs - Kfs)^2 ) %>%
      group_by(cycle) %>%
      summarise(Kfs_error = sqrt(sum(row_error)) / (hold - 2) )
    
    Kfs_data <- left_join(Kfs, Kfs_error, by = "cycle")
    
    
    ## Standard Deviation of water level
    sd_waterlevel <- dat %>%
      filter(cycle != 0) %>%
      group_by(cycle) %>%
      summarise(sd = sd(`Water Level`))
    
    Kfs_data <- left_join(Kfs_data, sd_waterlevel, by = "cycle")
    
    
    
    ######################    Validation tests   ############################
    # CALCULATED TEST: 
      p_Kfs <- signif(p_Kfs, 3)
      Kfs_data$Kfs <- signif(Kfs_data[[2]], 3)
      Kfs_data$calc_check <- Kfs_data$Kfs == p_Kfs
    
    
    # RANGE TEST: 
      # get boundaries for each soil texture to address the extreme values
      data(USDA)
      
      # convert S, Si, C % to whole numbers and add theoretical low/high Db
      USDA <- USDA %>%  
        mutate(across(c(Sand, Silt, Clay), function(x) x*100)) %>%
        mutate(Db_low = 0.9, Db_high = 2.0)
      
      # assign variables and run Rosetta. Output ksat = log10(cm/day)
      vars1 = c('Sand', 'Silt', 'Clay', 'Db_low')
      r1 <- ROSETTA(USDA, vars = vars1)
      
      vars2 = c('Sand', 'Silt', 'Clay', 'Db_high')
      r2 <- ROSETTA(USDA, vars = vars2)
      
      # extract out min/max
      low_r <- r1 %>%
        group_by(Label) %>%
        summarise(ksat_min = min(ksat))
      
      high_r <- r2 %>%
        group_by(Label) %>%
        summarise(ksat_max = max(ksat))
      
      ranges <- left_join(low_r, high_r, by = "Label")
      
      # convert to cm/sec
      ranges <- ranges %>%  
        mutate(across(c(ksat_min, ksat_max), function(x) (10^(x))/86400))
      
      # match units selected for the run. 
      # possible units: cm/s, cm/hr, in/s, in/hr
      ksat_units = units$units[units$names == "Kfs"]
      
      #unit conversions
      convert_units <- function(data, target_units) {
        inches_per_cm = 0.393701
        seconds_per_hour = 3600
        
        # conversions
        if(target_units == "cm/hr") {
          data$ksat_min <- data$ksat_min * seconds_per_hour
          data$ksat_max <- data$ksat_max * seconds_per_hour
        } else if (target_units == "in/s"){
          data$ksat_min <- data$ksat_min * inches_per_cm
          data$ksat_max <- data$ksat_max * inches_per_cm
        } else if (target_units == "in/hr"){
          data$ksat_min <- data$ksat_min * inches_per_cm * seconds_per_hour
          data$ksat_max <- data$ksat_max * inches_per_cm * seconds_per_hour
        } else {
          stop("Unsupported units specified.")
        }
        return(data)
      }
      
      # Perform conversion if necessary. 
      if(ksat_units!="cm/s"){
        ranges <- convert_units(ranges, ksat_units)
      }
      
      # pull out range by specific texture
      texture = input$dropdown
      low <- ranges$ksat_min[ranges$Label == texture]
      high <- ranges$ksat_max[ranges$Label == texture]
      Kfs_data$range_check <- between(Kfs_data$Kfs, low, high)    
    
    
    # MAGNITUDE TEST: 
      Kfs_data$mag_check <- (Kfs_data$Kfs / Kfs_data$Kfs_error) >= 10
    
    
    # WATER LEVEL TEST:
      wl_units = units$units[units$names == "Water Level"]
      sd_threshold = 0.25 #cm  
      
      convert_units2 <- function(threshold, target_units) {
        inches_per_cm = 0.393701
        millimeters_per_cm = 10
        
        # conversions
        if(target_units == "mm") {
          threshold = threshold * millimeters_per_cm
        } else if (target_units == "in"){
          threshold = threshold * inches_per_cm
        } else {
          stop("Unsupported units specified.")
        }
        return(data)
      }
      
      # Perform conversion if necessary. 
      if(wl_units!="cm"){
        ranges <- convert_units2(sd_threshold, wl_units)
      }
      
      #Perform test
      Kfs_data$level_check <- Kfs_data$sd <= sd_threshold
    
    # Rename columns
    names(Kfs_data) <- c("Cycle", 
                         paste0("Kfs (", units$units[units$names == "Kfs"], ")"), 
                         paste0("Kfs Error (", units$units[units$names == "Kfs Error"], ")"),
                         paste0("Water Level SD (", units$units[units$names == "Water Level"], ")"),
                         "Calculated Test",
                         "Range Test",
                         "Error Test", 
                         "Water Level Test")
      
    # Change column order
    Kfs_data <- Kfs_data[,c(1,4,3,2,5,6,7,8)]
      
    # Change view of data to scientific notation with 3 significant digits
    ## Preserve original table first
    Kfs_table <- Kfs_data
      
    Kfs_table[[3]] <- formatC(Kfs_table[[3]], format = "e", 2)
    Kfs_table[[4]] <- formatC(Kfs_table[[4]], format = "e", 2)
    Kfs_table[[2]] <- signif(Kfs_table[[2]], 2)
    
    return(Kfs_table)
    
  })
  
  
  # Render the results in a table
  output$results_table <- renderTable({
    req(processed_data())  # Ensure processed data is available
    processed_data()
  })
  
  # Render the results plot
  output$results_plot <- renderPlot({
    req(uploaded_data())  # Ensure processed data is available
    raw <- uploaded_data()$sheet2
    
    # Bring in the raw data again
    names(raw)[2] <- "Time"
    
    # Pivot the dataframe to long format
    raw_long <- raw %>%
      ungroup() %>%
      select(-`Record ID`) %>%
      pivot_longer(!Time, names_to = "metric", values_to = "value")
    
    # Plot
    
      ggplot(raw_long, aes(Time, value)) +
      geom_line(aes(color = metric), size = 1) +
      facet_grid(metric~., scales = "free_y", switch = "y") +
      ylab(" ") +
      scale_colour_brewer(type = "div") +
      theme_bw()+
      theme(strip.text.y = element_text(size = 10, hjust = 0.5,
                                        vjust = 0.5, face = 'bold')) +
      theme(legend.title=element_blank())
  
  })
    

#Provide a download handler for the processed data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("processed_data", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(processed_data())  # Ensure processed data is available
      write.csv(processed_data(), file, row.names = FALSE)
    }
  )
}

# Run the Shiny App
shinyApp(ui, server)
