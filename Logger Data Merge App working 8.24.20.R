library(shiny)
library(shinydashboard)
library(lubridate)
library(shinyWidgets)
library(DT)
library(data.table)
library(tidyr)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyverse)
library(readxl)
library(writexl)
library(scales)
library(leaflet)
library(leaflet.providers)


# .. Data
setwd("C:/Users/colin/Downloads/")  # <-- YOU GOTTA CHANGE THIS
serverData <- read_excel("MARINE_LOGGER_DATABASE (62).xlsx")
serverData <- as.data.frame(serverData)

#### External Data ----

coralwatch <- read_csv("external_data_noaa_daily_sst.csv")
coralwatch$DateStart <- format(as.Date(coralwatch$DateStart))
coralwatch$DateEnd <- format(as.Date(coralwatch$DateEnd))
coralwatch.mean.sstmax <- mean(coralwatch$SST_MAX)
coralwatch.mean.sstmin <- mean(coralwatch$SST_MIN)
coralwatch.mean.sstavg <- mean(coralwatch$SST_AVG)


oni <- read.csv("external_data_noaa_monthly_oni.csv")
oni$DateStart <- format(as.Date(oni$DateStart))
oni$DateEnd <- format(as.Date(oni$DateEnd))

pdo <- read.csv("external_data_noaa_monthly_pdo.csv")
pdo$DateStart <- format(as.Date(pdo$DateStart))
pdo$DateEnd <- format(as.Date(pdo$DateEnd))

external_data_choices <- list(
  "None" = "None",
  "NOAA Coral Reef Watch - Maximum SST" = "SSTMAX",
  "NOAA Coral Reef Watch - Average SST" = "SSTAVG",
  "NOAA Coral Reef Watch - Minimum SST" = "SSTMIN",
  "NOAA Coral Reef Watch - SST Anomaly" = "SSTANOM",
  "NOAA Coral Reef Watch - Degree Heating Weeks" = "DHW",
  "NOAA Coral Reef Watch - Bleaching Alert Level" = "BAA",
  "NOAA Oceanic Nino Index" = "ONI",
  "NOAA Pacific Decadal Oscillation" = "PDO"
)


#### Define UI #### 
ui <- dashboardPage(
  dashboardHeader(title = "Guam Marine Water Quality", titleWidth = 300),
  dashboardSidebar(width = 300,
                   sidebarMenu(
                     menuItem("Visualize/Export data", tabName = "exportdata", icon = icon("file-export"), startExpanded = FALSE),
                     menuItem("Data options", tabName= "dataoptions", icon = icon("filter"),
                              h4("Filter data by:"),
                              dateRangeInput("logger_select_daterange", "Date range:",
                                             start = "2016-05-06", end = NULL,
                                             startview = "year", language = "en",
                                             format = "yyyy/mm/dd"),
                              pickerInput("logger_select_site", "Sites to show:",
                                          choices = c("Piti", "Agat", "Adelup", "Adelup Far"),
                                          selected = c("Piti", "Agat", "Adelup"),
                                          options = list(`actions-box` = TRUE),
                                          multiple = T),
                              pickerInput("logger_select_datatype", "Data Type to show:",
                                          choices = c("Temp_C", "Light_Intensity", "Abs_Pressure_kPa"),
                                          selected = c("Temp_C", "Light_Intensity", "Abs_Pressure_kPa"),
                                          options = list(`actions-box` = TRUE),
                                          multiple = T),
                              h4("Summarize data by:"),
                              radioButtons("data_summarylist", "label" = NULL,
                                           choices = c("None", "Day", "Week", "Month"),
                                           inline = TRUE,
                                           selected = ("Day")),
                              downloadButton('downLoadFilter',"Download filtered data"),
                              h4("Choose an external data set to display."),
                              selectInput("lit_siteplot_externaldata", "label" = NULL,
                                          choices = external_data_choices),
                              sliderInput("lit_siteplot_external_alpha", "External dataset transparency:",
                                          min = 0, max = 1, value = 0.3, round = 2)),     
                     menuItem("Upload", tabName = "upload", icon = icon("file-upload")),
                     menuItem("Update external databases", tabName = "externaldatasets", icon = icon("globe")),
                     menuItem("Contact", tabName = "contact", icon = icon("id-card")),
                     hr()
                     )
                   ),
  
  dashboardBody(
    #### Tab: Upload ####
    tabItems(
      tabItem(tabName = "upload", icon = icon("dashboard"),
              fluidRow(
                box(
                  title = "Choose logger file for upload", width =20,
                  fileInput('fileUpload', 'Choose file', width = 100),
                  downloadButton("downloadData", label = "Download Merged File")),
              fluidRow(DT::dataTableOutput('tblMerged')))
              ),
      
      #### Tab: Export data ####
      tabItem(tabName = "exportdata", 
              fluidRow(
                box(
                title = "Plot", width = 12, solidHeader = TRUE, status = "primary", collapsible = TRUE,
                plotOutput("loggerplot", height = 500)),
                ),
              fluidRow(
                    leafletOutput("guam"),
                    p()
                ),
              fluidRow(box(
                title = "Summary Table Output", width = 12, solidHeader = TRUE,
                status = "primary", collapsible = TRUE, 
                DT::dataTableOutput("datable")))
              
      ),
      # .. Tab: Exernal Datasets ----
      tabItem(tabName = "externaldatasets",
              h2("External Datasets"),
              box(title = h3("NOAA Coral Reef Watch SST Data"), status = "primary", solidHeader = TRUE, width = 3,
                  h5("This data is pulled from the NOAA Coral Reef Watch data set and has a 5-km resolution.
                      Available data includes daily average SST, high SST, low SST, anomaly (difference from monthly average),
                      degree heating weeks, and bleaching alert level."),
                  h5("For more information, see www.coralreefwatch.noaa.gov/satellite/index.php."),
                  tags$hr(),
                  actionBttn(
                    inputId = "dataset_update_noaacrw",
                    label = "Update NOAA Coral Reef Watch SST Dataset Now",
                    color = "primary",
                    style = "bordered"
                  ),
                  tags$hr(),
                  verbatimTextOutput(outputId = "dataset_update_noaacrw_output")
              ),
              
              
              box(title = h3("NOAA Aqua MODIS SST Data"), status = "info", solidHeader = TRUE, width = 3,
                  h5("This data is pulled from the NOAA Aqua MODIS 1-Day Composite data set."),
                  h5("For more information, see https://coastwatch.pfeg.noaa.gov/infog/MB_sstd_las.html."),
                  tags$hr(),
                  actionBttn(
                    inputId = "dataset_update_noaasst",
                    label = "Update NOAA Aqua MODIS SST Dataset Now",
                    color = "primary",
                    style = "bordered"
                  ),
                  tags$hr(),
                  verbatimTextOutput(outputId = "dataset_update_noaasst_output")
              ),
              
              box(title = h3("Oceanic Nino Index"), status = "success", solidHeader = TRUE, width = 3,
                  h5("The Oceanic Nino Index is based on centered 30-year base periods updated every five years."),
                  h5("For more information, see https://origin.cpc.ncep.noaa.gov/products/analysis_monitoring/ensostuff/ONI_v5.php."),
                  tags$hr(),
                  actionBttn(inputId = "dataset_update_noaaoni",
                             label = "Update NOAA ONI Dataset Now",
                             color = "primary",
                             style = "bordered"),
                  tags$hr(),
                  verbatimTextOutput(outputId = "dataset_update_noaaoni_output")),
              
              box(title = h3("Pacific Decadal Oscillation"), status = "warning", solidHeader = TRUE,
                  width = 3,
                  h5("The Pacific Decadal Oscillation "),
                  tags$hr(),
                  actionBttn(inputId = "dataset_update_noaapdo",
                             label = "Update NOAA PDO Dataset Now",
                             color = "primary",
                             style = "bordered"),
                  tags$hr(),
                  verbatimTextOutput(outputId = "dataset_update_noaapdo_output")
              )
      )
    )
  )
)


server <- function(input, output, session) {
  #### Filter data ####
  
  datafiltered <- reactive({
    serverData %>% 
      filter(Site %in% input$logger_select_site) %>% 
      filter(Date > input$logger_select_daterange[1]) %>%
      filter(Date < input$logger_select_daterange[2]) %>% 
      filter(Data_Type %in% input$logger_select_datatype)
    })
  
  #### Summarize #### 
  plot_data <- reactive({
    
    if(all(input$data_summarylist == "Day")){return(
      datafiltered() %>% 
        mutate(Date = as.Date(Date)) %>%
        group_by(Date, Site, Data_Type) %>% 
        summarise("Value" = mean(Value))
    )}
    
    else if(all(input$data_summarylist == "Week")){return(
      datafiltered() %>% 
        mutate(Date = floor_date(as.Date(Date), unit="week")) %>%
        group_by(Date, Site, Data_Type) %>%
        summarise("Value" = mean(Value))
    )}
    
    else if(all(input$data_summarylist == "Month")){return(
      datafiltered() %>% 
        mutate(Date = floor_date(as.Date(Date), unit="month")) %>%
        group_by(Date, Site, Data_Type) %>%
        summarise("Value" = mean(Value))
    )}
    
    else if(all(input$data_summarylist == "None")){return(
      datafiltered() %>%
        mutate(Date = as.Date(Date)) %>%
        group_by(Date, Site, Data_Type, Value)
    )}
  })

  output$datable <- renderDT(plot_data(),
                                    filter = 'top',
                                    extensions = 'Buttons', options = list(
                                      dom = 'Blfrtip',
                                      buttons = 
                                        list('copy'),
                                      scrollX = TRUE,
                                      pageLength = 10,
                                      lengthMenu = list(c(10,100,-1), list('10', '100', 'All')),
                                      rownames = FALSE
                                      )
                             )

  output$tblMerged <- DT::renderDataTable({
    if(is.null(mergedData()))
      return()
    
    
    DT::datatable(mergedData(),
                  filter = 'top',
                  extensions = 'Buttons', options = list(
                    dom = 'Blfrtip',
                    buttons = 
                      list('copy'),
                    scrollX = TRUE,
                    pageLength = 10,
                    lengthMenu = list(c(10,100,-1), list('10', '100', 'All')),
                    rownames = FALSE
                  )
    )
  })
  
  #### Merger ####
  contents <- reactive({
    inputFile <- input$fileUpload
    if (is.null(inputFile))
      return()
    # I use Mutate and as.date so the date is the correct object type. I can not get Time to behave as a POSIXct object.
    read_excel(inputFile$datapath) %>%
      group_by("Date_Time" = floor_date(Date_Time, unit = "hour"), Site, Data_Type, Logger_SN) %>%
      summarise("Value" = mean(Value)) %>%
      mutate("Date" = format(Date_Time, "%Y-%m-%d")) %>%
      mutate("Time" = format(Date_Time, "%H:%M:%S")) %>%
      relocate(Date, Time, Site, Data_Type, Value, Logger_SN) %>%
      ungroup() %>%
      select(-c(Date_Time)) 
  })
  
  #contentsplit <- reactive({
   # if(is.null(contents()))
   #   return()
  #  contents() %>% separate(Date_Time, c("Date", "Time"), " ")
 # })
  
  mergedData <- reactive({
    if(is.null(contents()))
      return()
    rbind(x = serverData, y = contents()) #%>%
      #mutate("Date" = format(as.Date(Date), "%Y-%m-%d"))
  })

  #### Download handlers ####
  output$downloadData <- downloadHandler(
    filename = function() {"MARINE_LOGGER_DATABASE.xlsx"},
    content = function(file) {write_xlsx(mergedData(), path = file)}
  )
  
  output$downLoadFilter <- downloadHandler(
    filename = function() {
      paste('Filtered data-', Sys.Date(), '.xlsx', sep = '')
    },
    content = function(file){
      write_xlsx(plot_data(), path = file)
    }
  )
  
  #### Plotting ####
  output$loggerplot <- renderPlot({
    
    ggplot()+
      scale_x_date(breaks = "1 month", labels = date_format("%b %Y"))+
      theme(axis.text.x=element_text(angle=45, hjust=1), axis.title.x=element_blank())+
      facet_grid(rows= vars(Data_Type), scales = "free") +
      lit_siteplot_external_data()+
      geom_line(data = plot_data(), 
                aes(x = Date, y = Value, group = Site, color = Site))
      })
  # ....... External Data ----
  lit_siteplot_external_data <- reactive({
    
    coralwatch_data = coralwatch %>% 
      filter(DateStart > input$logger_select_daterange[1]) %>%
      filter(DateEnd < input$logger_select_daterange[2])
    
    
    if(input$lit_siteplot_externaldata == "NONE"){return(NULL)}
    
    if(input$lit_siteplot_externaldata == "SSTMAX"){return(list(
      geom_rect(data = coralwatch_data, 
                aes(xmin = as.Date(DateStart), xmax = as.Date(DateEnd), ymin = 26, ymax = 34, fill = SST_MAX),
                position = "identity", show.legend = TRUE, alpha = input$lit_siteplot_external_alpha),
      scale_fill_gradient2(high = "red3", mid = "white", low = "blue3", midpoint = coralwatch.mean.sstmax)))}
    
    if(input$lit_siteplot_externaldata == "SSTAVG"){return(list(
      geom_rect(data = coralwatch_data, 
                aes(xmin = as.Date(DateStart), xmax = as.Date(DateEnd), ymin = 26, ymax = 34, fill = SST_AVG),
                position = "identity", show.legend = TRUE, alpha = input$lit_siteplot_external_alpha),
      scale_fill_gradient2(high = "red3", mid = "white", low = "blue3", midpoint = coralwatch.mean.sstavg)))}
    
    if(input$lit_siteplot_externaldata == "SSTMIN"){return(list(
      geom_rect(data = coralwatch_data, 
                aes(xmin = as.Date(DateStart), xmax = as.Date(DateEnd), ymin = 26, ymax = 34, fill = SST_MIN),
                position = "identity", show.legend = TRUE, alpha = input$lit_siteplot_external_alpha),
      scale_fill_gradient2(high = "red3", mid = "white", low = "blue3", midpoint = coralwatch.mean.sstmin)))}
    
    if(input$lit_siteplot_externaldata == "SSTANOM"){return(list(
      geom_rect(data = coralwatch_data, 
                aes(xmin = as.Date(DateStart), xmax = as.Date(DateEnd), ymin = 26, ymax = 34, fill = SST_ANOM),
                position = "identity", show.legend = TRUE, alpha = input$lit_siteplot_external_alpha),
      scale_fill_gradient2(high = "red3", mid = "white", low = "blue3", midpoint = 0)))}
    
    if(input$lit_siteplot_externaldata == "DHW"){return(list(
      geom_rect(data = coralwatch_data, 
                aes(xmin = as.Date(DateStart), xmax = as.Date(DateEnd), ymin = 26, ymax = 34, fill = DHW),
                position = "identity", show.legend = TRUE, alpha = input$lit_siteplot_external_alpha),
      scale_fill_gradient2(high = "red3", mid = "white", low = "blue3", midpoint = 0)))}
    
    if(input$lit_siteplot_externaldata == "BAA"){return(list(
      geom_rect(data = coralwatch_data, 
                aes(xmin = as.Date(DateStart), xmax = as.Date(DateEnd), ymin = 26, ymax = 34, fill = ALERTLEVEL),
                position = "identity", show.legend = TRUE, alpha = input$lit_siteplot_external_alpha),
      scale_fill_gradient2(high = "red3", mid = "white", low = "blue3", midpoint = 0)))}
    
    if(input$lit_siteplot_externaldata == "ONI"){return(list(
      geom_rect(data = oni %>% 
                  filter(DateStart > input$logger_select_daterange[1]) %>%
                  filter(DateEnd < input$logger_select_daterange[2]), 
                aes(xmin = as.Date(DateStart), xmax = as.Date(DateEnd), ymin = 26, ymax = 34, fill = ANOM),
                position = "identity", show.legend = TRUE, alpha = input$lit_siteplot_external_alpha),
      scale_fill_gradient2(high = "red3", mid = "white", low = "blue3", midpoint = 0)))}
    
    if(input$lit_siteplot_externaldata == "PDO"){return(list(
      geom_rect(data = pdo %>% 
                  filter(DateStart > input$logger_select_daterange[1]) %>%
                  filter(DateEnd < input$logger_select_daterange[2]),  
                aes(xmin = as.Date(DateStart), xmax = as.Date(DateEnd), ymin = 26, ymax = 34, fill = PDO),
                position = "identity", show.legend = TRUE, alpha = input$lit_siteplot_external_alpha),
      scale_fill_gradient2(high = "red3", mid = "white", low = "blue3", midpoint = 0)))}
  })
  
  ####Map####
  output$guam <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Esri.WorldStreetMap) %>%
      setView(lng = 144.793732, lat = 13.444304, zoom= 10) %>%
      addMarkers(
        lng = 144.723822, lat = 13.477952,
        label = "Adelup",
        labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "auto", textsize = "12px")) %>%
      addMarkers(
        lng = 144.654832, lat = 13.384732,
        label = "Agat",
        labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "left", textsize = "12px")) %>%
      addMarkers(
        lng = 144.706604, lat = 13.476117,
        label = "Piti",
        labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "left", textsize = "12px")) 
  })
}

# Run the application 
shinyApp(ui = ui, server = server)