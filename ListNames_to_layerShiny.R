library(shiny)
library(leaflet)
library(dplyr)
library(shinycssloaders)
library(rgdal)
library(plotly)
library(htmltools)
library(DT)
library(shinyjs)
library(sf)

library(mapview)
library(sp)
library(rgdal)
library(rgeos)
library(tidyverse)
library(tm)

# Define UI for dataset viewer app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Query a list of properties from property title using owners' names"),
  
  # Sidebar layout with a input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel("Input data panel", 
                 
                 # Input: Selector for choosing dataset ----
                 fileInput("file1", "Please upload data file (xlsx or csv)", accept = c(".xlsx", ".csv"), multiple=F), # Kathy
                 
                 selectInput("dataset","Data Sheet Uploaded:", choices = "Files uploaded"), #Kathy
                 selectInput("variable","Owners Name Column:", choices = NULL),
                 
                 tags$hr(style="border-color: Orange;"),
                 
                 # ----
                 
                 fileInput("filemap", "Please upload Property_Title including .shp, .dbf, .sbn, .sbx, .shx, .prj", 
                           accept=c('.shp','.dbf','.sbn','.sbx','.shx',".prj"), 
                           multiple=TRUE),
                 
                 actionButton("GoButton","Let's GO!"),
                 downloadButton("downloadData", "Download")
                 
    ), 
    
    
    # Main panel for displaying outputs ----
    mainPanel("Output result panel", 
              mapviewOutput("View")
    )
    
  )
)


# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  
  options(shiny.maxRequestSize=30*1024^2) # increase the maximum upload size to 30MB
  
  dfile = reactiveValues() #create an empty list of reactive values which you'll populate as you go
  
  # ------
  #assign the upload path(s) and filename(s) to reactive value(s) and update the dataset choice list
  observeEvent(input$file1,{
    dfile$dd<-input$file1$datapath
    dfile$df<-input$file1$name
    updateSelectInput(session, "dataset", "Data Sheet Uploaded:", choices = dfile$df)
  })
  
  #update the variable list based on the dataset chosen and assign the contents of the file to a reactive value
  observeEvent(input$dataset,
               {
                 dfile$id<-which(dfile$df==input$dataset) #reactive value identifying which file in the list is chosen
                 req(dfile$dd[dfile$id])
                 if (tools::file_ext(dfile$dd[dfile$id]) == "xlsx") {
                   dfile$sel<-readxl::read_excel(dfile$dd[dfile$id])  #assign the contents of the chosen file to a reactive value
                 } else {
                   dfile$sel<-readr::read_csv(dfile$dd[dfile$id]) # check the extension, if csv. then {csv} else {excel}
                 }
                 updateSelectInput(session, "variable", "Owners Name Column:", choices = colnames(dfile$sel))
               })
  
  mapfile <- reactive({
    
    shpdf <- input$filemap
    if(is.null(shpdf)){
      return()
    }
    previouswd <- getwd()
    uploaddirectory <- dirname(shpdf$datapath[1])
    setwd(uploaddirectory)
    for(i in 1:nrow(shpdf)){
      file.rename(shpdf$datapath[i], shpdf$name[i])
    }
    setwd(previouswd)
    
    map <- readOGR(paste(uploaddirectory, shpdf$name[grep(pattern="*.shp$", shpdf$name)], sep="/"))#,  delete_null_obj=TRUE)
    map <- spTransform(map, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
    
  })
  
  # ----- 
  
  # Return the requested dataset ----
  observeEvent(input$GoButton,{
    
    library(stringr)
    
    ########### Query parcels based on name search ############
    
    name_match = function(keyword, target_field){
      
      count_matches = numeric(length(strsplit(target_field, ", ")[[1]]))
      
      for (i in seq(strsplit(target_field, ", ")[[1]])) {
        count_matches[i] = sum(strsplit(keyword, " ")[[1]] %in% strsplit(strsplit(target_field, ", ")[[1]][i], " ")[[1]])
      }
      
      max(count_matches) >= 2
      
    }
    
    keyword = iconv(enc2utf8(pull(dfile$sel[input$variable])))
    
    AllProperty = mapfile()
    
    target_field = removeWords(AllProperty$owners, c(LETTERS, " & ", " + ", "Limited", "Station", "Trust", "Trustee", "Family", "Farm", "Incorporated"))
    # problem with company names, initials, symbols
    
    random = lapply(keyword, function(keyword){which(mapply(name_match, keyword, strsplit(target_field, ", ") ) == TRUE)} )
    
    target_parcels = AllProperty[unlist(random),]
    
#    target_parcels = target_parcels[target_parcels$Hectares >= 10, ]
    
    output$View <- renderMapview({ 
      
      mapview(target_parcels)
      
    })
    
    output$downloadData <- downloadHandler(
      filename = function() {dfile$dd[dfile$id]}, 
      content = function(file) { st_write(target_parcels, file, driver="ESRI Shapefile") })
    
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
