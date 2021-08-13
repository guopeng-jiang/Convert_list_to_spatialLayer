library(shiny)
library(leaflet)
library(dplyr)
library(shinycssloaders)
library(rgdal)
library(plotly)
library(htmltools)
library(DT)
library(shinyjs)

# Define UI for dataset viewer app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Turning a list of names into spatial polygons"),
  
  # Sidebar layout with a input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel("Input data panel", 
                 
                 # Input: Selector for choosing dataset ----
                 fileInput("file1", "Please upload data file (xlsx or csv)", accept = c(".xlsx", ".csv"), multiple=F), # Kathy
                 
                 selectInput("dataset","Data:", choices = "Files uploaded"), #Kathy
                 selectInput("variable","Variable:", choices = NULL),
                 
                 tags$hr(style="border-color: Orange;"),
                 
                 # ----
                 
                 actionButton("GoButton","Let's GO!"),
                 downloadButton("downloadData", "Download")
                 
    ), 
    
    
    # Main panel for displaying outputs ----
    mainPanel("Output result panel", 
              leafletOutput("View")
    )
    
  )
)


# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  
  dfile = reactiveValues() #create an empty list of reactive values which you'll populate as you go
  
  # ------
  #assign the upload path(s) and filename(s) to reactive value(s) and update the dataset choice list
  observeEvent(input$file1,{
    dfile$dd<-input$file1$datapath
    dfile$df<-input$file1$name
    updateSelectInput(session, "dataset", "Data:", choices = dfile$df)
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
                 updateSelectInput(session, "variable", "Variable:", choices = colnames(dfile$sel))
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
    
    library(sp)
    library(rgdal)
    library(rgeos)
    library(tidyverse)
    library(tm)
    library(sf)
    
    AllProperty = read_sf("N:/Gorden_Jiang/Tukituki_LUC/ALLproperties.shp", crs =2193) %>% st_transform(4326)
    keyword = iconv(enc2utf8(pull(dfile$sel[input$variable])))
    
    target_field = removeWords(AllProperty$owners, c(LETTERS, " & ", " + ", "Limited", "Station", "Trust", "Trustee", "Family", "Farm"))
    # problem with company names, initials, symbols
    
    random = lapply(keyword, function(keyword){which(mapply(name_match, keyword, strsplit(target_field, ", ") ) == TRUE)} )
    
    target_parcels = AllProperty[unlist(random),]
    target_parcels = target_parcels[target_parcels$Hectares >= 10, ]
    
    output$View <- renderLeaflet({ 
      
      labels <- sprintf(
        "<strong>%s</strong><br/>",
        paste(target_parcels$title_no, "--", target_parcels$owners, sep='')
      ) %>% lapply(htmltools::HTML)
      
      m <- leaflet() %>% addTiles() %>% addPolygons(data = target_parcels, weight = 1, label = labels)
      
      })

    output$downloadData <- downloadHandler(
      filename = function() { dfile$dd[dfile$id] }, 
      content = function(file) {writeOGR(target_parcels, file, driver="ESRI Shapefile")})
    
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)

