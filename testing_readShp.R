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

library(sp)
library(rgdal)
library(rgeos)
library(tidyverse)
library(tm)
library(mapview)

# Define UI for dataset viewer app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Turning a list of names into spatial polygons"),
  
  # Sidebar layout with a input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel("Input data panel", 
                 
                 # ----
                 
                 fileInput("filemap", "", accept=c('.shp','.dbf','.sbn','.sbx','.shx',".prj"), multiple=TRUE),
                 
                 actionButton("GoButton","Let's GO!"),
                 
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
    
    target_parcels = mapfile()
    
    output$View <- renderMapview({ 
      
      mapview(target_parcels)
      
    })
    
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)