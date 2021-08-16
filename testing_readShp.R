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

# Define UI for dataset viewer app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Turning a list of names into spatial polygons"),
  
  # Sidebar layout with a input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel("Input data panel", 
                 
                 # ----
                 
                 fileInput(
                   inputId = "filemap",
                   label = "Upload Property_Title. Choose shapefile (shp.)",
                   multiple = F,
                   accept = c(".shp")
                 ),
                 
                 actionButton("GoButton","Let's GO!"),
                 
    ), 
    
    
    # Main panel for displaying outputs ----
    mainPanel("Output result panel", 
              leafletOutput("View")
    )
    
  )
)


# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  
  options(shiny.maxRequestSize=30*1024^2) # increase the maximum upload size to 30MB
  
  mapfile <- reactive({
    
    req(input$filemap)
    
    # shpdf is a data.frame with the name, size, type and datapath of the uploaded files
    shpdf <- input$filemap
    
    # Name of the temporary directory where files are uploaded
    tempdirname <- dirname(shpdf$datapath[1])
    
    # Rename files
    for (i in 1:nrow(shpdf)) {
      file.rename(
        shpdf$datapath[i],
        paste0(tempdirname, "/", shpdf$name[i])
      )
    }
    
    # Now we read the shapefile with read_sf() of rgdal package
    
    mapfile = read_sf(tempdirname, crs =2193) %>% st_transform(4326)
    mapfile
  })
  
  # ----- 
  
  # Return the requested dataset ----
  observeEvent(input$GoButton,{
    
    target_parcels = mapfile()
    
    output$View <- renderLeaflet({ 
      
      labels <- sprintf(
        "<strong>%s</strong><br/>",
        paste(target_parcels$title_no, "--", target_parcels$owners, sep='')
      ) %>% lapply(htmltools::HTML)
      
      m <- leaflet() %>% addTiles() %>% addPolygons(data = target_parcels, weight = 1, label = labels)
      
    })
    
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)