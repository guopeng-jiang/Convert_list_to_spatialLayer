library(shiny)

# Define UI for dataset viewer app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("This is a Shiny APP for matching addresses"),
  
  # Sidebar layout with a input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel("Input data panel", 
      
      # Input: Selector for choosing dataset ----
      fileInput("file1", "Please upload data file (xlsx or csv)", accept = c(".xlsx", ".csv"), multiple=F), # Kathy

      selectInput("dataset","Data:", choices = "Files uploaded"), #Kathy
      selectInput("ID","ID Field:", choices = NULL),
      selectInput("variable","Variable:", choices = NULL),
      
      tags$hr(style="border-color: Orange;"),

      # ----
      fileInput("file2", "Please upload data file (xlsx or csv)", accept = c(".xlsx", ".csv"), multiple=F), # Kathy
      
      selectInput("dataset2","Data:", choices = "Files uploaded"), #Kathy
      selectInput("ID2","ID Field:", choices = NULL),
      selectInput("variable2","Variable:", choices = NULL),
      
      actionButton("GoButton","Let's GO!"),
      downloadButton("downloadData", "Download")
      
    ), 
    
    
    # Main panel for displaying outputs ----
    mainPanel("Output result panel", 
              verbatimTextOutput("View", placeholder=TRUE), 
              tableOutput("Your.output")
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
    updateSelectInput(session, "ID", "ID Field:", choices = colnames(dfile$sel))
    })
  
  # ----- 
  observeEvent(input$file2,
               {
    dfile$dd2<-input$file2$datapath
    dfile$df2<-input$file2$name
    updateSelectInput(session, "dataset2", "Data:", choices = dfile$df2)
    })
  
  observeEvent(input$dataset2,
               {
    dfile$id2 = which(dfile$df2 == input$dataset2) #reactive value identifying which file in the list is chosen
    req(dfile$dd2[dfile$id2])
    if (tools::file_ext(dfile$dd2[dfile$id2]) == "xlsx"){
      dfile$sel2 = readxl::read_excel(dfile$dd2[dfile$id2])  #assign the contents of the chosen file to a reactive value
    } else {
      dfile$sel2 = readr::read_csv(dfile$dd2[dfile$id2])
    }
    updateSelectInput(session, "variable2", "Variable:", choices = colnames(dfile$sel2))
    updateSelectInput(session, "ID2", "ID Field:", choices = colnames(dfile$sel2))
    })

  
  # Return the requested dataset ----
  observeEvent(input$GoButton,{
    
    # print(substr(pull(dfile$sel[input$variable]),1, 15)) # All columns in a tibble must be vectors.
    
    # test = "c(\"251 Mill Road, 0 Kashmir Road and 445 Clinton Makaretu Road.\", \"549 Black Road, RD 1, Takapau 4286\"\n)"
    #substr(gsub('\n)', "", noquote(test), fixed = T), 3, nchar(gsub('\n)', "", noquote(test), fixed = T)))
    library(dplyr)
    
    long = data.frame(ID = dfile$sel[input$ID], address = substr( iconv(enc2utf8(pull(dfile$sel[input$variable]) ), sub= "byte"),1,15) )
    short = data.frame(ID = dfile$sel2[input$ID2], address = substr( iconv(enc2utf8(pull(dfile$sel2[input$variable2]) ), sub= "byte"),1,15) )
    
    library(fuzzyjoin)

    fuz_match = stringdist_join(long, short, by = "address", 
                                mode = "left", ignore_case = T, method = "jw",max_dist = 3, 
                                distance_col = "dist") %>% group_by(ID) %>% slice(which.min(dist))
    
    fuz_match$match_yes = mapply(function(a, b) grepl(a, b, fixed = T), 
                                 substr(fuz_match$address.x,1,5), 
                                 substr(fuz_match$address.y,1,5)) 
    
    output$View <- renderPrint({ paste0(sum(fuz_match$match_yes), " detected by fuzzy + hard matching") })
    output$Your.output <- renderTable({ subset(fuz_match, fuz_match$match_yes == TRUE) })
    
    output$downloadData <- downloadHandler(
      filename = function() {
        paste0(dfile$dd[dfile$id], ".xlsx")}, 
      content = function(file) {writexl::write_xlsx(fuz_match, file)})
    
          })

}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
