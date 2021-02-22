# this app is a minimal example of uploading a layout file using the layout uploads module
library(tidyverse)
library(shiny)
library(shinyalert) # needed to upload layout files
library(shinycssloaders) # needed for the spinner on the layout plots
library(varhandle) 
library(shinyWidgets)
library(glue)
source("layout_filter_module.R")

ui <- fluidPage(useShinyalert(),
                titlePanel("Filter layouts module"),
                
                sidebarLayout(
                    sidebarPanel(
                        filterlayoutUI("filter_layout")[[1]]
                    ),
                    mainPanel(
                        p("accessed externally"),
                        verbatimTextOutput("selection_external"),
                        p("accessed internal to module"),
                        filterlayoutUI("filter_layout")[[2]]
                    )
                )
)


server <- function(input, output, session) {
    ### layout munching
    layout_ex <- reactive(tibble(well = c("A1", "A2", "A3", "A4"),
                                 dye = c("A001", "A001", "A001", "A004"),
                                 dye_conc = c(1, 2, 3, 4)))
    
    filter_list <- filterlayoutServer("filter_layout", layout_ex, text_name = "Plot")
    
    output$selection_external <- renderPrint(filter_list())
    
}

shinyApp(ui = ui, server = server)
