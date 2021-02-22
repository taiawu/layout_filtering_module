# library(tidyverse)
# library(shinyWidgets) # has pickerInput
# library(glue) # required for custom names within the selectors


## helper functions for the modules
contract_layout <- function(layout) {
  layout %>%
    mutate(across(everything(), as.character)) %>%
    mutate(well_id = well) %>%
    pivot_longer(-well_id, names_to = "variable", values_to = "value") %>%
    unite(picker_val, c(value, variable), sep  = "<LAYOUT_GROUP>", remove = FALSE)
}

expand_layout <- function(layout_cont) {
  layout_cont %>% 
    select(-picker_val) %>%
    pivot_wider(id_cols = well_id, names_from = variable, values_from = value) %>%
    select(-well_id)
  
}


layout_to_picker_list <- function(layout_cont) {
  
  layout_exp <- expand_layout(layout_cont)
  
  layout_names <- layout_exp %>%
    as.list() %>%
    lapply(unique)
  
  layout_vals <- map2(layout_names, names(layout_names), paste, sep = "<LAYOUT_GROUP>")
  
  layout_list <- map2(layout_vals, layout_names, set_names)  %>%
    .[order(sapply(., length))]
  
}

picked_to_layout <- function(layout_raw, picked_vec) {
  
  layout <- layout_raw %>% 
            mutate(across(everything(), as.character)) 
  
  tryCatch({ 
    
    layout_c_picked <- contract_layout(layout) %>%
      filter(picker_val %in% picked_vec) %>%
      expand_layout()
    
    layout %>%
      right_join(., layout_c_picked) %>%
      drop_na() %>%
      distinct() 
    
  }, error = function(e) { 
    layout_c_picked <- contract_layout(layout) %>%
      expand_layout() %>%
      mutate(across(everything(), ~NA ))

     return(layout[1,] %>% mutate(across(where(is.character), ~NA_character_)) )
    # return(layout[1,] %>% mutate(across(where(is.character), ~NA_character_)) )
    # return(
    #   layout %>%
    #     right_join(., layout_c_picked  , by = character()) %>%
    #     drop_na() %>%
    #     distinct() 
    # )
  })
  
}

# modules
##### working on the modules
filterlayoutUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("picker")),
    verbatimTextOutput(ns("selection"))
  )
}

# should return something that can be used to filter the layout outside of the module
filterlayoutServer <- function(id, layout, text_name) {
  
  stopifnot(is.reactive(layout)) # layout should be reactive
  
  moduleServer(id, function(input, output, session) {
    lower_case_name <- tolower(text_name)
    
    observeEvent(layout(), {
      
      layout_cont <- reactive(contract_layout(layout()))
      filtered_layout <- reactive(layout())
      picker_list_raw <-  reactive(layout_to_picker_list(layout_cont()))
      
      output$picker <- renderUI({ # called in UI module
        
        pickerInput(
          # THANK YOU for renderUI in modules https://gist.github.com/tbadams45/38f1f56e0f2d7ced3507ef11b0a2fdce 
          inputId =  session$ns("take_these"), 
          choices = picker_list_raw(),
          selected = picker_list_raw(),
          multiple = TRUE,
          options = pickerOptions(
            actionsBox = TRUE,
            selectAllText = glue("{text_name} all"),
            deselectAllText = glue("{text_name} none"),
            title = glue("Select data to {lower_case_name}"),
            tickIcon = "glyphicon-eye-open",
            selectedTextFormat= "count",
            countSelectedText = "{0} selections"
          )
        )
      })
      
      output$selection <- renderPrint({picked_to_layout(layout(), input$take_these)})
    })
    
    reactive(picked_to_layout(layout(), input$take_these))
  })
}