library(tidyverse)
library(shiny)
library(shinyjs)
library(ggrepel)

linked_radio_ui <- function(id){
  
  ns <- NS(id)
  
  uiOutput(ns("radio_btn"))
}


linked_radio <- function(input, output, session, rx, reactInd_value, ...){
  ns <- session$ns
  
  onclick(ns("radio_btn"),{{
    rx()$reactInd <- reactInd_value

  }})
  
  if(rx()$reactInd != reactInd_value){ 
    
      output$radio_btn <- renderUI({
        
        ns <- session$ns
        
        radioButtons(ns("radio_btn"),
                 label = label,
                 choices = 1:6,
                 selected = character(0))
      })
    }
  
}

scatter_plot <- function(rx_reactInd, rx_df, rx_df_head, rx_df_row, rx_x, rx_y){
  req(rx_reactInd > 0)

    rx_df %>% ggplot() + 
      geom_point(aes(rx_x, rx_y), alpha = .1, size = 3) +
      geom_point(data = rx_data_head, aes(rx_x, rx_y), size = 3) + 
      geom_text_repel(data = rx_data_head, mapping = aes(rx_x,rx_y), label = 1:nrow(rx_data_head)) +
      geom_point(data = rx_df_row, aes(rx_x, rx_y), color = "red", size = 3)
}

# linked_radio <- function(input, output, session, reactInd, df){
#   
# }

ui <- fluidPage(
  useShinyjs(), 
  
  # Application title
  titlePanel("Check it out... linked radio buttons!"),
  
  sidebarLayout(
    
    # Sidebar with a slider input
    sidebarPanel(
      fluidRow(
        column(4,
               uiOutput("cars")
               ),
        column(4,
               uiOutput("pressure")
               ),
        column(4,
               uiOutput("faithful")
               ),
        column(4,
               linked_radio_ui("faithful2")
               )
        )
        
      ),
    
    # Show a plot of the generated distribution
    mainPanel(
      "Current dataset: ",textOutput("current",inline = TRUE), 
     plotOutput("plot")
    )
  )
)

server <- shinyServer(function(input, output, session) {
  
  rx <- reactiveValues(reactInd = 0)
  
  callModule(module = linked_radio,id = "faithful2", rx = reactive({rx}), reactInd_value = 4, label = "faithful2")
  
  
  # Observers (which input was most recently changed)
  
  onclick("cars",{{
    rx$reactInd <- 1

  }})

  onclick("pressure",{{
    rx$reactInd <- 2

  }})
  
  onclick("faithful",{{
    rx$reactInd <- 3

  }})

  
  rx_data <- reactive({
    df <- list(cars, pressure, faithful)[[rx$reactInd]]
    colnames(df) <- c("x","y")
    df
    
  })
  
  rx_data_head <- reactive({head(rx_data())})
  
  rx_row <- reactive({
    switch(rx$reactInd,
           "1" = rx_data()[input$cars,],
           "2" = rx_data()[input$pressure,],
           "3" = rx_data()[input$faithful,])
    
  })
  
  
# NEXT STEP: figure out how to figure out which radio option is selected
  
  observe({ 
    
    if(rx$reactInd != 1){
      
      output$cars <- renderUI({
        radioButtons("cars",
                 label = 'cars',
                 choices = 1:6,
                 selected = character(0))
      })
    }
    
    if(rx$reactInd != 2){
      
      output$pressure <- renderUI({
        radioButtons("pressure",
                 label = 'pressure',
                 choices = 1:6,
                 selected = character(0))
      })
    }
     
    if(rx$reactInd !=3){
      
      output$faithful <- renderUI({
        radioButtons("faithful",
                 label = 'faithful',
                 choices = 1:6,
                 selected = character(0))
      })
    }
    
    
    })

  
  output$current <- renderText({
    switch(rx$reactInd,
           "1" = "cars",
           "2" = "pressure",
           "3" = "faithful")
    
  })
  
  
   output$plot <- renderPlot({

    req(rx$reactInd > 0)

    rx_data() %>% ggplot() + 
      geom_point(aes(x,y), alpha = .1, size = 3) +
      geom_point(data = rx_data_head(), aes(x,y), size = 3) + 
      geom_text_repel(data = rx_data_head(), mapping = aes(x,y), label = 1:nrow(rx_data_head())) +
      geom_point(data = rx_row(), aes(x,y), color = "red", size = 3)


  })
  
})

shinyApp(ui, server)
