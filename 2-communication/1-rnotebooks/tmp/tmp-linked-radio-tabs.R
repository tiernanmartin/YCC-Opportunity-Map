library(shiny)
library(shinyjs)
library(ggvis)


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
               )
        )
        
      ),
    
    # Show a plot of the generated distribution
    mainPanel(
      "Current dataset: ",textOutput("current",inline = TRUE),
     verbatimTextOutput("glimpse")
    )
  )
)



server <- shinyServer(function(input, output, session) {
  
  rx <- reactiveValues(reactInd = 0)
  
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
  
  observe({ 
    
    if(rx$reactInd != 1){
      
      output$cars <- renderUI({
        radioButtons("cars",
                 label = 'cars',
                 choices = 1:5,
                 selected = character(0))
      })
    }
    
    if(rx$reactInd != 2){
      
      output$pressure <- renderUI({
        radioButtons("pressure",
                 label = 'pressure',
                 choices = 1:5,
                 selected = character(0))
      })
    }
     
    if(rx$reactInd !=3){
      
      output$faithful <- renderUI({
        radioButtons("faithful",
                 label = 'faithful',
                 choices = 1:5,
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
  
  # rx_data() %>% ggvis(~x, ~y) %>% layer_points() %>% bind_shiny("plot")
  
  output$glimpse <- renderPrint({

    req(rx$reactInd > 0)

    rx_data() %>% glimpse()


  })
  
})

shinyApp(ui, server)
