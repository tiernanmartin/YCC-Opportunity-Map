library(shiny)
library(shinyjs)

ui <- fluidPage(
  useShinyjs(), 
  
  # Application title
  titlePanel("Hello Shiny!"),
  
  sidebarLayout(
    
    # Sidebar with a slider input
    sidebarPanel(
     uiOutput("cars"),
     uiOutput("pressure"),
     uiOutput("rock") 
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      verbatimTextOutput("current"),
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
  
  onclick("rock",{{
    rx$reactInd <- 3

  }})


  
  # onclick("faithful",{{
  #   rx$reactInd <- 3 
  #   
  # }})
  # 
  # onclick("mtcars",{{
  #   rx$reactInd <- 3 
  #   
  # }})
  
  rx_data <- reactive({
    list(cars, pressure, rock)[[rx$reactInd]]
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
      
      output$rock <- renderUI({
        radioButtons("rock",
                 label = 'rock',
                 choices = 1:5,
                 selected = character(0))
      })
    }
    
    
    })
  

  
  output$current <- renderText({rx$reactInd})
  
  output$glimpse <- renderPrint({
    
    req(rx$reactInd > 0 )
    
    rx_data() %>% glimpse()
    
    
  })
  
})

shinyApp(ui, server)
