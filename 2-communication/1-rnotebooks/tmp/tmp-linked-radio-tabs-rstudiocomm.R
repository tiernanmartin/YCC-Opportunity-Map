# SETUP ----

library(shiny)
library(shinyjs)

# MODULES ----

# none yet

# UI ----
ui <- fluidPage(
  useShinyjs(), 
  
  # Application title
  titlePanel("Example: linked radio buttons"),
  
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
      fluidRow(
        column(4,
               strong("Current dataset: "),textOutput("current",inline = TRUE),
               br(),
               strong("Selected row:"), textOutput("current_row", inline = TRUE),
               verbatimTextOutput("row"),
               strong("Summary:"),
               verbatimTextOutput("summary")
        )

      )

    )
  )
)

# SERVER ----

server <- function(input, output, session) {
  
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
  

  observe({ 
    
    if(rx$reactInd != 1){
      
      output$cars <- renderUI({
        radioButtons("cars",
                 label = 'cars:',
                 choices = 1:6,
                 selected = character(0))
      })
    }
    
    if(rx$reactInd != 2){
      
      output$pressure <- renderUI({
        radioButtons("pressure",
                 label = 'pressure:',
                 choices = 1:6,
                 selected = character(0))
      })
    }
     
    if(rx$reactInd !=3){
      
      output$faithful <- renderUI({
        radioButtons("faithful",
                 label = 'faithful:',
                 choices = 1:6,
                 selected = character(0))
      })
    }
    
    
    })

  
  output$current <- renderText({
    
    req(rx$reactInd > 0)
    
    switch(rx$reactInd,
           "1" = "cars",
           "2" = "pressure",
           "3" = "faithful")
  })
  
  output$current_row <- renderText({
    
    req(rx$reactInd > 0)
    
    switch(rx$reactInd,
           "1" = input$cars,
           "2" = input$pressure,
           "3" = input$faithful)
  })
  
  output$row <- renderPrint({
    req(rx$reactInd > 0)
    switch(rx$reactInd,
           "1" = cars[input$cars,],
           "2" = pressure[input$pressure,],
           "3" = faithful[input$faithful,])
    
  })
  
  output$summary <- renderPrint({
    req(rx$reactInd > 0)
    df <- switch(rx$reactInd,
           "1" = cars,
           "2" = pressure,
           "3" = faithful)
    summary(df)
  })
  
}

# APP ----

shinyApp(ui, server)
