# SETUP ----
library(tidyverse)
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
  data <- 
    list("cars", "pressure","faithful") %>% 
    {tibble(name = map_chr(., 1),
            df = map(.,get))} %>% 
    rownames_to_column("idx")
  
  rv <- reactiveValues(data = data)
  
  onclick("cars",{{
    rv$data <- filter(data, name %in% "cars")

  }})

  onclick("pressure",{{
     rv$data <- filter(data, name %in% "pressure")

  }})
  
  onclick("faithful",{{
     rv$data <- filter(data, name %in% "faithful")

  }})
  

  observe({ 
    
    
    if(rv$data$idx != 1){
      
      output$cars <- renderUI({
        radioButtons("cars",
                     label = 'cars:',
                     choices = 1:6,
                     selected = character(0))
      })
    }
    
    if(rv$data$name != 2){
      
      output$pressure <- renderUI({
        radioButtons("pressure",
                     label = 'pressure:',
                     choices = 1:6,
                     selected = character(0))
      })
    }
    
    if(rv$data$name != 3){
      
      output$faithful <- renderUI({
        radioButtons("faithful",
                     label = 'faithful:',
                     choices = 1:6,
                     selected = character(0))
      })
    } 
  })
  
  
  output$current <- renderText({
    
    req(nrow(rv$data) == 1)
    rv$data$name
    
  })
  
  output$current_row <- renderText({
    
    req(nrow(rv$data) == 1)
    
    input[[rv$data$name]]
  })
  
  output$row <- renderPrint({
    req(nrow(rv$data) == 1)
    
    df <- rv$data$df %>% pluck(1)
    
    df[input[[rv$data$name]],]
    
    
  })
  
  output$summary <- renderPrint({
    req(nrow(rv$data) == 1) 
    
    rv$data$df %>% pluck(1) %>% summary
  })
  
}

# APP ----

shinyApp(ui, server)
