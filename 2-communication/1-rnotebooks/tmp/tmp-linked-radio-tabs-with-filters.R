library(shiny)

radioSubgroup <- function(inputId, id, label, choices, inline = FALSE) {
  values <- paste0(id, "-", choices)
  choices <- setNames(values, choices)

  rb <- radioButtons(inputId, label, choices, selected = character(0), inline = inline)
  rb$children
}

radioGroupContainer <- function(inputId, ...) {
  class <- "form-group shiny-input-radiogroup shiny-input-container"
  div(id = inputId, class = class, ...)
}

ui <- fluidPage(
  titlePanel("Example: linked radio buttons"),

  sidebarLayout(
    sidebarPanel(
      sliderInput("choices", "Choices:", min = 1, max = 10, step = 1, value = 5),
      uiOutput("radio_group")
    ),
    
    mainPanel(
      fluidRow(
        column(4,
               strong("Current dataset: "), textOutput("current", inline = TRUE),
               br(),
               strong("Selected row:"), textOutput("selectedRow", inline = TRUE),
               verbatimTextOutput("row"),
               strong("Summary:"),
               verbatimTextOutput("summary")
        )
      )
    )
  )
)

server <- function(input, output, session) {

  output$radio_group <- renderUI({
    radioGroupContainer("selectedRow",
        fluidRow(
         
          column(4, radioSubgroup("selectedRow", "cars", label = "cars:", choices = 1:input$choices)),
          column(4, radioSubgroup("selectedRow", "pressure", label = "pressure:", choices = 1:input$choices)),
          column(4, radioSubgroup("selectedRow", "faithful", label = "faithful:", choices = 1:input$choices))
        )
      )
  })
  
  
  selectedRow <- reactive({
    req(input$selectedRow)
    parts <- unlist(strsplit(input$selectedRow, "-"))
    list(id = parts[1], value = parts[2])
  })

  currentDataset <- reactive({
    getExportedValue("datasets", selectedRow()$id)
  })

  output$current <- renderText({
    selectedRow()$id
  })

  output$selectedRow <- renderText({
    selectedRow()$value
  })

  output$row <- renderPrint({
    currentDataset()[selectedRow()$value, ]
  })

  output$summary <- renderPrint({
    summary(currentDataset())
  })
}

shinyApp(ui, server)