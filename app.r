library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Title the Shiny App
    titlePanel("BSGP 2024 LM Dashboard"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
            # Input: Select a file ----
            fileInput("file1", "Choose CSV File",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            
            # Horizontal line ----
            tags$hr(),
            
            # Input: Checkbox if file has header ----
            checkboxInput("header", "Header", TRUE),
            
            # Input: Select separator ----
            radioButtons("sep", "Separator",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = ","),
            
            # Input: Select quotes ----
            radioButtons("quote", "Quote",
                         choices = c(None = "",
                                     "Double Quote" = '"',
                                     "Single Quote" = "'"),
                         selected = '"'),
            
            # Horizontal line ----
            tags$hr(),
            
            # Input: Select number of rows to display ----
            radioButtons("disp", "Display",
                         choices = c(Head = "head",
                                     All = "all"),
                         selected = "head"),
            tags$hr(),
            actionButton("go", "Plot Linear Model")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("origPlot"),
           plotOutput("lmPlot"),
           tableOutput("contents"),
           verbatimTextOutput("values") # Allows for display of slope, intercept, and correlation coefficient
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    lmdata <- reactiveValues()
    
    dataInput <- reactive({
        req(input$file1)
        
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
        return(df)
    })

    observeEvent(input$go,{
        update_lm()
    })

    #Extract variables frrom dataset
    update_lm <- function(){
        model <- lm(y ~ x, data = dataInput())
        lm_summary <- summary(model)
        lmdata$model <- model
        lmdata$slope <- lm_summary$coefficients[2,1]
        lmdata$intercept <- lm_summary$coefficients[1,1]
        lmdata$r <- lm_summary$r.squared
    }
    
    #Display original plot with labels
    output$origPlot <- renderPlot({
        plot(dataInput()$x, dataInput()$y, xlab = 'X', ylab = 'Y')
    })

    #Display lm plot with labels
    output$lmPlot <- renderPlot({
        plot(dataInput()$x, dataInput()$y, xlab = 'X', ylab = 'Y')
        if (!is.null(lmdata$model)) {
            abline(lmdata$model, col = 'blue')
        }
    })

    output$contents <- renderTable({
        if(input$disp == "head") {
            return(head(dataInput()))
        } else {
            return(dataInput())
        }
    })

    #Display variables of interest
    output$values <- renderText({
        paste(
            "Slope:", round(lmdata$slope, 2), 
            "Intercept:", round(lmdata$intercept, 2), 
            "R-Squared:", round(lmdata$r, 2)
        )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)