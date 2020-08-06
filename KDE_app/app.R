
library(shiny)

#TODO: Import
######### 


get_kde <- function(n, h, Kernel, data){
    #'
    #'
    # f <-function(x){
    #   1/(n*h) * sum(
    #     Kernel((data - x)/h)
    #   )
    # }
    ## VECTORIZED
    f <- function(x){
        temp <- outer(data, x, `-`)
        temp <- Kernel(temp/h)
        temp <- apply(temp, 2, sum)
        1/(n*h) * temp
    }
    return(f)
}
e_kernel <- function(x){
    0.75 * (x <1 & x > -1) * (1 - x^2)
}
############


# Define UI
ui <- fluidPage(
    theme = shinythemes::shinytheme("darkly"),
    # Title
    titlePanel("Kernel Estimator of Density"),
    # App ist zweigeteilt
    sidebarLayout(
        #Plot
        mainPanel(
            plotOutput("plot")
        ),
        #Variablen
        sidebarPanel(
            radioButtons("bw-selector", "Bandwidth Selector", choices=c("CV", "PCO", "GL")),
            sliderInput("n_samples", "Number of Samples", min = 1000, max = 10000, value = 5000, step = 100),
            sliderInput("h", "Bandwidth", min = 0, max = 5, step = 0.05, value = 1)
        )
        
    )
)

#define server
server <- function(input, output) {
    data <- reactive(
        rnorm(input$n_samples)
    )
    output$plot <- renderPlot({
        hist(data(), freq = F, breaks = 1000, xlim = c(-5,5), ylim = c(0, 0.7), border = "light grey")
        curve(dnorm, add = T, col = "red")
        kde <- get_kde(Kernel = e_kernel, n = input$n_samples, h = input$h, data = data())
        curve(kde, add = T, col = "blue")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
