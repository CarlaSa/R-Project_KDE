
library(shiny)
#library("kernels.R")

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

kernels <- c(
    gaussian = function(u) {
        1 / (sqrt(2*pi)) * exp(-u^2 / 2)
    },
    rectangular = function(u) {
        1/2 * (abs(u) <= 1)
    },
    triangular = function(u) {
        (1 - abs(u)) * (abs(u) <= 1)
    },
    epanechnikov = function(u) {
        3/4 * (1 - u^2) * (abs(u) <= 1)
    },
    biweight = function(u) {
        15/16 * (1 - u^2)^2 * (abs(u) <= 1)
    },
    silverman = function(u) {
        1/2 * exp(-abs(u)/sqrt(2)) *
            sin(abs(u)/sqrt(2) + pi/4)
    }
)
kernels$parabolic <- kernels$epanechnikov

distributions <- c(
    normal = rnorm,
    beta = rbeta
)
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
            selectInput(
                "distr", "Distribution", names(distributions)
            ),
            sliderInput("n_samples", "Number of Samples", min = 0, max = 5000, value = 5000, step = 100),
            
            radioButtons("bw-selector", "Bandwidth Selector", choices=c("CV", "PCO", "GL")),
            sliderInput("h", "Bandwidth", min = 0, max = 0.5, step = 0.005, value = 0.5),
            selectInput(
                "ker", "Kernel", names(kernels)
            ),
        )
        
    )
)

#define server
server <- function(input, output) {
    data <- reactive(
        rnorm(input$n_samples)
    )
    output$plot <- renderPlot({
        hist(data(), freq = F, breaks = 1000, xlim = c(-4,4), ylim = c(0, 0.7), border = "light grey")
        curve(dnorm, add = T, col = "red")
        kde <- get_kde(Kernel = kernels[[input$ker]], n = input$n_samples, h = input$h, data = data())
        curve(kde, add = T, col = "blue")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
