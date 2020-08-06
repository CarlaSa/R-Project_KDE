### should be imported instead###
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

kde <- get_kde(Kernel = e_kernel, n = 1000, h = 3, data = rnorm(10))

#######



ui <- fluidPage(
  theme = shinythemes::shinytheme("darkly"),
  titlePanel("Kernel Estimator of Density"),
  sidebarLayout(
    mainPanel(
      plotOutput("plot")
    ),
    sidebarPanel(
      #h3("Bandwidth Selector"),
      radioButtons("bw-selector", "Bandwidth Selector", choices=c("CV", "PCO", "GL")), 
      sliderInput("n_samples", "Number of Samples", min = 1000, max = 10000, value = 5000, step = 100),
      sliderInput("h", "Bandwidth", min = 0, max = 5, step = 0.05, value = 1)
    )
    
  )
)
server <- function(input, output) {
  data <- reactive(
    rnorm(input$n_samples)
  )
  # kde <- reactive(
  #   get_kde(Kernel = e_kernel, n = input$n_samples, h = input$h, data = data())
  # )
  output$plot <- renderPlot({
    hist(data(), freq = F, breaks = 1000, xlim = c(-5,5), ylim = c(0, 0.7), border = "light grey")
    curve(dnorm, add = T, col = "red")
    kde <- get_kde(Kernel = e_kernel, n = input$n_samples, h = input$h, data = data())
    curve(kde, add = T, col = "blue")
  })
}

shinyApp(ui, server)
