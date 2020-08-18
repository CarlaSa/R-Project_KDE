library(KDE)

ui <- fluidPage(
    titlePanel('Kernel Estimator of Density'),
    sidebarLayout(
        sidebarPanel(
            sliderInput('plot_range', 'plot range', min = -10, max = 10, value = c(-3, 3), step = 0.25)
        ),
        mainPanel(
            plotOutput('plot')
        )
    ),
    fluidRow(
        column(3,
               h4('true density function'),
                'Your function expression:',
                textInput('f', 'f <- function(x)', value = 'abs(x)/4 * (abs(x) <= 2)')
               ),
        column(3,
                h4('sampling'),
                numericInput('n_obs', 'number of observations', value = '1000'),
                tabsetPanel(type = 'tabs',
                            tabPanel('rejection sampling',
                                     numericInput('n_iter', 'average number of iterations per sample',
                                                  value = 10
                                                 ),
                                     selectInput('helper', 'helper distribution', helpers)
                                    ),
                            tabPanel('read/save data',
                                     fileInput("rds_file", "Choose RDS File",
                                        accept = c(
                                          "application/gzip",
                                          ".RDS", ".rds")
                                        ),
                                     downloadButton('rds_download', 'Download data')
                                    )),
                actionButton('resample', 'resample (not working yet)')
              ),
        column(3,
                h4('kernel'),
                selectInput('kernel', 'kernel', names(kernels))
              ),
        column(3,
                h4('bandwidth selection'),
                sliderInput('h', 'fixed bandwidth', min = 1e-10, max = 1e0, value = 0.5),
                checkboxInput('extra_fixed', 'Display an extra KDE using a manually controlled fixed bandwidth'),
                selectInput('bandwidth_selection_method', 'bandwidth selection method', bandwidth_selection_criteria()),
              )
    )
)

server <- function(input, output) {
    notification_id <- NULL
    data <- NULL
    data_file_mode <- FALSE
    # workaround to make the 'resample' button working:
    # we observe this 'state' boolean and resample when it is TRUE
    # (reset it to FALSE when done)
    resample_waiting <- FALSE
    # of not NULL, this is the bandwidth for the kde plotted in blue colour:
    # it can be set by the value computed by some bandwidth selection algorithm
    computed_bandwidth <- NULL
    
    # parsing the true function
    .get_f_silently <- reactive({
        tryCatch({
                function(x) eval(rlang::parse_expr(input$f))
            },
                 error = function(msg) msg)
    })
    .get_f <- function() {
        f <- .get_f_silently()
            if (!is.null(notification_id))
                removeNotification(notification_id)
        if(!is.function(f)) {
            notification_id <<- showNotification(paste(f), duration = 0)
            return(FALSE)
        }
        f
    }
    
    # sampling
    .get_data <- reactive({
        if(resample_waiting ||
           is.null(data) || length(data) != as.integer(input$n_obs) ||
           data_file_mode != !is.null(input$rds_file)) {
            data_file_mode <- !is.null(input$rds_file)
            resample_waiting <- FALSE;
            .resample()
        }
        data
    })
    .resample <- function() {
        if(is.null(input$rds_file))
            data <<- rejection_sample(as.integer(input$n_obs), .get_f_silently(),
                                      #helper_distribution = input$helper$distribution,
                                      #helper_density = input$helper$density,
                                      n_iter = as.integer(input$n_iter)
                                     )
        else {
            data <<- readRDS(input$rds_file$datapath)
        }
    }
    
    .get_fixed_kde <- reactive({
        get_kde(as.double(input$h), 
                kernels[[input$kernel]], 
                .get_data())
    })
    
    # data upload mechanism
    #output
    
    # download button
    output$rds_download <- downloadHandler(
        filename = function() {
            paste('data', ".rds", sep = "")
        },
        content = function(file) {
            saveRDS(.get_data(), file)
        }
    )
    
    # resample button
    observeEvent(input$resample, {
        resample_waiting <<- TRUE
        .get_data()
    })
    
    # extra fixed mode:
    # display a separate KDE with fixed bandwidth
    .is_fixed_mode <- reactive({
        input$extra_fixed
    })
    
    .get_bandwidth <- reactive({
        if(!is.null(computed_bandwidth))
            computed_bandwidth
        else
            .get_fixed_bandwidth()
    })
    .get_fixed_bandwidth <- reactive({
        input$h
    })
    
    output$plot <- renderPlot({
        hist(.get_data(), breaks = 1000, freq = FALSE, xlim = input$plot_range, border = 'light grey')
        f1 <- .get_f()
        curve(f1, add = TRUE, col = 'green')
        kde_fixed <- .get_fixed_kde()
        if(.is_fixed_mode()) curve(kde_fixed, add = TRUE, col = 'red')
        legend('topleft',
               c('number of data samples (scaled)',
                 'true function',
                 paste('kernel density estimator with bandwidth', .get_bandwidth()),
                 if(.is_fixed_mode()) paste('kernel density estimator with (fixed) bandwidth', .get_fixed_bandwidth())
                ),
              col = c('light grey', 'green', 'blue', 'red'),
              lwd = c(3, 1, 1, 1))
    })
}

shinyApp(ui, server)
