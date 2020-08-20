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
                selectInput('pdf_factory', 'Probability distribution', names(pdf_factories), selected = 'normal'),
                'Your function expression:',
                textInput('f', 'f <- function(x)', value = '1/(1 * sqrt(2 * pi)) * exp(-1/2 * ((x - 0)/1)^2)')
               ),
        column(3,
                h4('sampling'),
                numericInput('n_obs', 'number of observations', value = '1000'),
                tabsetPanel(type = 'tabs',
                            tabPanel('rejection sampling',
                                     numericInput('n_iter', 'average number of iterations per sample',
                                                  value = 10
                                                 ),
                                     selectInput('helper', 'helper distribution', names(helpers), selected = 'normal')
                                    ),
                            tabPanel('read/save data',
                                     fileInput("rds_file", "Choose RDS File",
                                        accept = c(
                                          "application/gzip",
                                          ".RDS", ".rds")
                                        ),
                                     downloadButton('rds_download', 'Download data')
                                    )),
              ),
        column(3,
                h4('kernel'),
                selectInput('kernel', 'kernel', names(kernels))
              ),
        column(3,
                h4('bandwidth selection'),
                sliderInput('h', 'fixed bandwidth', min = 0.01, max = 1, value = 0.5),
                checkboxInput('extra_fixed', 'Display an extra KDE using a manually controlled fixed bandwidth'),
                fluidRow(
                    column(9,
                        selectInput('bandwidth_selection_method', 'bandwidth selection method', names(KDE:::bandwidth_selection_criteria()))
                    ),
                    column(3,
                        actionButton('run_bws', 'Run')
                    ),
                    column(4,
                        'Result:'
                    ),
                    column(5,
                        textOutput('h_bws')
                    ),
                    column(3,
                        actionButton('use_h_bws', 'Use')
                    )
                )

              )
    )
)

server <- function(input, output, session) {
    notification_id <- NULL
    data <- NULL
    data_file_mode <- FALSE

    # if not NULL, this is the bandwidth for the kde plotted in blue colour:
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
        if(is.null(data) || length(data) != as.integer(input$n_obs) ||
           data_file_mode != !is.null(input$rds_file)) {
            data_file_mode <- !is.null(input$rds_file)
            .resample()
        }
        data
    })
    .resample <- function() {
        if(is.null(input$rds_file))
            withProgress(message = 'generating random samples', value = 0.1, {
                data <<- rejection_sample(as.integer(input$n_obs), .get_f_silently(),
                                          helper = helpers[[input$helper]],
                                          n_iter = as.integer(input$n_iter)
                                         )
                incProgress(0.9)
            })
        else {
            data <<- readRDS(input$rds_file$datapath)
        }
    }
       
    # download button
    output$rds_download <- downloadHandler(
        filename = function() {
            paste('data', ".rds", sep = "")
        },
        content = function(file) {
            saveRDS(.get_data(), file)
        }
    )
    
    observeEvent(input$pdf_factory, {
        pdf_factory <- pdf_factories[[input$pdf_factory]]
        if(identical(pdf_factory, pdf_factories$custom))
            f_text <- formals(pdf_factory)[[1]]
        else {
            pdf <- pdf_factory()
            env <- as.list(environment(pdf))
            f_text <- deparse(body(pdf))
            f_text <- stringr::str_c(f_text, collapse = ';')
            for(n in names(env)) {
                pat <- stringr::str_glue('(?<!\\w){n}(?!\\w)')
                f_text <- stringr::str_replace_all(f_text, pat, as.character(env[[n]]))
            }
        }
        
        updateTextInput(session, 'f', value = f_text)
    })
    
    # compute bandwidth
    observeEvent(input$run_bws, {
        withProgress(message = 'computing the bandwidth', value = 0.1, {
            computed_bandwidth <<- bandwidth_selection(input$bandwidth_selection_method, kernels[[input$kernel]], .get_data(), maxEval = 1e3, set_up_cluster = FALSE)
            incProgress(0.85)
            output$h_bws <- renderText(computed_bandwidth)
            incProgress(0.05)
        })
    })

    observeEvent(input$use_h_bws, {
        updateSliderInput(session, 'h', value = computed_bandwidth)
    })

    # extra fixed mode:
    # display a separate KDE with fixed bandwidth
    .is_fixed_mode <- reactive({
        input$extra_fixed
    })
    
    .get_bandwidth <- function(){
        if(!is.null(computed_bandwidth))
            computed_bandwidth
        else
            input$h
    }

    output$plot <- renderPlot({
        hist(.get_data(), breaks = 1000, freq = FALSE, xlim = input$plot_range, border = 'light grey')
        f1 <- .get_f()
        curve(f1, add = TRUE, col = 'green')
        kde <- get_kde(.get_bandwidth(), 
                kernels[[input$kernel]], 
                .get_data())
        curve(kde, add = TRUE, col = 'blue')
        if(.is_fixed_mode()) {
            kde_fixed <- get_kde(input$h, kernels[[input$kernel]], .get_data())
            curve(kde_fixed, add = TRUE, col = 'red')
        }
        legend('topleft',
               c('number of data samples (scaled)',
                 'true function',
                 paste('kernel density estimator with bandwidth', .get_bandwidth()),
                 if(.is_fixed_mode()) paste('kernel density estimator with (fixed) bandwidth', input$h)
                ),
              col = c('light grey', 'green', 'blue', 'red'),
              lwd = c(3, 1, 1, 1))
    })

}

shinyApp(ui, server)
