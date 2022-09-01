#####################################
#                                   #
#     Shiny app for kfa package     #
#    Kyle Nickodem & Peter Halpin   #
#                                   #
#####################################

# Load packages
if(!require(shiny)){install.packages('shiny')}
if(!require(kfa)){install.packages('kfa')}
if(!require(kfa)){install.packages('rhandsontable')}
library(shiny); library(kfa); library(rhandsontable)

# ---- Define UI --------------------------------------

ui <- fluidPage(

  #### Set theme colors ####
  tags$head(
    tags$style(HTML('#click1to2{background-color:#428BCA; color: white}',
                    '#click2to3{background-color:#428BCA; color: white}',
                    '#click2to1{background-color:#428BCA; color: white}',
                    '#click3to2{background-color:#428BCA; color: white}',
                    '#run{background-color:#428BCA; color: white}',
                    '#report{background-color:#428BCA; color: white}',
                    '#findk{background-color:#428BCA; color: white}'
    ))),

  ####  Set app header ####
  fluidRow(
    column(width = 12,
           HTML(paste(tags$strong("kfa", style = "font-size:40px;"),
                      "v0.2.0")),
           h4("K-Fold Cross-Validation for Factor Analysis"),
           HTML(paste(
             "If you find this app useful, please cite: Nickodem, K., & ",
             "Halpin, P. (2022). kfa: K-Fold Cross-Validation for Factor Analysis ",
             "[R package v.0.2.0]. https://CRAN.R-project.org/package=kfa"),
             sep = ""),
           style = "padding-bottom: 10px;"
    )
  ),

  #### Set sidebar with instructions ####

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      h4("How to Use The kfa App"),

      # Step 1
      tags$div(
        HTML(paste(
          tags$b('Step 1: Import Data.'),
          "Import a .rds, .csv, or .tsv file containing the variables to be factor analyzed ",
          "with observations in rows and variables in columns. ",
          "Delete unwanted columns by right-clicking on column and selecting 'Remove column'.",
          "All cell values must be numeric with",tags$code("NA"),"indicating missing values."
        )
        ),
        style = "padding-bottom: 10px;"),
      tags$p("Click 'Confirm Data' button to finalize the data to be analyzed and continue to '2. Analysis' tab."
      ),
      # Step 2
      tags$div(
        HTML(paste(
          tags$b('Step 2: Analysis.'),
          "Set kfa analysis options. Click 'Run kfa' button to run the analysis.",
          "A summary will appear when analysis is complete.",
          "Click 'To Step 3' button to continue to '3. Reporting' tab.")
        ),
      ),
      # Step 3
      tags$div(
        HTML(paste(
          tags$b('Step 3: Reporting.'),
          "Set reporting options. Click 'Generate Report' button to summarize findings and download report.")
        ),
      )
    ),

    # specify input options and output
    mainPanel(
      tabsetPanel(id = "thetabs",

                  # Step 1. Data ------------

                  tabPanel("1. Data",  value = "tab1",
                           ## Input
                           wellPanel(
                             # Field for specifying filepath or browse files
                             fileInput(inputId = "data", label = "Data File to Analyze",
                                       multiple = FALSE), accept = c(".csv", ".tsv", ".rds"),
                             # QUESTION: can we make the example dataset available?

                             ## Output - Display interactive handsontable
                             rHandsontableOutput("dataset"),
                           ),
                           helpText(paste("If needed, right-click table to display options for adding/removing rows and columns.",
                                          "Use Shift and Cmd/Ctrl to help highlight groups of columns/rows.",
                                          "Data will temporarily grey-out after each change.")),

                           # Button to move to next tab
                           actionButton(
                             inputId = "click1to2",
                             label = "Confirm Data"),

                           # Display warning on data detection
                           htmlOutput("step1_data_warning")
                  ),
                  # Step 2. Analysis ------------

                  tabPanel("2. Analysis",  value = "tab2",
                           ## Input
                           column(12,
                                  wellPanel(
                                    helpText(tags$h3("Design Options")),
                                    fluidRow(
                                      column(4, numericInput(inputId = "k", label = "Number of folds", value = 5, min = 2, max = 10)),
                                      column(4, numericInput(inputId = "m", label = "Maximum number of factors", value = NULL)),
                                      column(4, numericInput(inputId = "seed", label = "Set seed for splitting data", value = 101))
                                    ),
                                    helpText("See 'Power for K' tab for help determining the number of folds"),
                                    helpText(tags$h3("Data and Estimation Options")),
                                    fluidRow(
                                      column(4, textInput(inputId = "ordered", label = "List ordinal variables (or check box for all)")),
                                      column(4, textInput(inputId = "estimator", label = "Estimator", value = "MLMVS")),
                                      column(4, selectInput(inputId = "missing", label = "Handle missing data", choices = c("listwise", "pairwise",
                                                                                                                            "fiml", "fiml.x", "robust.two.stage")))
                                    ),
                                    fluidRow(
                                      column(4, checkboxInput(inputId = "orderedall", label = "Treat all variables as ordinal"))
                                    ),
                                    helpText(tags$h3("Factor Analysis Options")),
                                    fluidRow(
                                      column(4, textInput(inputId = "rotation", label = "Rotation method for EFA models", value = "oblimin")),
                                      column(4, radioButtons(inputId = "simple", label = "Allow cross-loadings in CFA models?",
                                                             choices = list("Yes" = 1, "No" = 2), selected = 2, inline = T)),
                                      column(4, numericInput(inputId = "min.loading", label = "Minimum factor loading size to retain variable on factor",
                                                             value = NA, min = 0, max = 1))
                                    )
                                    # add custom cfa option
                                  )),
                           # Add navigation buttons
                           actionButton(inputId = "click2to1",
                                        label = "Back To Step 1"),
                           actionButton(inputId = "run",
                                        label = "Run kfa"),
                           actionButton(inputId = "click2to3",
                                        label = "To Step 3"),

                           ## Output
                           # htmlOutput("vnames")
                           htmlOutput("kfaout")
                  ),

                  # Step 3. Reporting  ------------

                  tabPanel("3. Reporting",  value = "tab3",
                           ## Input
                           column(12,
                                  wellPanel(
                                    helpText(tags$h3("Output Options")),
                                    fluidRow(
                                      column(5, textInput(inputId = "file.name", label = "Name of created file")),
                                      column(5, textInput(inputId = "report.title", label = "Title of the report")),
                                      column(2, selectInput(inputId = "report.format", label = "Format of the report",
                                                            selected = "HTML", choices = c("HTML" = "html_document",
                                                                                           "Word" = "word_document",
                                                                                           "PDF" = "pdf_document")))
                                    ),
                                    helpText(tags$h3("Report Display")),
                                    fluidRow(
                                      column(5, textInput(inputId = "index", label = HTML("Indices to summarize model fit<br/>(List indices separated by a comma)"),
                                                          value = "default")),
                                      column(3, numericInput(inputId = "digits", label = "Number of decimal places to display",
                                                                         value = 2, min = 0, max = 6))
                                    ),
                                    helpText(tags$h3("Flag Model Issues")),
                                    fluidRow(
                                      column(3, numericInput(inputId = "load.flag", label = "Flag factor loadings below:", value = .30, min = 0, max = 1)),
                                      column(3, offset = 1, numericInput(inputId = "cor.flag", label = "Flag factor correlations above:", value = .90, min = 0, max = 1)),
                                      column(3, offset = 1, numericInput(inputId = "rel.flag", label = "Flag factor reliability below:", value = .60, min = 0, max = 1))
                                    ))),

                           # Add navigation buttons
                           actionButton(inputId = "click3to2",
                                        label = "Back to Step 2"),
                           downloadButton(outputId = "report",
                                        label = "Generate Report")
                           # Output
                           # uiOutput("modelfit"),
                           # htmlOutput(outputId = "report")
                  ),

                  # Optional. Power for K  ------------

                  tabPanel("Power for K", value = "tab4",
                           ## Input
                           wellPanel(
                             fluidRow(
                               column(4, numericInput(inputId = "nk", label = "Number of observations", value = NULL)),
                               column(4, numericInput(inputId = "pk", label = "Number of variables to factor analyze", value = NULL)),
                               column(4, numericInput(inputId = "mk", label = "Maximum number of factors", value = NULL))
                             ),
                             fluidRow(
                               column(4, numericInput(inputId = "min_n", label = "Minimum sample size per fold", value = 200)),
                               column(4, numericInput(inputId = "rmsea0", label = "RMSEA under the null hypothesis",
                                                      value = .05, min = 0, max = 1)),
                               column(4, numericInput(inputId = "rmseaA", label = "RMSEA under the alternative hypothesis",
                                                      value = .08, min = 0, max = 1))
                             ),
                             # Button to run power analysis
                             actionButton(
                               inputId = "findk",
                               label = "Find K"),
                             # Output
                             htmlOutput("kfound")
                           )
                  ) # tabPanel
      ) # tabsetpanel
    ), #mainpanel
  ) #sidebarlayout
) #fluidpage


# ---- Define server logic --------------------------------------
server <- function(input, output) {

  # Step 1. Data ------------

  ## import data from path
  df <- reactive({
    req(input$data)
    ext <- tools::file_ext(input$data$name) # grabs file extension to be used in switch
    # Not sure if
    validate(need(ext %in% c("csv", "tsv", "rds"), "Please upload a .csv, .tsv, or .rds file"))

    switch(ext,
           csv = read.csv(input$data$datapath),
           tsv = read.table(input$data$datapath, sep = "\t", header = TRUE),
           rds = readRDS(input$data$datapath))
  })

  ## convert handsontable to r object
  usedata <- reactive({
    hot_to_r(input$dataset)
  })

  ## Render data table
  output$dataset <- renderRHandsontable({
    if(is.null(input$dataset)){
      thedata <- df() # from file path
    } else {
      thedata <- usedata() # after table has changed
    }
    rhandsontable(thedata, rowHeaders = NULL, stretchH = "all",
                  useTypes = FALSE, # columns can be edited when FALSE
                  height = 300)

    # # Disable row and column editing
    # hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
  })

  # observeEvent(input$dataset, {
  #   output$vnames <- renderText({
  #     paste0("<b>Variables for analysis: </b> <br>",
  #            paste(names(usedata()), collapse = ", "))
  #   })
  #
  # })



  observeEvent(input$click1to2, {
    # Test if data was imported
    test_data_import <- try(req(input$dataset), silent = T)

    if (inherits(test_data_import, "try-error")) {
      output$step1_data_warning <- renderText("No data detected. Did you import a .csv, .tsv, or .rds file?")
    } else {
      output$step1_data_warning <- renderText("")
      updateTabsetPanel(inputId = "thetabs", selected = "tab2")
    }

  })


  # Step 2. Analysis ------------

  ##### Navigation reactions ####

  observeEvent(input$click2to3, {
    ## NEED TO PUT IN CHECKS THAT BLANK FIELDS HAVE BEEN ENTERED
    updateTabsetPanel(inputId = "thetabs", selected = "tab3")
  })

  observeEvent(input$click2to1, {
    updateTabsetPanel(inputId = "thetabs", selected = "tab1")
  })

  ##### Analysis reactions ####

  # Radio button for simple structure
  simple <- reactive({
    if(input$simple == 1){simple <- FALSE} else {simple <- TRUE}
  })

  # checkbox for ordered argument
  ordered <- reactive({
    if(input$orderedall){
      ordered <- TRUE}
    else {
      ordered <- input$ordered
        }
    ordered
    })

  # object where we can save results from kfa() call to use in kfa_report()
  models <- reactiveValues(kfa = NULL)

  ## when 'Run kfa' button is clicked
  observeEvent(input$run, {
    ## NEED TO PUT IN CHECKS THAT BLANK FIELDS HAVE BEEN ENTERED

    # output$vnames <- renderText({
    #   paste0("<b>Variables for analysis: </b> <br>",
    #          paste(names(usedata()), collapse = ", "))
    # })

    datadf <- usedata() # convert handsontable to dataframe

    ## run kfa and save models to reactiveValues object
    models$kfa <- kfa(data = datadf, variables = names(datadf), k = input$k, m = input$m, seed = input$seed,
                  # custom.cfas = NULL, cores = NULL, # need to create inputs for these
                  rotation = input$rotation, simple = simple(), min.loading = input$min.loading,
                  ordered = ordered(), estimator = input$estimator, missing = input$missing)

    ## gather analysis information to output
    mnames <- models$kfa$model.names # model names
    fac.max <- max(as.numeric(substring(mnames[grepl("-factor", mnames)], 1, 1)))  # kfa naming convention "#-factor"; custom functions are assumed to have different convention
    vnames <- dimnames(lavaan::lavInspect(models$kfa$cfas[[1]][[1]], "sampstat")$cov)[[1]] # variable names
    nobs <- sum(unlist(lapply(models$kfa$cfas, function(x) lavaan::lavInspect(x[[1]], "nobs"))))

    ## formatting analysis info as lines of text; replicates Overview section of report
    output$kfaout <- renderText({
      paste0("<b>k</b>"," = ", length(models$kfa$cfas),
      "<br><b># of variables</b>", " = ", length(vnames),
      "<br><b># of observations</b>", " = ", nobs,
      "<br><b>Max # of factors allowed</b>",  " = ", length(models$kfa$efa.structures),
      "<br><b>Max # of factors extracted</b>",  " = ", fac.max)
    })

    # #### Model Fit ####
    # ## summarizing fit statistics by fold
    # kfits <- k_model_fit(models, index = input$index, by.fold = TRUE) # dataframe for each fold
    # fit.table <- agg_model_fit(kfits, index = "all", digits = 2)
    # # adjust model order to match model.names and other output
    # fit.table <- fit.table[order(factor(fit.table$model, levels = mnames)),]
    #
    # ftn <- names(fit.table)
    # dft <- ftn[grepl("df", ftn)] # naive or scaled?
    # ix <- unique(gsub("mean.|range.", "", ftn[!ftn %in% c("model", dft)]))
    # fit.map <- data.frame(col_keys = ftn,
    #                       top = c("model", dft, rep(ix, each = 2)),
    #                       bottom = c("model", dft, rep(c("mean", "range"), times = length(ix))))
    #
    # fit.flex <- flextable::flextable(fit.table)
    # fit.flex <- flextable::colformat_double(fit.flex, j = -c(1, 2), digits = inputs$digits)
    # fit.flex <- two_level_flex(fit.flex, mapping = fit.map, vert.cols = c("model", dft), border = officer::fp_border(width = 2))
    #
    # output$modelfit <- renderUI({
    #   hmtltools_value(fit.flex)
    # })
  })

  # Step 3. Reporting ------------

  observeEvent(input$click3to2, {
    updateTabsetPanel(inputId = "thetabs", selected = "tab2")
  })

  reportext <- reactive({
    req(input$report.format)
    re <- input$report.format
    validate(need(re %in% c("html_document", "word_document", "pdf_document"), "Option not available yet"))

    switch(re,
           html_document = ".html",
           word_document = ".docx",
           pdf_document = ".pdf")
  })

    output$report <- downloadHandler(
      filename = function(){
        paste0(input$file.name, reportext())
      },
      content = function(file){

        ## running report
        if(input$report.format == "word_document"){
          width <- 6.5
        } else {
          width <- NULL
        }

        # if(is.null(word.template)){
          word.template <- system.file("rmd", "kfa_word_template.docx", package = "kfa")
        # }

          ## inputs to pass onto rmarkdown
          params <- list(models = models$kfa,
                         report.title = input$report.title,
                         index = input$index,
                         load.flag = input$load.flag,
                         cor.flag = input$cor.flag,
                         rel.flag = input$rel.flag,
                         digits = input$digits,
                         width = width)

        template <- system.file("rmd", "kfa-report-shiny.Rmd", package = "kfa")
        rmarkdown::render(input = template,
                          params = params,
                          output_format = input$report.format,
                          output_file = file,
                          output_options = list(toc = TRUE, toc_depth = 2,
                                                always_allow_html = TRUE,
                                                reference_docx = word.template))
                          # envir = new.env(parent = globalenv()))

      }
    )

  # Power for K ------------

  observeEvent(input$findk, {
    kout <- find_k(n = input$nk, p = input$pk, m = input$mk,
                   min.n = input$min_n, rmsea0 = input$rmsea0, rmseaA = input$rmseaA)
    output$kfound <- renderText({
      paste0("k = ", kout[[1]],
             "<br>yields test sets of ", kout[[2]],
             " observations for running the CFA models within each fold")
    })
  })

}

# Run the application
shinyApp(ui = ui, server = server)


