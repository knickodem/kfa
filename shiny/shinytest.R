models <- list(kfa = example)
input <- list(report.title = "whatever",
              report.format = "html_document",
              index = "default",
              load.flag = .30,
              cor.flag = .90,
              rel.flag = .60,
              digits = 2)
width <- 6.5
word.template <- system.file("rmd", "kfa_word_template.docx", package = "kfa")
file <- getwd()

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
                                        reference_docx = word.template),
                  envir = new.env(parent = globalenv()))
