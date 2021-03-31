#' badzupagui
#' @export
badzupagui <- function(){
  shiny::shinyApp(
    ui = fluidPage(
      titlePanel("BAD-ZUPA"),
      br(),
      sidebarLayout(
        sidebarPanel(
          h3("Uploading Files"),
          tags$head(tags$style(
            ".progress-bar{background-color: black;}"
          )),
          hr(style = "border-top: 1px solid #000000;"),
          "Choose CSV File",
          fileInput(
            'file1',
            NULL,
            accept = c('text/csv',
                       'text/comma-separated-values,text/plain',
                       '.csv')
          ),

          tags$head(tags$style(
            HTML(
              ".btn {
              background-color:#400E82; color: white;font-weight: bold;}
              .btn:hover{
              #border-color: #dda0dd;
              background-color: #6E5484;color: white;
              }
              "
            )
            )),
          actionButton('action_plot', 'Plot density function'),

          br(),
          h3("Options"),
          hr(style = "border-top: 1px solid #000000;"),

          "Plot confidence interval?",
          selectInput(
            "CI",
            NULL,
            choices = list("Yes" = 1, "No" = 2),
            selected = 1
          ),
          "Plot rugs?",
          selectInput(
            "rug",
            NULL,
            choices = list("Yes" = 1, "No" = 2),
            selected = 1
          ),
          #"How many discretizate axis ?",
          #textInput("m", NULL, 500),
          fluidRow(
            column(6,
                   "Min of x axis:",
                   textInput("xlim_min", NULL)),
            column(6,
                   "Max of x axis:",
                   textInput("xlim_max", NULL))
          ),

          fluidRow(
            column(6,
                   "Label of x axis:",
                   textInput("xlab", NULL, "x")),
            column(6,
                   "Label of y axis:",
                   textInput("ylab", NULL, "Frequency"))
          ),
          "Color code of confidence interval:",
          textInput("color", NULL, "#dda0dd"),

          h3("Save As"),
          hr(style = "border-top: 1px solid #000000;"),

          fluidRow(
            column(6,
                   "Width of plot (inch):",
                   textInput("width", NULL, 8)),
            column(6,
                   "Height of plot (inch):",
                   textInput("height", NULL, 4))
          ),
          fluidRow(column(
            6,
            "Saved format:",
            radioButtons(
              inputId = "var3",
              label = NULL,
              choices = list("pdf", "png")
            )
          ),
          column(6,
                 uiOutput("ui_botm"))),


          h3(textOutput("message_data2")),
          hr(style = "border-top: 1px solid #000000;"),
          tableOutput('contents'),
          textOutput("message_data1")
            ),


        # sidebarPanel(
        #   actionButton("action_plot", "Test")
        # ),

        mainPanel(#textOutput("message_data2"),
          tabsetPanel(
            tabPanel(
              "Computation",
              plotOutput('plot'),
              #downloadButton(outputId = "down", label = "Save the plot"),
              br(),
              br(),
              br(),
              code("Run Log"),
              shinyjs::useShinyjs(),
              pre(id = "console")
            ),
            tabPanel("How to use BAD-ZUPA?", ""),
            tabPanel("Config for Professional", "")
          ))
          )
          ),

    server = function(input, output) {
      badzupa <- reactive({
        inFile <- input$file1
        read.csv(
          inFile$datapath,
          header = T,
          sep = ",",
          quote = ","
        )
      })

      output$message_data1 <- renderText({
        if (is.null(input$file1)) {
          return(NULL)
        } else {
          if (nrow(data) >= 11) {
            data <- badzupa()
            l <- nrow(data) - 10
            paste("....+", l, " elements")
          }
        }
      })

      output$message_data2 <- renderText({
        if (is.null(input$file1)) {
          return(NULL)
        } else {
          "Uploaded data"
        }
      })

      output$contents <- renderTable({
        if (is.null(input$file1)) {
          return(NULL)
        } else {
          data <- badzupa()
          data[1:10,]
        }
      })

      preplot <- reactive({
        data <- badzupa()
        d <- data[, 1]
        func <- function() {
          message("Progress of Step1 (estimate bandwidth):")
          bd <- badzupa::bddensity(d)
          message("Progress of Step2 (bootstrap sampling):")
          qq <- badzupa::bdquantile(bd)
          message("Done!")
          return(list(
            x = bd$x,
            y = bd$y,
            qx = qq$x,
            quantile = qq$quantile
          ))
        }
        withCallingHandlers(
          func(),
          # can use "warning" instead/on top of "message" to catch warnings too
          message = function(m) {
            shinyjs::html("console", m$message, TRUE)
          }
        )
      })

      badplot <- function() {
        if (is.null(input$file1)) {
          NULL
        } else {
          data <- badzupa()
          xlim <- NULL
          #print(input$xlim_min)

          if (input$xlim_min == "") {
            xlim[1] <- min(data[, 1])
          } else {
            xlim[1] <- as.numeric(input$xlim_min)
          }

          if (input$xlim_max == "") {
            xlim[2] <- max(data[, 1])
          } else {
            xlim[2] <- as.numeric(input$xlim_max)
          }

          if (input$color == "") {
            color <- "#dda0dd"
          } else {
            color <- input$color
          }
          #  if(input$xlim_max == "auto") xlim = NULL
          #  if(input$xlim_min == "auto") xlim = NULL

          #  xlim = c(as.numeric(input$xlim_min), as.numeric(input$xlim_max))

          ans <- preplot()

          plot(
            NA,
            NA,
            xlim = xlim,
            ylim = c(0, max(ans$quantile)),
            xlab = input$xlab,
            ylab = input$ylab
          )
          if (input$CI == 1) {
            polygon(c(ans$x, rev(ans$x)),
                    c(ans$quantile[1, ], rev(ans$quantile[2,])),
                    col = color,
                    border = NA)
          }
          lines(x = ans$x,
                y = ans$y,
                lwd = 1.4)

          abline(h = 0)
          if (input$rug == 1)
            rug(data[, 1])
        }
      }

      badplot_init <- function() {
        xlim = c(0, 0)
        if (input$xlim_min == "") {
          xlim[1] <- 0
        } else {
          xlim[1] <- as.numeric(input$xlim_min)
        }

        if (input$xlim_max == "") {
          xlim[2] <- 4600
        } else {
          xlim[2] <- as.numeric(input$xlim_max)
        }

        plot(
          NA,
          NA,
          xlim = xlim,
          ylim = c(0, 0.001),
          xlab = input$xlab,
          ylab = input$ylab
        )
        text(
          x = 2300,
          y =  0.00055,
          "Welcome to BAD-ZUPA!",
          cex = 3,
          col = "#400E82"
        )
        text(
          x = 2300,
          y =  0.00037,
          "This is BAD-ZUPA version 0.1",
          cex = 1,
          col = "#400E82"
        )
        text(
          x = 2300,
          y =  0.00032,
          "Author: Tan Furukawa (furukawatan@gmail.com)",
          cex = 1,
          col = "#400E82"
        )
      }


      output$plot <- renderPlot({
        badplot_init()
      })

      observeEvent(input$action_plot, {
        output$plot <- renderPlot({
          if (!is.null(input$file1))
            badplot()
          else
            badplot_init()
        })
      })



      observeEvent(input$action_plot, {
        output$downloadData <- downloadHandler(
          filename =  function() {
            paste("result", input$var3, sep = ".")
          },
          # content is a function with argument file. content writes the plot to the device
          content = function(file) {
            if (input$var3 == "pdf")
              pdf(file,
                  width = as.numeric(input$width),
                  height = as.numeric(input$height))# open the pdf device
            else
              png(
                file,
                width = as.numeric(input$width),
                height = as.numeric(input$height),
                units = "in",
                res = 1200,
                pointsize = 4
              ) # open the png device

            badplot()# draw the plot

            dev.off()  # turn the device off
          }
        )

        output$ui_botm <- renderUI({
          if (!is.null(input$file1)) {
            tagList(downloadButton("downloadData", "Download Plot"))
          }
        })
      })
    }
        )

}

