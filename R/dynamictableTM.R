# Dynamic table to alter treatment effect
#' @import rhandsontable shiny


editmatrix <- function(outdir=getwd()){

    DFtrans <- data.frame(m.P)
    DFcost <- data.frame(m.P_treatment)


  ui <- shinyUI(fluidPage(

    titlePanel("Transition probability matrix: Edit and save the model input table"),
    sidebarLayout(
      sidebarPanel(width = 2,
                   helpText("Double-click on a cell to edit"),


                   br(),

                   wellPanel(
                     h3("Save table"),
                     div(class='row',
                         div(class="col-sm-6",
                             actionButton("save", "Save"))

                     )
                   )

      ),

      mainPanel(
        wellPanel(
          uiOutput("message", inline=TRUE)
        ),

        helpText("Make sure that the sum of each row is equal to 1!"),
        br(),
       helpText("Transitionmatrix of usual care"),
        rHandsontableOutput("hot"),
        br(),
       helpText("Transitionmatrix of treatment"),
        rHandsontableOutput("cost"),
        br(),

        plotOutput("plotmodel")

      )
    )
  ))

  server <- shinyServer(function(input, output, session) {
    session$onSessionEnded(function() {
      stopApp()
    })

    values <- reactiveValues()

    ## Handsontable
    # --- probability input
    observe({
      if (!is.null(input$hot)) {
        values[["previous"]] <- isolate(values[["DFtrans"]])
        DFtrans = hot_to_r(input$hot)
      } else {
        if (is.null(values[["DFtrans"]]))
          DFtrans <- DFtrans
        else
          DFtrans <- values[["DFtrans"]]
      }
      values[["DFtrans"]] <- DFtrans
    })

    output$hot <- renderRHandsontable({
      DFtrans <- values[["DFtrans"]]
      if (!is.null(DFtrans))
        rhandsontable(DFtrans, useTypes = as.logical(TRUE))
    })

    # --- cost input
    observe({
      if (!is.null(input$cost)) {
        values[["previous"]] <- isolate(values[["DFcost"]])
        DFcost = hot_to_r(input$cost)
      } else {
        if (is.null(values[["DFcost"]]))
          DFcost <- DFcost
        else
          DFcost <- values[["DFcost"]]
      }
      values[["DFcost"]] <- DFcost
    })

    output$cost <- renderRHandsontable({
      DFcost <- values[["DFcost"]]
      if (!is.null(DFcost))
        rhandsontable(DFcost, useTypes = as.logical(TRUE))
    })



    ## Save
    observeEvent(input$save, {
      #fileType <- isolate(input$fileType)
      finalDF <- isolate(values[["DFtrans"]])
      finalDF2 <- isolate(values[["DFcost"]])
      m.P <<- as.matrix(finalDF)
      m.P_treatment <<- as.matrix(finalDF2)

    }
    )


    output$plotmodel <- renderPlot({
      second(HS)
    })

    ##-- Message
    output$message <- renderUI({
      if(input$save==0){
        helpText(sprintf("When you are done editing the transition matrix, press Save and close this window.
                         To undo your change, press right-mouse button and reload the table"))
      }else{
        helpText(sprintf("Input saved. Please close this window to continue."))

      }
    })

  })

  ## run app
  runApp(list(ui=ui, server=server))
  return(invisible())
}
