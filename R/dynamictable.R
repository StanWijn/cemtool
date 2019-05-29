# Dynamic table to alter treatment effect
#' @import rhandsontable shiny


editTable <- function(DF, outdir=getwd(), outfilename="table"){
  if(HS==3){
  DFtrans <- DF[,1:3]
  DFcost  <- DF[,c(4:6, 10)]
  DFutil  <- DF[,7:9 ]
  } else if(HS==4){
    DFtrans <- DF[,1:6]
    DFcost  <- DF[,c(7:10, 15)]
    DFutil  <- DF[,11:14]
  } else if(HS==5){
    DFtrans <- DF[,1:10]
    DFcost  <- DF[,c(11:15,21 )]
    DFutil  <- DF[,16:20]
  } else if(HS==6){
    DFtrans <- DF[,1:15]
    DFcost  <- DF[,c(16:21,28) ]
    DFutil  <- DF[,22:27]
  }

  ui <- shinyUI(fluidPage(

    titlePanel("Markov model input: Edit and save the model input table"),
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


        br(), br(),

        rHandsontableOutput("hot"),
        br(),
        rHandsontableOutput("cost"),
        br(),
        rHandsontableOutput("effect"),
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
        rhandsontable(DFtrans, useTypes = as.logical(F))#, stretchH = "all")
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
        rhandsontable(DFcost, useTypes = as.numeric(T))#, stretchH = "all")
    })

    # --- effect input
    observe({
      if (!is.null(input$effect)) {
        values[["previous"]] <- isolate(values[["DFutil"]])
        DFutil = hot_to_r(input$effect)
      } else {
        if (is.null(values[["DFutil"]]))
          DFutil <- DFutil
        else
          DFutil <- values[["DFutil"]]
      }
      values[["DFutil"]] <- DFutil
    })

    output$effect <- renderRHandsontable({
      DFutil <- values[["DFutil"]]
      if (!is.null(DFutil))
        rhandsontable(DFutil, useTypes = as.logical(F))#, stretchH = "all")
    })
    #output$cost <- renderRHandsontable({
    #  DF <- values[["DFcost"]]
    #  if (!is.null(DF))
    #    rhandsontable(DF, useTypes = as.logical(FALSE), stretchH = "all")
    #})
    #output$effect <- renderRHandsontable({
    #  DF <- values[["DFutil"]]
    #  if (!is.null(DF))
    #    rhandsontable(DF, useTypes = as.logical(FALSE), stretchH = "all")
    #})

    ## Save
    observeEvent(input$save, {
      #fileType <- isolate(input$fileType)
      finalDF1 <- isolate(values[["DFtrans"]])
      finalDF2 <- isolate(values[["DFcost"]])
      finalDF3 <- isolate(values[["DFutil"]])
      finalDF <- cbind(finalDF1, finalDF2, finalDF3)
      modelinput2 <<- finalDF

    }
    )


   # observeEvent(input$addcolumn, {
   #   DF <- isolate(values[["DF"]])
   #   values[["previous"]] <- DF
   #   newcolumn <- eval(parse(text=sprintf('%s(nrow(DF))', isolate(input$newcolumntype))))
   #   values[["DF"]] <- setNames(cbind(DF, newcolumn, stringsAsFactors=FALSE), c(names(DF), isolate(input$newcolumnname)))
   # })

    output$plotmodel <- renderPlot({
      second(HS)
    })

    ## Message
    output$message <- renderUI({
      if(input$save==0){
        helpText(sprintf("When you are done editing the model input, press Save and close this window.
                         To undo your change, press right-mouse button and reload the table", outdir))
      }else{
        helpText(sprintf("Input saved. Please close this window to continue."))

      }
    })

  })

  ## run app
  runApp(list(ui=ui, server=server))
  return(invisible())
}
