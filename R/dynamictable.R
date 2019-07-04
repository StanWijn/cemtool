# Dynamic table to alter treatment effect
#' @import rhandsontable shiny shinydashboard


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

  title <-tags$div(h2("Step 2: Markov model input"))
  header <- dashboardHeader(tags$li(class = "dropdown",
                                    tags$style(".main-header {max-height: 100px}"),
                                    tags$style(".main-header .logo {height: 100px}")),
                            title = title, 
                            
                            titleWidth = '100%')
  
  sidebar <- dashboardSidebar(disable = TRUE)
  
  body <- dashboardBody(
    tags$head(tags$style(HTML('
        .skin-blue .main-header .logo {
                            background-color: #3c8dbc;
                            }
                            .skin-blue .main-header .logo:hover {
                            background-color: #3c8dbc;
                            }
                            '))),
    tags$style(HTML("hr {border-top: 1px solid #000000;}")),
    tags$hr(),
    wellPanel(
      uiOutput("message", inline=TRUE),
      div(class='row',
          div(class="col-sm-6",
              actionButton("save", "Save")))
    ),
    tags$hr(),
    
        fluidRow(
          column(5, align = "left",
     
        helpText("Transition probabilities for both strategies.
                 Please enter in the probability to move from one state to another. (exampe: p.A is the probability to move from the first healhstate to the second 
                 healthstate. The probability to remain in a state will automatically be calculated. In this step it is not possible to include recovery probabilities
                 for example to return from the second healthstate to the first healthstate (reverse of p.A). This is posisble in the next step: Transition probability matrix"),
        rHandsontableOutput("hot"),
        br(),
        helpText("Costs of the healthstates for both strategies"),
        rHandsontableOutput("cost"),
        br(),
        helpText("Effects (utilities) for both strategies (ranging from 1 to 0; perfect healthy to death)"),
        rHandsontableOutput("effect"),
        br(),
        
        sliderInput("d.rc", label = h5("What is the discount rate for costs? (for the Netherlands: 0.04, the UK: 0.03)"), 
                    min = 0.01, max = 0.10, value = 0.04),
        sliderInput("d.re", label = h5("What is the discount rate for effects/utilities? (for the Netherlands: 0.015, the UK: 0.03"), 
                    min = 0.01, max = 0.10, value = 0.015)
        ),
        
        
        column(7,
        plotOutput("plotmodel"))),
        
    fluidRow(column(4,
                    tags$hr(),
                    wellPanel(
                      #  uiOutput("message", inline=TRUE),
                      div(class='row',
                          div(class="col-sm-6",
                              actionButton("save2", "Save and/or update plot")))
                    ),
                    tags$hr())
    )
        

      )
    


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
        rhandsontable(DFtrans, rowHeaderWidth = 150, useTypes = as.logical(F), stretchH = "all")
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
        rhandsontable(DFcost, rowHeaderWidth = 150, useTypes = as.numeric(T), stretchH = "all")
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
        rhandsontable(DFutil, rowHeaderWidth = 150, useTypes = as.logical(F), stretchH = "all")
    })
    
    # --- discount rates
    observeEvent(input$save| input$save2,{
      d.rc <<- as.numeric(input$d.rc)
      d.re <<- as.numeric(input$d.re)
    }) 
    
    
    ## Save
    observeEvent(input$save| input$save2, {
      #fileType <- isolate(input$fileType)
      finalDF1 <- isolate(values[["DFtrans"]])
      finalDF2 <- isolate(values[["DFcost"]])
      finalDF3 <- isolate(values[["DFutil"]])
      finalDF <- cbind(finalDF1, finalDF2, finalDF3)
      modelinput <<- finalDF

    }
    )

    output$plotmodel <- renderPlot({
        second(HS)}, width = 900, height = 600)

    ## Message
    output$message <- renderUI({
      if(input$save==0 & input$save2 == 0){
        helpText(sprintf("When you are done editing the model input, press Save and close this window.
                         To undo your change, press right-mouse button and reload the table", outdir))
      }else{
        helpText(sprintf("Input saved. Please close this window to continue."))

      }
    })

  })

  ## run app
  runApp(shinyApp(ui = dashboardPage(header, sidebar, body), server))
  return(invisible())
}
