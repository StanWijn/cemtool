# Dynamic table to alter treatment effect
#' @import rhandsontable shiny shinydashboard utils

#outdir=getwd()

editmatrix <- function(m.P, m.P_treatment){

    DFtrans <- data.frame(m.P)
    DFcost <- data.frame(m.P_treatment)

    title <- tags$div(h2("Step 3: Transition probability matrix"))
    
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
        helpText("Make sure that the sum of each row is equal to 1!"),
        br(),
        helpText("Rows indicate the originating healthstate, colums indicate targeted healthstate.
                 Example: first row, second column is the probability to move from the first healthstate to the second."),
        br(),
       helpText("Transitionmatrix of usual care"),
        rHandsontableOutput("hot"),
        br(),
       helpText("Transitionmatrix of treatment"),
        rHandsontableOutput("cost"),
       
       br()
       ),
       column(7,
        plotOutput("plotmodel")
       )
      ),
      
      fluidRow(column(4,
                      tags$hr(),
                      wellPanel(
                        div(class='row',
                            div(class="col-sm-6",
                                actionButton("save2", "Save")))
                      ),
                      tags$hr())
      )
    )
  

  server <- shinyServer(function(input, output, session) {
   session$onSessionEnded(function() {
     stopApp()
   })

    values <- reactiveValues()

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
        rhandsontable(DFtrans, useTypes = as.logical(F), stretchH = "all") %>% 
        hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) 
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
        rhandsontable(DFcost, useTypes = as.logical(F), stretchH = "all") %>% 
        hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) 
    })

  
    ## Save
    observeEvent(input$save | input$save2, {
      finalDF <- isolate(values[["DFtrans"]])
      finalDF2 <- isolate(values[["DFcost"]])
      assign('m.P', as.matrix(finalDF), envir = cemtool.env)
      assign('m.P_treatment', as.matrix(finalDF2), envir = cemtool.env)
     }
    )

    output$plotmodel <- renderPlot({
      second(cemtool.env$HS, cemtool.env$v.n)
    }, width = 900, height = 600)

    ##-- Message
    output$message <- renderUI({
      if(input$save==0 & input$save2 == 0){
        helpText(sprintf("When you are done editing the transition matrix, press Save and close this window.
                         To undo your change, press right-mouse button and reload the table"))
      }else{
        helpText(sprintf("Input saved. Please close this window to continue."))

      }
    })

  })

  ## run app
  runApp(shinyApp(ui= dashboardPage(header, sidebar, body), server=server))
  return(invisible())
}
