# Dynamic table to alter treatment effect
#' @import rhandsontable shiny shinydashboard


basic <- function(){
  
  
  title <- tags$div(h2("Step 1: Basic information"))
  
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
              actionButton("save", "Save and/or update plot")))
    ),
    tags$hr(),
    
    
    fluidRow(
      column(5, align = "left",
             
             #step 1:
             helpText("You can build a Markov model with 3 to 6 health state models, these include a start and death/absorption state."),
              helpText("For example: Healthy -> Sick -> Dead"),
            helpText("Press Save to visualize the model and save the input for further use"),
             sliderInput("healthstates", label = h4("How many health states does the model have?"), min = 3, max = 6, value = 3),
             br(),
             #step 2:
             textInput("HS1", label = h5("What is the name for the start state? (e.g. Healthy):"), value = ""),
             textInput("HS2", label = h5("What is the name for health state 2?(e.g. Sick):"), value = ""),
             conditionalPanel(
               condition = "input.healthstates > 3",
               textInput("HS3", label = h5("What is the name for health state 3?:"), value = "")), 
             conditionalPanel(
               condition = "input.healthstates > 4",
               textInput("HS4", label = h5("What is the name for health state 4?:"), value = "")),
             conditionalPanel(
               condition = "input.healthstates > 5",
               textInput("HS5", label = h5("What is the name for health state 5?:"), value = "")),      
             textInput("dead", label = h5("What is the name for the death/absorption state?(e.g. Dead):"), value = ""),
             br()),
      column(7, 
             plotOutput("plotmodel")
      )
    ), 
    
    
    fluidRow(column(6,    
                    #step 3: other variables
                    sliderInput("cycles", label = h5("How many years do you want the model to run? (cycles):"), min = 2, max = 100, value = 10),
                    br(),
                    helpText("You can compare two strategies: usual care vs an intervention (e.g. surgery vs physiotherapy)"),
                    textInput("usualcare", label = h5("What is the name of your usual care strategy?: "), value = ""),
                    textInput("intervention", label = h5("What is the name of your intervention strategy?: "), value = ""),
                    br())),
    
    fluidRow(column(4,
      tags$hr(),
     wellPanel(
       div(class='row',
           div(class="col-sm-6",
               actionButton("save2", "Save and/or update plot")))
     ),
      tags$hr())
    )
    )
  
  
  
  
  ##---------------------------------------------------------------------------------------------
  server <- shinyServer(function(input, output, session) {
    session$onSessionEnded(function() {
      stopApp()
    })
    
    # step 1: health state
    observeEvent(input$save | input$save2,{
      assign('HS', as.numeric(input$healthstates), envir = cemtool.env)
      # step 2: names of health states:
      assign('HS1', as.character(input$HS1) , envir = cemtool.env)
      assign('HS2', as.character(input$HS2) , envir = cemtool.env)
      assign('HS3', as.character(input$HS3) , envir = cemtool.env)
      assign('HS4', as.character(input$HS4) , envir = cemtool.env)
      assign('HS5', as.character(input$HS5) , envir = cemtool.env)
      assign('dead', as.character(input$dead) , envir = cemtool.env)
      # step 3: other variables:
      assign('n.t', as.integer(input$cycles), envir = cemtool.env)
      assign('control', as.character(input$usualcare), envir = cemtool.env)
      assign('intervention', as.character(input$intervention), envir = cemtool.env)
      cemtool.env$Strategies <- c(cemtool.env$control, cemtool.env$intervention)
      cemtool.env$plot <- second(cemtool.env$HS, cemtool.env$v.n)  
      
      if(cemtool.env$HS==3){
        cemtool.env$v.n <- c(cemtool.env$HS1, cemtool.env$HS2, cemtool.env$dead)
      } else if(cemtool.env$HS==4){
        cemtool.env$v.n <- c(cemtool.env$HS1, cemtool.env$HS2, cemtool.env$HS3, cemtool.env$dead)
        
      } else if(cemtool.env$HS==5){
        cemtool.env$v.n <- c(cemtool.env$HS1, cemtool.env$HS2, cemtool.env$HS3, cemtool.env$HS4, cemtool.env$dead)
        
      } else if(cemtool.env$HS==6){
        cemtool.env$v.n <- c(cemtool.env$HS1, cemtool.env$HS2, cemtool.env$HS3,
                        cemtool.env$HS4, cemtool.env$HS5, cemtool.env$dead)
      
    }
      }) 
    
    
    
    ## plot:
    output$plotmodel <- renderPlot({
     if(input$save ==0 & input$save2 == 0){
        first(input$healthstates)
      }else{
        HS <- input$healthstates
        
        if(input$healthstates==3){
          v.n <- c(input$HS1, input$HS2, input$dead)
        } else if(input$healthstates==4){
          v.n <- c(input$HS1, input$HS2, input$HS3, input$dead)
          
        } else if(input$healthstates==5){
          v.n <- c(input$HS1, input$HS2, input$HS3, input$HS4, input$dead)
          
        } else if(input$healthstates==6){
          v.n <- c(input$HS1, input$HS2, input$HS3,
                   input$HS4, input$HS5, input$dead)
          
        }
        second(HS, v.n)

      }
    }, width = 900, height = 600)
    
    
    
    ##-- Message
    output$message <- renderUI({
      if(input$save==0 & input$save2 == 0){
        helpText(sprintf("When you are done editing the basic information, press Save and close this window."))
      }else{
        helpText(sprintf("Input saved. If you want to update the plot, press Save again.
                         Please close this window to continue."))
        
      }
    })
    
  })
  
  
  
  ## run app
  runApp(shinyApp(ui = dashboardPage(header, sidebar, body), server))
  return(invisible())
  
}

