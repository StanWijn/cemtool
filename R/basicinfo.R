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
             helpText("It is only possible to build 3 to 6 healthstate models, these include a start and death state. 
                 Press Save to visualize the model and save the input for further use", "\n"),
             sliderInput("healthstates", label = h4("How many healthstates does the model have?"), min = 3, max = 6, value = 3),
             br(),
             #step 2:
             textInput("HS1", label = h5("What is the name for the start state?:"), value = ""),
             textInput("HS2", label = h5("What is the name for health state 2?:"), value = ""),
             conditionalPanel(
               condition = "input.healthstates > 3",
               textInput("HS3", label = h5("What is the name for health state 3?:"), value = "")), 
             conditionalPanel(
               condition = "input.healthstates > 4",
               textInput("HS4", label = h5("What is the name for health state 4?:"), value = "")),
             conditionalPanel(
               condition = "input.healthstates > 5",
               textInput("HS5", label = h5("What is the name for health state 5?:"), value = "")),      
             textInput("dead", label = h5("What is the name for the absorption state?:"), value = ""),
             br()),
      column(7, 
             plotOutput("plotmodel")
      )
    ), 
    
    
    fluidRow(column(6,    
                    #step 3: other variables
                    sliderInput("cycles", label = h5("How many cycles do you want in years?:"), min = 2, max = 100, value = 10),
                    br(),
                    helpText("We only support the comparison of two strategies: usual care vs an intervention"),
                    textInput("usualcare", label = h5("What is the name of your usual care strategy?: "), value = ""),
                    textInput("intervention", label = h5("What is the name of your intervention strategy?: "), value = ""),
                    br())),
    
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
  
  
  
  
  ##---------------------------------------------------------------------------------------------
  server <- shinyServer(function(input, output, session) {
    session$onSessionEnded(function() {
      stopApp()
    })
    
    # step 1: health state
    observeEvent(input$save | input$save2,{
      HS <<- as.numeric(input$healthstates)
    }) 
    
    # step 2: names of health states:
    observeEvent(input$save| input$save2,{
      HS1 <<- as.character(input$HS1) 
      HS2 <<- as.character(input$HS2) 
      HS3 <<- as.character(input$HS3) 
      HS4 <<- as.character(input$HS4) 
      HS5 <<- as.character(input$HS5) 
      dead <<- as.character(input$dead)}) 
    
    # step 3: other variables:
    observeEvent(input$save | input$save2,{
      n.t <<- as.integer(input$cycles)})
    observeEvent(input$save| input$save2,{
      control <<- as.character(input$usualcare)}) 
    observeEvent(input$save| input$save2,{
      intervention <<- as.character(input$intervention)}) 
    observeEvent(input$save| input$save2,{
      Strategies <<- c(control, intervention)})
    observeEvent(input$save| input$save2,{
      n.s <<- HS})
    observeEvent(input$save| input$save2,{
      if(HS==3){
        v.n <<- c(HS1, HS2, dead)
      } else if(HS==4){
        v.n <<- c(HS1, HS2, HS3, dead)
      } else if(HS==5){
        v.n <<- c(HS1, HS2, HS3, HS4, dead)
      } else if(HS==6){
        v.n <<- c(HS1, HS2, HS3, HS4, HS5, dead)
      }
    })
    
    
    ## plot:
    output$plotmodel <- renderPlot({
     if(input$save ==0 & input$save2 == 0){first(input$healthstates)
      }else{
        second(HS)  
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
  # runApp(list(ui=ui, server=server))
  return(invisible())
}

