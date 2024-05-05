library(shiny)

#df <- read.csv("../datas/questions2.csv")
df <- read.csv("../data/questions2.csv",nrows=5)
df2 <- cbind(df, Selected=FALSE, Correct=FALSE)



# Define radio button module
questionModuleUI <- function(id) {
  
  ns <- NS(id)
  tagList(
    wellPanel(h5(""),
              
              radioButtons(ns("radio"), "Choose an option:",
                           choices = c("NONE")),
              
             
             
    )
            
    
  )
  
}



questionModuleServer <- function(id, df, index, dfgrades){
  
  moduleServer( id, function(input, output, session){
    # Update choices dynamically
    
    observeEvent(index$indexvalue,{
      questionNo = df[index$indexvalue,1]
      question = df[index$indexvalue,2]
      choices <- as.numeric(paste(unlist(df[index$indexvalue,4:8])))
      answer <- df[index$indexvalue,3]
      prevSelected <- dfgrades$df_data$Selected[index$indexvalue]
      
      
      updateRadioButtons(session, "radio",
                         label = paste("Question ", questionNo, ":  What is ", question ),
                         choices =  choices,
                         selected = prevSelected)
     
    })
    
    # Return the selected value
    selected_value <- reactive({
      input$radio
    })
    
    return(selected_value)
  })
}





# Define UI
ui <- fluidPage(
  wellPanel(h2("Micky's Math Test")),
  fluidRow(
      column(2),
      column(8, 
             questionModuleUI("rb_module")),
      column(2)
  ), 
  fluidRow(
    
    
    column(2,) ,
    column(8,
           wellPanel(actionButton("submit", "Submit"),
                     textOutput("result"))
           ),
    column(2,) ,
      
  ),
  fluidRow(
    
    
    column(2,
           wellPanel(actionButton("prevq", "Previous"))) ,
    column(8,
           wellPanel(h5("Select an answer, then press Submit"))),
    column(2,
           wellPanel(actionButton("nextq", "Next"))) ,
  ),
  fluidRow(
    column(2, "Gradebook" ),
    column(8,  tableOutput("table1"),),
    column(2)
  ), 
  fluidRow(
    column(2, "Answer List"),
    column(8,  tableOutput("table2"),),
    column(2)
  ),
  
  
)

# Define server logic
server <- function(input, output, session) {
  
  df <- read.csv("../data/questions2.csv",nrows=5)
  df2 <- cbind(df, Selected=FALSE, Correct=FALSE)

  dfgrades <- reactiveValues(df_data = NULL)
  dfgrades$df_data <- df2
  
  
  columns <- c("Item","Question","Answer","Selected","Correct") 
  df3 <- data.frame(matrix(nrow = 0, ncol = length(columns))) 
  colnames(df3) <- columns
  print(df3)

  
  dfanswers <- reactiveValues(df_data = NULL)
  dfanswers$df_data <- df3
  #print(dfanswers$df_data)
  
  
  # Index for questions df 
  index <- reactiveValues(indexvalue = 1)
  
  selected <- questionModuleServer("rb_module", df, index, dfgrades)
  
  append_answer <- function(selected, index, dfanswers, df ) {
    
    question <- df[index$indexvalue,2]
    answer <- df[index$indexvalue,3]
    if(answer == selected){
      correct = TRUE
    }else {
      correct <- FALSE
    }
    
    insert_list <- c(index$indexvalue,
                     question,
                     answer,
                     selected,
                     correct
                    )
    temp <- dfanswers$df_data
    
    temp[nrow(temp) + 1,] <- insert_list
    print(temp)
    return(temp)
    
  }
  
  update_grades <- function(selected, df, dfgrades,  index) {
    answer <- df[index$indexvalue,3]
    temp <- dfgrades$df_data
    temp$Selected[index$indexvalue] = selected
    if ( selected() == answer)  {
      temp$Correct[index$indexvalue] = TRUE
      output$result <- renderText({
        paste( "Correct " )
      })  
    } else {
      temp$Correct[index$indexvalue] = FALSE
      output$result <- renderText({
        paste( selected(), " is not correct." )
      })  
    }
    return(temp)
  }
  
  observeEvent(input$submit, {
    
    answer <- df[index$indexvalue,3]
    
    if(is.null(selected())){
      output$result <- renderText({
        paste( "Please Select an Answer: " )
      })
    }
    else if ( selected() == "NONE") {}
    else {
      # update gradebook
      dfgrades$df_data <- update_grades(selected(), df, dfgrades, index)
      # update answer list
      dfanswers$df_data <- append_answer(selected(), index, dfanswers, df )
      # splice off the last three answered questions and see if all 3 answered incorrectly
      tail3 <- tail(dfanswers$df_data$Correct,3)
      finished = all(tail3 == FALSE)
      if(finished){
        print("finished")
      }
      if ( selected() == answer)  {
        output$result <- renderText({
          paste( "Correct " )
        })  
      } else {
        output$result <- renderText({
          paste( selected(), " is not correct." )
        })  
      }
    }
  })  
  
  observeEvent(input$nextq, {
    n <- nrow(df)
    if (index$indexvalue < n) {
      index$indexvalue <- index$indexvalue + 1
      output$result <- renderText({
        paste("")
      }) 
      
    } 
    updateActionButton(session, "nextq",
                       label = "Next",
    )
    updateActionButton(session, "prevq",
                       label = "Previous",
    )
    if (index$indexvalue == n){
      updateActionButton(session, "nextq",
                         label = "Finish",)
    }
  })
  
  observeEvent(input$prevq, {
    n <- nrow(df)
    if (index$indexvalue >  1) {
      index$indexvalue <- index$indexvalue - 1
      output$result <- renderText({
        paste("")
      })  
    }
    updateActionButton(session, "nextq",
                       label = "Next",
    )
    updateActionButton(session, "prevq",
                       label = "Previous",
    )
    if (index$indexvalue == 1){
      updateActionButton(session, "prevq",
                         label = "Start",
      )
    }
  })
  
  # Output the gradebook table
  output$table1 <- renderTable({
     dfgrades$df_data
  })
  
  # Output the answers table
  output$table2 <- renderTable({
    dfanswers$df_data
  })
}

# Run the application
shinyApp(ui = ui, server = server)
