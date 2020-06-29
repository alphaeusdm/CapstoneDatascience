library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("swiftKey"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      textInput("textIn", 
                label = h3("Text input: "), 
                value = "",
                placeholder = "enter text here"),
      
      a("For Source Code click here",href="https://github.com/alphaeusdm/CapstoneDatascience")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      h5("please wait a few momnets till the data loads"),
      br(),
      h4("Predicted Word: "),
      textOutput("textPred"),
      h4("Text Output with predicted word: "),
      textOutput("textOut")
    )
  )
))