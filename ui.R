library(shiny)
library(shinyWidgets)
library(shinythemes)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  theme = shinytheme("flatly"),
  setBackgroundImage(src = "https://windowseat.ph/wp-content/uploads/2016/01/31779248_xxl.jpg"),
  
  #setBackgroundColor("ghostwhite",gradient = "radial"),
  
  # Application title
  headerPanel("Mosquito Abundance Forecasting Model", windowTitle = "Mosquito Forecast Module"),
  sidebarLayout(
  sidebarPanel(
    
    
    selectInput("select","Select the Area",c("-SELECT-"="0","Karupur"="1","Suramangalam"="2","Arisipalayam"="3",
                                             "Ammapet"="4","Fairlands"="5","Shevapet"="6")),
  textInput("rain","Rainfall",placeholder = "Rainfall in cm"),
  textInput("min","Min. Temperature",placeholder = "Min. Temperature"),
  textInput("max","Max. Temperature",placeholder = "Max. Temperature"),
  textInput("rh1","Relative Humidity @ 8hrs",placeholder = "Relative Humidity @ 8"),
  textInput("rh2","Relative Humidity @ 14hrs",placeholder = "Relative Humidity @ 14")
                                            
  ),
  
  
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("distPlot"),
       tableOutput("distTable"),
       textOutput("mlr")
       
    )
   
  )
  )
)
