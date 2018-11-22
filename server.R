library(shiny)
library(neuralnet)
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$distPlot <- renderPlot({
    
    mos_ANN <- read.csv("E:/Data Analytics/mos_ANN.csv")
    mos1=mos_ANN[-1]
    mos2=mos1
    
    
    rain=as.numeric(input$rain)
    mt1=as.integer(input$min)
    mt2=as.integer(input$max)
    rh1=as.integer(input$rh1)
    rh2=as.integer(input$rh2)
    
    mos2=rbind(mos2,c(rain,mt1,mt2,rh1,rh2))
    
    mos2$Rainfall=(mos2$Rainfall-min(mos2$Rainfall))/(max(mos2$Rainfall)-min(mos2$Rainfall))
    mos2$Min..Temperature=(mos2$Min..Temperature-min(mos2$Min..Temperature))/(max(mos2$Min..Temperature)-min(mos2$in..Temperature))
    mos2$Max..Temperature=(mos2$Max..Temperature-min(mos2$Max..Temperature))/(max(mos2$Max..Temperature)-min(mos2$Max..Temperature))
    mos2$Relative.Humidity.1..8.00.hrs.=(mos2$Relative.Humidity.1..8.00.hrs.-min(mos2$Relative.Humidity.1..8.00.hrs.))/(max(mos2$Relative.Humidity.1..8.00.hrs.)-min(mos2$Relative.Humidity.1..8.00.hrs.))
    mos2$Relative.Humidity.2..14.00.hrs.=(mos2$Relative.Humidity.2..14.00.hrs.-min(mos2$Relative.Humidity.2..14.00.hrs.))/(max(mos2$Relative.Humidity.2..14.00.hrs.)-min(mos2$Relative.Humidity.2..14.00.hrs.))
    
    library(neuralnet)
    set.seed(333)
    nnet=neuralnet(Population~Rainfall+Min..Temperature+Max..Temperature+Relative.Humidity.1..8.00.hrs.+Relative.Humidity.2..14.00.hrs.,
                   data=mos2,hidden=1,err.fct = "ce",linear.output = FALSE,
                   lifesign = "full", rep=5,
                   algorithm = "rprop+", stepmax = 1000)
    
    plot(nnet,rep=2)
    
   
  })
  output$distTable <- renderTable({
    
    mos_ANN <- read.csv("E:/Data Analytics/mos_ANN.csv")
    mos1=mos_ANN[-1]
    mos2=mos1
    
    rain=as.integer(input$rain)
    mt1=as.integer(input$min)
    mt2=as.integer(input$max)
    rh1=as.integer(input$rh1)
    rh2=as.integer(input$rh2)
    
    mos2=rbind(mos2,c(rain,mt1,mt2,rh1,rh2))
    
    mos2$Rainfall=(mos2$Rainfall-min(mos2$Rainfall))/(max(mos2$Rainfall)-min(mos2$Rainfall))
    mos2$Min..Temperature=(mos2$Min..Temperature-min(mos2$Min..Temperature))/(max(mos2$Min..Temperature)-min(mos2$in..Temperature))
    mos2$Max..Temperature=(mos2$Max..Temperature-min(mos2$Max..Temperature))/(max(mos2$Max..Temperature)-min(mos2$Max..Temperature))
    mos2$Relative.Humidity.1..8.00.hrs.=(mos2$Relative.Humidity.1..8.00.hrs.-min(mos2$Relative.Humidity.1..8.00.hrs.))/(max(mos2$Relative.Humidity.1..8.00.hrs.)-min(mos2$Relative.Humidity.1..8.00.hrs.))
    mos2$Relative.Humidity.2..14.00.hrs.=(mos2$Relative.Humidity.2..14.00.hrs.-min(mos2$Relative.Humidity.2..14.00.hrs.))/(max(mos2$Relative.Humidity.2..14.00.hrs.)-min(mos2$Relative.Humidity.2..14.00.hrs.))
    
    mos3=mos2
    
    
    library(neuralnet)
    set.seed(333)
    nnet=neuralnet(Population~Rainfall+Min..Temperature+Max..Temperature+Relative.Humidity.1..8.00.hrs.+Relative.Humidity.2..14.00.hrs.,
                   data=mos3,hidden=1,err.fct = "ce",linear.output = FALSE,
                   lifesign = "full", learningrate=0.7,
                   algorithm = "rprop+", stepmax = 1000)
    
    output=compute(nnet,mos3[nrow(mos3),-6])
    
    p1=output$net.result
    
    
    
    pred1=ifelse(p1<0.50,0,1)
    
    
    a=c(pred1,"0 : Safe | 1 : Unsafe")
    
  })
  
  output$mlr <- renderText({ 
    
    #converting the input values to numeric
    
    r=as.integer(input$rain)
    mt1=as.integer(input$min)
    mt2=as.integer(input$max)
    rh1=as.integer(input$rh1)
    rh2=as.integer(input$rh2)
    
    
    #round the value
    Population=-360.4787+(0.001818149 *r)+(-24.69082 *mt1)+(31.59007*mt2)+(-3.891116 *rh1)+(5.117325 *rh2)
    
    if(Population<0)
    {
      percent="2%"
    }
    if(Population<5)
    {
      percent="4%"
    }
    else if(Population<12)
    {
      percent="8%"
    }
    else if(Population<25)
    {
      percent="12%"
    }
    else if(Population<50)
    {
      percent="14%"
    }
    else if(Population<75)
    {
      percent="25%"
    }
    else if(Population<150)
    {
      percent="35%"
    }
    else if(Population<275)
    {
      percent="40%"
    }
    else if(Population<450)
    {
      percent="55%"
    }
    else if(Population<650)
    {
      percent="60%"
    }
    else if(Population<850)
    {
      percent="70%"
    }
    else if(Population<1200)
    {
      percent="85%"
    }
    else if(Population<2200)
    {
      percent="90%"
    }
    else
    {
      percent="100%"
    }
    
    
    
    a=c("Population increases by",percent)
    
  })
})
