library(shiny)
ui <- fluidPage(
  
  sliderInput(inputId = "num",label = "Choose number of days for training set",value=365,min=365,max=1500),
  dateInput(inputId="inputDate", label="Choose a starting date for training set", start = "2012-07-12", min = "2012-07-12",value="2012-07-12",
                 max = "2017-04-17", format = "yyyy-mm-dd"),
     plotOutput("hist")
  )

server <- function(input, output) {
 
  output$hist<- renderPlot({
    data_predicted <-get_weight(input$num,input$inputDate)
    dates <- data_predicted[[1]]
    pred <- data_predicted[[2]]
    data <- data_predicted[[3]]
    title<- "Forecast from Prophet"
    #plot(m,pred,main=title)
    
    fcst<-pred[c('ds','yhat','yhat_lower','yhat_upper')]
    m<-ggplot(data=data,aes(x=date,y=weight, fill="Real"))+geom_point(colour="lightblue")
    m+geom_line(data=fcst,aes(x=ds,y=yhat))+
    geom_line(data=fcst,aes(x=ds,y=yhat_lower),linetype="dotted")+
    geom_line(data=fcst,aes(x=ds,y=yhat_upper),linetype="dotted")+
    ggtitle("Predicted vs real weight") +
      xlab("Date") +
      ylab("Weight, lbs")+
      theme(legend.position = "right")+
      scale_fill_discrete("",labels=c("Real"))

  })
}

shinyApp(ui = ui, server = server)