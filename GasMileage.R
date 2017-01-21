#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
  titlePanel("Predict your car's mileage"),
  
  # Sidebar with a slider input for number cylinders, horsepower, weight and transmission
  sidebarLayout(
    sidebarPanel(
      sliderInput("cyl1", "Number of Cylinders:", min = 4,max = 12,value = 6),
      sliderInput("hp1","Estimated horsepower:", min = 50,max = 600, value = 250),
      sliderInput("wt1","Weight in kg:", min = 1,max = 10, value = 2),
      sliderInput("am1","Transmission (0 = automatic, 1 = manual):", min = 0,max = 1, value = 0,step = 1),
      numericInput("conf1","Desired Prediction Interval Confidence (%):  ",min=50,max=100,value= 90),
      submitButton('Predict mileage')
    ),
    
    # Show the output along with prediction interval
    mainPanel(
      p('Predicted gas mileage based on inputs:'),
      verbatimTextOutput("prediction")
    )
  )
)

data(mtcars)

prediction<-function(cyl1,hp1,wt1,am1,conf1) {
  pred <- lm(mpg~cyl + hp + wt + am, data = mtcars)
  new_preds <- data.frame(cyl=cyl1,hp=hp1,wt=wt1,am=am1)
  p_pred <- predict(pred,newdata = new_preds, interval = "prediction",level = conf1/100)[1]
  p_lwr <- predict(pred,newdata = new_preds, interval = "prediction",level = conf1/100)[2]
  p_upr <- predict(pred,newdata = new_preds, interval = "prediction",level = conf1/100)[3]
  cat('Predicted mileage is ',round(p_pred,2), ' mpg with a ', conf1,'% prediction interval of (', round(p_lwr,2),' mpg,',round(p_upr,2),' mpg).')
  
}
# Define server logic required to draw a histogram
server <- shinyServer(
  function(input, output) {
    
    output$inputcylvalue <- renderPrint({input$cyl1})
    output$inputhpvalue <- renderPrint({input$hp1})
    output$inputwtvalue <- renderPrint({input$wt1})
    output$inputamvalue <- renderPrint({input$am1})
    output$inputconfvalue <- renderPrint({input$conf1})
    output$prediction <- renderPrint({prediction(input$cyl1,input$hp1,input$wt1,input$am1,input$conf1)})
    
  }
  
)
# Run the application 
shinyApp(ui = ui, server = server)

