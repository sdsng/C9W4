library(shiny) 

shinyServer(function(input, output) {
        model <- reactive({
                brushed_data <- brushedPoints(iris, input$brush1,xvar = "Sepal.Length", yvar = "Sepal.Width")
                if(nrow(brushed_data) < 2){
                        return(NULL) }
                lm(Sepal.Width ~ Sepal.Length, data = brushed_data) })
                                              
        output$slopeOut <- renderText({ 
                if(is.null(model())){
                        "No Model Found"
                } else {
                        model()[[1]][2] 
                        }
        })
        
        output$intOut <- renderText({ 
                if(is.null(model())){
                        "No Model Found"
                } else {
                        model()[[1]][1] }
        })
        
        output$plot1 <- renderPlot({
                plot(iris$Sepal.Length, iris$Sepal.Width, xlab = "Sepal Length", ylab = "Sepal Width", main = "Sepal Measurements", cex = 1.5, pch = 16, bty = "n")
                if(!is.null(model())){
                        abline(model(), col = "blue", lwd = 2) }
        })
})