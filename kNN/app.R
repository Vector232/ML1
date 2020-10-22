library(shiny)

sortObjectsByDist = function(xl, z){
  
  l = dim(xl)[1];
  n = dim(xl)[2]-1;
  
  distances = matrix(NA, l, 2);
  
  for (i in 1:l){
    distances[i, ] = c(i, sqrt(sum((xl[i, 1:n] - z)^2)));
  }
  
  orderedXl = xl[order(distances[, 2]), ];
  return (orderedXl);
}

classif_kNN = function(orderedXl, k){
  
  classes = orderedXl[1:k, 3];
  
  counts = table(c(classes));
  
  class = names(which.max(counts));
  
  return (class)
}

kNN = function(xl, z, k){
  return(classif_kNN(sortObjectsByDist(xl, z), k))
}

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("KNN"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("k",
                        "k:",
                        min = 1,
                        max = 150,
                        value = 1),
            sliderInput("x",
                        "x:",
                        min = 10,
                        max = 70,
                        value = 1),
            sliderInput("y",
                        "y:",
                        min = 0,
                        max = 25,
                        value = 0)
        
        ),

        # Show a plot of the generated distribution
        mainPanel(
          plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    colors <- c("1" = "red", "2" = "green3", "3" = "blue")
  
    output$distPlot <- renderPlot({
      x = input$x/10
      y = input$y/10
      plot(iris[, 3:4], pch = 21, bg = colors[iris$Species], 
           col = colors[iris$Species], asp = 1, main = "kNN")
      dist = sortObjectsByDist(iris[, 3:5], c(x,y))
      class = classif_kNN(dist, input$k)
      points(dist[1:input$k, ], pch = 21, asp = 1)
      points(x, y, pch = 22, bg = colors[class], col = colors[class], asp = 1)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
