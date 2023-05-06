library(shiny)
library(dplyr)
library(ggplot2)

bd1 <- read.csv("data/terrenos2.csv")
bd2 <- read.csv("data/terrenos_descricao.csv")

bd_merged <- merge(bd1, bd2, by.x = "Id", by.y = "Id_Terreno")


ui <- fluidPage(

    titlePanel("Terrenos"),

    tabsetPanel(
      id = "tab",
      tabPanel("Por Vegetação", value = 1),
      tabPanel("Soma Por Sexo", value = 2),
      tabPanel("3", value = 3),
      tabPanel("4", value = 4),
    ),
    
    sidebarLayout(
        sidebarPanel(
            conditionalPanel(condition="input.tab==1", 
               radioButtons("denVeg", "Densidade da Vegetação",c("Alta", "Baixa")),
            ),
            
            conditionalPanel(condition="input.tab==2", 
               sliderInput("priceRange", "Preço entre:",
                           min = 1, max = 100, value = c(1, 100)
               ),
            ),
        ),

        mainPanel(
          conditionalPanel(condition="input.tab==1", 
             plotOutput("denVegPlot")
          ),
          
          conditionalPanel(condition="input.tab==2", 
             plotOutput("sumSexPlot")
          ),
           
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$denVegPlot <- renderPlot({
      bdFiltered <- filter(bd_merged, Densidade_Vegetacao==input$denVeg)
      infoPxT <- aes(x=Tamanho,y=Preco,main="Terrenos")
      ggplot(data=bdFiltered, infoPxT) + geom_bar(aes(fill=Sexo), stat="identity", position="dodge")
    })
    
    output$sumSexPlot <- renderPlot({
      bdFiltered <- filter(bd_merged ,between(Preco, input$priceRange[1], input$priceRange[2]))
      masc <- subset(bdFiltered, Sexo=="M")
      fem <- subset(bdFiltered, Sexo=="F")
      totalPrecoM <- masc$Preco |> sum()
      totalPrecoF <- fem$Preco |> sum()
      
      dfSxV <- data.frame(Sexo=c("M","F"),TotalMilRS=c(totalPrecoM,totalPrecoF))
      infoSxV <- aes(x=Sexo,y=TotalMilRS,main="Terrenos")
      ggplot(data=dfSxV, infoSxV) + geom_bar(aes(fill=Sexo), stat="identity", position="dodge")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
