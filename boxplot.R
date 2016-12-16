
require("jsonlite")
require("RCurl")
require("dplyr")
require("plyr")
require("tidyr")
require("ggplot2")
require("ggthemes")
library(shiny)
require(car)
library("shinydashboard")
library(plotly)

ui<-shinyUI(fluidPage(
  selectInput(input = "gender",
              label = "Choose a Gender: ",
              c("Male", "Female"), selected = "Male"),
  
  mainPanel(
    plotlyOutput("boxplot"), width = 500, height = 500)
))
server <- shinyServer(function(input, output){
  
  output$boxplot<-renderPlotly({
df<- data.frame(fromJSON(getURL(URLencode('oraclerest.cs.utexas.edu:5001/rest/native/?query="SELECT * FROM DEMOGRAPHICS d INNER JOIN MENTALHEALTH m ON d.id_num = m.id_num INNER JOIN DRUGUSE du ON m.id_num = du.id_num"'),httpheader=c(DB='jdbc:oracle:thin:@aevum.cs.utexas.edu:1521/f16pdb', USER='cs329e_jmc6473', PASS='orcl_jmc6473', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE) ))
df$scode<-mapvalues(df$BETTER_DEAD, from = c("Nearly every day", "More than half the days", "Several days", "Never"), to=c("1", "2", "3", "4"))
    df <-df %>%
      dplyr::select(GENDER, RATIO_INCOME_POVERTY, BETTER_DEAD)%>%
      dplyr::filter(GENDER%in%c(input$gender), RATIO_INCOME_POVERTY!="null",BETTER_DEAD!="null")%>%
      dplyr::group_by(GENDER, RATIO_INCOME_POVERTY, BETTER_DEAD)
      
      p<-ggplot(data= df, aes(x =BETTER_DEAD, y=as.numeric(as.character(RATIO_INCOME_POVERTY)), fill=BETTER_DEAD)) + geom_boxplot() + xlim(c("Nearly every day", "More than half the days", "Several days", "Never")) + ylim(0,5) + xlab("") + ylab("Ratio Income Poverty") + theme(legend.position="none")
ggplotly(p)
  })})
shinyApp(ui = ui, server = server)
