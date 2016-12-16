# NHANES_DataVisualizations
[Flex Dashboard Link](https://janelchumley.shinyapps.io/FlexDashboard/)
##**Data Source**
The data comes from the National Health and Nutrition Examination Survey (NHANES), from the 2013-14 session. NHANES is designed to assess the health and nutritional status of adults and children in the U.S., and is a major program of the National Center for Health Statistics (NCHS). The survey examines a nationally representative sample of about 5,000 people each year; they are in counties across the country, 15 of which are visited each year. We downloaded six tables of data: responses to questions about alcohol use, demographics, drug use, income, mental health, and occupation. The dataset can be found [here](https://wwwn.cdc.gov/Nchs/Nhanes/Search/DataPage.aspx?Component=Demographics&CycleBeginYear=2013). 

##**Flexdashboard Storyboard Creation**
**Creating the markdown file**

Step 1: Create an R markdown file with the flexdashboard::flex_dashboard output format. 

Step 2: Add the storyboard: true option to the dashboard and runtime: shiny.

Step 3: Include a set of level 3 (###) dashboard components. Each component will be allocated it’s own frame in the storyboard, with the section title used as the navigation caption.

Step 4: Call all required R libraries

```{r, eval=FALSE}
---
title: "CDC National Health and Nutrition Survey Data Analysis"
output: 
  flexdashboard::flex_dashboard:
    storyboard: true
runtime: shiny
---
  
library(flexdashboard)
require("jsonlite")
require("RCurl")
require("dplyr")
require("plyr")
require("tidyr")
require("ggplot2")
require("ggthemes")
library(shiny)
require(car)
library(plotly)
require(rbokeh)
```

###**Tab 1: Bar Graph Using Shiny and Plotly HTML Widget**

![](tab1.png)

Step 1: Use required flexdashboard storyboard syntax to create a tab for the bar graph.

```{r, eval=FALSE}
-----------------------------------------------------------------------
### Bar Graph: Depression v. Annual Income by Marital Status
```

Step 2: Create the Shiny server script. First we need to do a SQL query within the data.frame expression. Within the SQL query, we'll create multiple joins between the following tables: **DEMOGRAPHICS, MENTALHEALTH, DRUGUSE.**

```{r, eval=FALSE}
df<- data.frame(fromJSON(getURL(URLencode('oraclerest.cs.utexas.edu:5001/rest/native/?query="SELECT * FROM DEMOGRAPHICS d 
                                          INNER JOIN MENTALHEALTH m ON d.id_num = m.id_num 
                                          INNER JOIN DRUGUSE du ON m.id_num = du.id_num"'),httpheader=c(DB='jdbc:oracle:thin:@aevum.cs.utexas.edu:1521/f16pdb', USER='cs329e_jmc6473', PASS='orcl_jmc6473', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE) ))
server <- shinyServer(function(input, output){
```

Step 3: Create a **selectInput** Shiny widget. Here we're using the attribute **marital** as input. Then, call the plotly function **plotlyOutput** passing in the parameter **barplot** and setting **width** and **height** to **"auto"**. 

```{r, eval=FALSE}
selectInput(input = "marital",
                     label = "Select a Martial Status: ",
                     c("Divorced", "Living with partner", "Married", "Never married", "Separated", "Widowed"), selected = "Married")

plotlyOutput("barplot",width = "auto", height = "auto")

```

Step 4: Call the **renderPlotly** function, assigning it to the **output$barplot** expression . Within the function, use the **dplyr** queries **select, filter, group_by, and summarize** to call the required attributes: **MARITAL, ANNUAL_HH_INCOME, DEPRESSED.**

```{r, eval=FALSE}

  output$barplot<-renderPlotly({

    df <-df %>%
      dplyr::select(MARITAL, ANNUAL_HH_INCOME, DEPRESSED)%>%
      dplyr::filter(MARITAL%in%(input$marital), !ANNUAL_HH_INCOME%in%c("null", "Under $20,000", "$20,000 + "), DEPRESSED!="null" )%>%
      dplyr::group_by(MARITAL, ANNUAL_HH_INCOME, DEPRESSED)%>%
      dplyr::summarize(count = n())%>%dplyr::mutate(percentage = count/sum(count), pos = (cumsum(percentage) - 0.5 * percentage))
```

Step 5: Call the **mapvalues** function, assigning it to the **df$scode** expression. This maps the **ANNUAL_HH_INCOME** attributes from characters to factors, which will allow us to order these attributes in the ggplot expression. 

```{r, eval=FALSE}
    df$scode<-mapvalues(df$ANNUAL_HH_INCOME, from = c("$0 - 4,999", "$5,000 - 9,999", "$10,000 - 14,999", "$15,000 - 19,999", "$20,000 - 24,999", "$25,000 - 34,999", "$35,000 - 44,999", "$45,000 - 54,999", "$55,000 - 64,999", "$65,000 - 74,999", "$75,000 - 99,999", "$100,000 +"), to=c("1", "2", "3", "4", "5", "6","7", "8", "9","10", "11", "12"))
```  

Step 6: Create a ggplot expression. 

```{r, eval=FALSE}
p <- ggplot(data= df, aes(x =ANNUAL_HH_INCOME, y=percentage, fill=DEPRESSED)) + 
  geom_bar(stat="identity", position ="stack") + 
  labs(title = "") + 
  ggtitle("Depression v. Annual Income") + 
  theme(legend.title = element_text(size=8), plot.title = element_text(size=10, face="bold"), axis.title.x = element_text(size=10), axis.text=element_text(size=7)) + 
  geom_text(data = subset(df,percentage>=0.05), aes(y=pos, label = paste(round(percentage*100,2), "%")), size=2, color="white")+ 
  xlim(c("$0 - 4,999", "$5,000 - 9,999", "$10,000 - 14,999", "$15,000 - 19,999", "$20,000 - 24,999", "$25,000 - 34,999", "$35,000 - 44,999", "$45,000 - 54,999", "$55,000 - 64,999", "$65,000 - 74,999", "$75,000 - 99,999", "$100,000 +")) + 
  xlab("") + 
  ylab("% of Total Count Depressed") +  
  scale_y_continuous(expand=c(0,0)) + 
  coord_flip() 
```

Step 7: Call the **ggplotly** function passing in the ggplot expression as the parameter, and pipe **layout(margin=list(b=150))** to make adjustments to the bottom margin. 

```{r, eval=FALSE}
ggplotly(p)%>%layout(margin=list(b=150))
})})
```

####################################################################
###**Tab 2: Box Plot Using Shiny and Plotly HTML Widget**

![](tab2.png)

Step 1: Use required flexdashboard storyboard syntax to create a tab for the box plot.

```{r, eval=FALSE}
-----------------------------------------------------------------------
### Box Plot: Ratio Income Poverty v. Severe Depression by Gender
```

Step 2: Next we create the Shiny server script. First we need to do a SQL query within the data.frame expression. Within the SQL query, we'll create multiple joins between the following tables: **DEMOGRAPHICS, MENTALHEALTH, DRUGUSE.**

```{r, eval=FALSE}
df<- data.frame(fromJSON(getURL(URLencode('oraclerest.cs.utexas.edu:5001/rest/native/?query="SELECT * FROM DEMOGRAPHICS d 
                                          INNER JOIN MENTALHEALTH m ON d.id_num = m.id_num 
                                          INNER JOIN DRUGUSE du ON m.id_num = du.id_num"'),httpheader=c(DB='jdbc:oracle:thin:@aevum.cs.utexas.edu:1521/f16pdb', USER='cs329e_jmc6473', PASS='orcl_jmc6473', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE) ))
server <- shinyServer(function(input, output){
```

Step 3: Call the **mapvalues** function, assigning it to the **df$scode** expression. This maps the **BETTER_DEAD** attributes from characters to factors, allowing us to order these attributes in the ggplot expression. 

```{r, eval=FALSE}
df$scode<-mapvalues(df$BETTER_DEAD, from = c("Nearly every day", "More than half the days", "Several days", "Never"), to=c("1", "2", "3", "4"))
```  

Step 4: Create a **selectInput** Shiny widget. Here we're using the attribute **gender** as input. Then, call the plotly function **plotlyOutput** passing in the parameter **boxplot**, and setting **width** and **height** to **"auto"**.

```{r, eval=FALSE}
selectInput(input = "gender",
              label = "Select a Gender: ",
              c("Male", "Female"), selected = "Male")

plotlyOutput("boxplot", width = "auto", height = "auto")
))
```

Step 5: Call **renderPlotly** function, assigning it to the **output$boxplot** expression . Within the function, use the **dplyr** queries **select, filter, group_by, and summarize** to call the required attributes: **GENDER, RATIO_INCOME_POVERTY, BETTER_DEAD.** 

```{r, eval=FALSE}
  output$boxplot<-renderPlotly({
      df <-df %>%
      dplyr::select(GENDER, RATIO_INCOME_POVERTY, BETTER_DEAD)%>%
      dplyr::filter(GENDER%in%c(input$gender), RATIO_INCOME_POVERTY!="null",BETTER_DEAD!="null")%>%
      dplyr::group_by(GENDER, RATIO_INCOME_POVERTY, BETTER_DEAD)
```

Step 6: Create a ggplot expression. 

```{r, eval=FALSE}
p <- ggplot(data= df, aes(x =ANNUAL_HH_INCOME, y=percentage, fill=DEPRESSED)) + 
  geom_bar(stat="identity", position ="stack") + 
  labs(title = "") + 
  ggtitle("") + 
  theme(legend.title =element_text(size=8), legend.text = element_text(size=6), plot.title=element_text(size=10, face="bold"), axis.title.x = element_text(size=7), axis.text=element_text(size=6)) + 
  geom_text(data = subset(df,percentage>=0.05), aes(y=pos, label = paste(round(percentage*100,1), "%")), size=1.75, color="white") + 
  scale_x_discrete(limits= c("$0 - 4,999", "$5,000 - 9,999", "$10,000 - 14,999", "$15,000 - 19,999", "$20,000 - 24,999", "$25,000 - 34,999", "$35,000 - 44,999", "$45,000 - 54,999", "$55,000 - 64,999", "$65,000 - 74,999", "$75,000 - 99,999", "$100,000 +")) + 
  xlab("") + 
  ylab("% of Total Count Depressed") + 
  scale_y_continuous(expand=c(0,0)) + 
  coord_flip() 
```

Step 7: Call the **ggplotly** function passing in the ggplot expression as the parameter, and pipe **layout(margin=list(b=150))** to make adjustments to the bottom margin. 

```{r, eval=FALSE}
ggplotly(p)%>%layout(margin=list(b=150))
})})
```
