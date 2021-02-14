

library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(DT)
library(jpeg)
library(grid)
library(leaflet)
library(scales)

ags <- read.csv("annual_generation_state.csv", stringsAsFactors = FALSE)
colnames(ags) <- c("Year","State","Type","ES","GMWH")
agstotal <- subset(ags, Type=="Total Electric Power Industry"&State=="US-TOTAL",select = c(Year,State,Type,ES,GMWH))
agsouttotal <- subset(agstotal, ES!="Total",select = c(Year,State,Type,ES,GMWH))
yegouttotal <- subset(agsouttotal,select = c("Year","ES","GMWH"))
value <- as.numeric(gsub(",","",yegouttotal$GMWH))
yegcopy <- yegouttotal
yegcopy$GMWH <- value



ui <- fluidPage(
   
   # Application title
   dashboardHeader(title = "             Jiajun Mo CS 424 Project 1"),
   dashboardSidebar(disable = FALSE, collapsed = FALSE,

                     sidebarMenu(
                       menuItem("Data From CS 424 class file, by Jiajun Mo at 2021/2/13", tabName = "ShowAbout", icon = NULL)
                     ),
	 mainPanel(
	plotOutput("SBES", height = 300),
	plotOutput("SBPES", height = 300),
	plotOutput("LCES", height = 300),
	plotOutput("LCPES", height = 300))
    )

)



server <- function(input, output) {
  

    output$SBES <- renderPlot({
     
     ggplot(yegouttotal, aes(x=Year, y=value,fill=ES))+  labs(x="Year", y="GENERATION (Megawatthours)", title="Stack Bar Amout") +coord_cartesian(xlim = c(1990,2019)) + geom_bar(position="dodge",stat="identity")
   })
   
   
   output$SBPES <- renderPlot({    
    ggplot(yegouttotal, aes(x=Year, y=value,fill=ES))+  labs(x="Year", y="GENERATION (Megawatthours)", title="Stack Bar Percentage")+ coord_cartesian(xlim = c(1990,2019)) + geom_bar(position="stack",stat="identity")
   })

   output$LCES <- renderPlot({    
    ggplot(yegouttotal) + geom_line(aes(x=Year, y=value,color=ES))+  labs(x="Year", y="GENERATION (Megawatthours)", title="Stack Bar Amout") + coord_cartesian(xlim = c(1990,2019))
   })

   output$LCPES <- renderPlot({    
    ggplot(yegcopy,aes(x=Year,y=GMWH,group=ES,fill=ES))+  labs(x="Year", y="GENERATION (Megawatthours)", title="Stack Bar Percentaget")+ coord_cartesian(xlim = c(1990,2019)) + geom_area(position = "stack",stat="identity")
   })
}

# Run the applicat?on 
shinyApp(ui = ui, server = server)

