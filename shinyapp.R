library(shiny)
library(ggplot2)
library(dplyr)
library(shinyWidgets)
ui <- fluidPage(
  setBackgroundColor(
    color = c("#F7FBFF", "#2171B5"),
    gradient = "radial",
    direction = c("top", "left")
  ),
  navbarPage("R for Big Data Assignment",

                 tabPanel("Price Per Arrondissement time series",
                        
                          sidebarLayout(

                            sidebarPanel(
                              fluidRow(
                                titlePanel(
                                  h2("Price Per Arrondissement time series", align = "center")
                                ),
                                column(12, align="center",
                                       numericInput("rangedata", "Observations total data: 657051", 10000, min = 1000, max = 657051),
                                       verbatimTextOutput("value")
                                ),
                                column(12, align="center",
                                       selectInput(inputId="arrondissement",label="Choose Arrondissement to Analyse",choices = c("Arrondissement 1"="01",
                                                                                                                                 "Arrondissement 2"="02",
                                                                                                                                 "Arrondissement 3"="03",
                                                                                                                                 "Arrondissement 4"="04",
                                                                                                                                 "Arrondissement 5"="05",
                                                                                                                                 "Arrondissement 6"="06",
                                                                                                                                 "Arrondissement 7"="07",
                                                                                                                                 "Arrondissement 8"="08",
                                                                                                                                 "Arrondissement 9"="09",
                                                                                                                                 "Arrondissement 10"="10",
                                                                                                                                 "Arrondissement 11"="11",
                                                                                                                                 "Arrondissement 12"="12",
                                                                                                                                 "Arrondissement 13"="13",
                                                                                                                                 "Arrondissement 14"="14",
                                                                                                                                 "Arrondissement 15"="15",
                                                                                                                                 "Arrondissement 16"="16",
                                                                                                                                 "Arrondissement 17"="17",
                                                                                                                                 "Arrondissement 18"="18",
                                                                                                                                 "Arrondissement 19"="19",
                                                                                                                                 "Arrondissement 20"="20",
                                                                                                                                 "All Arrondissement"="All Arrondissement"), selected = "Arrondissement 1",multiple = F))),

                              
                              column(12, align="center", checkboxGroupInput(inputId = "year", label = "Select Years to analyse", choices= c("2009"="2009",
                                                                                                                                    "2010"="2010",
                                                                                                                                    "2011"="2011",
                                                                                                                                    "2012"="2012",
                                                                                                                                    "2013"="2013",
                                                                                                                                    "2014"="2014",
                                                                                                                                    "2015"="2015",
                                                                                                                                    "2016"="2016"
                              ),selected = "2016")),
                              
                            ),
                            mainPanel(
                              
                              plotOutput(outputId = "arrondissementplot"),
                              titlePanel(
                                h6("Developed by: Nelson Lopez", align = "center")
                              ),
                            )
                            
                          )
                          
                          ),
                 tabPanel("Number of apartments by host",
                          sidebarLayout(
                            
                            sidebarPanel(
                              
                              titlePanel(
                                h1("Number of apartments by host", align = "center"),

                              ),
                              column(12, align="center",
                                     numericInput("hostrangedata", "Group by quantity of apartments and quantity of host, total sample data: 53", 53, min = 1, max = 53),
                                     verbatimTextOutput("hostrangedatavalue")
                              ),
                            ),
                            mainPanel(
                              
                              plotOutput(outputId = "hostplot"),
                              titlePanel(
                                h6("Developed by: Nelson Lopez", align = "center")
                              ),
                            )
                            
                          )
                          
                          ),
                 tabPanel("Renting Price mean per Arrondissement",
                          sidebarLayout(
                            
                            sidebarPanel(
                              titlePanel(
                                h2("Renting mean price per Arrondissement", align = "center")
                              ),
                              column(12, align="center",
                                     checkboxGroupInput(inputId="arrondissementprice",label="Choose Arrondissement to Analyse",choices = c("Arrondissement 1"="01",
                                                                                                                                    "Arrondissement 2"="02",
                                                                                                                                    "Arrondissement 3"="03",
                                                                                                                                    "Arrondissement 4"="04",
                                                                                                                                    "Arrondissement 5"="05",
                                                                                                                                    "Arrondissement 6"="06",
                                                                                                                                    "Arrondissement 7"="07",
                                                                                                                                    "Arrondissement 8"="08",
                                                                                                                                    "Arrondissement 9"="09",
                                                                                                                                    "Arrondissement 10"="10",
                                                                                                                                    "Arrondissement 11"="11",
                                                                                                                                    "Arrondissement 12"="12",
                                                                                                                                    "Arrondissement 13"="13",
                                                                                                                                    "Arrondissement 14"="14",
                                                                                                                                    "Arrondissement 15"="15",
                                                                                                                                    "Arrondissement 16"="16",
                                                                                                                                    "Arrondissement 17"="17",
                                                                                                                                    "Arrondissement 18"="18",
                                                                                                                                    "Arrondissement 19"="19",
                                                                                                                                    "Arrondissement 20"="20",
                                                                                                                                    "All Arrondissement"="All"), selected = "All"))),
                            
                            
                              
                       
                            mainPanel(
                              
                              plotOutput(outputId = "rentingprice"),
                              titlePanel(
                                h6("Developed by: Nelson Lopez", align = "center")
                              )
                            )
                            
                          )
                          
                          ),
                 tabPanel("Price Feature Relations",
                          sidebarLayout(
                            
                            sidebarPanel(
                              titlePanel(
                                h2("Relationship between price and apartment features listed", align = "center")
                              ),
                              column(12, align="center",
                                     selectInput(inputId="pricefeatures",label="Choose Arrondissement to Analyse",choices = c("Analysis by Property Type"="property_type",
                                                                                                                                           "Analysis by Room Type"="room_type", "Analysis by Bedrooms quantity"="bedrooms" ),
                                                 ,selected = "01", multiple = F))),

                            mainPanel(
                              
                              plotOutput(outputId = "pricefeatures"),
                              titlePanel(
                                h6("Developed by: Nelson Lopez", align = "center")
                              )
                            )
                            
                          )
                          
                          
                          )
)
)
server <- (function(input, output){
  
  output$arrondissementplot <- renderPlot({
    
    Visit_feq_df_grouped <- head(Visit_feq_df_grouped,input$rangedata)
    year_input <- input$year
    arrondissement_input <- c(input$arrondissement)
    
      if(input$arrondissement == "All Arrondissement"){
      visit_df_to_plot <- Visit_feq_df_grouped %>% filter(Year %in% year_input)
      ggplot(visit_df_to_plot, aes(x = month, y = nummonth, group = Year, color = Year, linetype = Year)) +
      geom_line()+ xlab("Month of the year") + ylab("Total visits") +
      facet_wrap(~ arrondissement) + labs(title= "Visit Frecuency Graph") + theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), axis.text.x=element_blank())
      } else{
        visit_df_to_plot <- Visit_feq_df_grouped %>% filter(arrondissement %in% arrondissement_input, Year %in% year_input)
        ggplot(visit_df_to_plot, aes(x = month, y = nummonth, group = Year, color = Year, linetype = Year)) +
        geom_line()+ xlab("Month of the year") + ylab("Total visits") + labs(title= "Visit Frecuency Graph") + theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), axis.text.x=element_blank())
      }
  })
  
  output$hostplot <- renderPlot({
    host_apartment_count$n <- as.numeric(levels(host_apartment_count$n))[host_apartment_count$n]
    df_host_to_plot <- head(df_host_to_plot,input$hostrangedata)
    
    ggplot(df_host_to_plot, aes(x=`host_apartment_count$n`, y=n) ) +
      geom_point(alpha = 2/5) + labs(title= "Number of aparments per host", y="Quantity of Host (Log10)", x = "Quantity of apartment owned", fill = "Legend - Host") + theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.position = "none") + scale_y_continuous(trans = "log10") 
  })
  output$rentingprice <- renderPlot({
    
    if(input$arrondissementprice == "All"){
      ggplot(mean_renting_price, aes(x=arrondissement, y=Renting_Price, fill=arrondissement) ) +
        geom_bar (stat="identity")+theme_minimal() + labs(title= "Renting Price mean per Arrondissement", y="Renting Price", x = "Arrondissement #", fill = "Arrondissement #") + theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.key.size = unit(0.4, 'cm'), legend.position = "none")
    }else{
      mean_renting_price <- mean_renting_price %>% filter(arrondissement %in% input$arrondissementprice)
      ggplot(mean_renting_price, aes(x=arrondissement, y=Renting_Price, fill=arrondissement) ) +
        geom_bar (stat="identity")+theme_minimal() + labs(title= "Renting Price mean per Arrondissement", y="Renting Price", x = "Arrondissement #", fill = "Arrondissement #") + theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.key.size = unit(0.4, 'cm'), legend.position = "none")
    }
  })
  output$pricefeatures <- renderPlot({
    
    
    
    ggplot(pricefeature_df, aes_string(x=input$pricefeatures, y=pricefeature_df$priceformatted, fill=input$pricefeatures) ) +
      geom_boxplot(outlier.shape = NA) + coord_flip() + theme(legend.key.size = unit(0.4, 'cm')) + labs(title= "Features vs Price analysis", y="Price in $", x = "Features", fill = "Features")+ theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.key.size = unit(0.4, 'cm'))

  })
  
})
shinyApp(ui = ui, server = server)


