library(htmltools)
library(markdown)
library(shiny)
library(bslib)
library(tidyverse)
library(ggplot2)
library(formattable)
library(ggh4x)

icppt_eur <- read.csv("icppt_eur.csv")
icppt_entso <- read.csv("icppt_entso.csv")


unit.scale = function(x) (1/10000) * (x*100)^2








ui <- fluidPage(titlePanel(h1("Differences between Eurobase & Entso-E Transparency platform"), windowTitle = "⚡Entso-E Platform"),
                navbarPage("",
                           tabPanel(h5("IC-PPT"),
                                    mainPanel(
                                      tabsetPanel(
                                        tabPanel("About",
                                                 fluidPage(htmltools::includeMarkdown("md_files/1.md"))
                                                 ),
                                        tabPanel("Stats",
                                                 sidebarPanel(position="right",
                                                              fluid=F,
                                                    selectInput("nrg_type", "Choose Production Type :",
                                                              choices = colnames(icppt_eur[,4:10]),
                                                              selected = "Hydro"
                                                              ),
                                                    selectInput("ctry", "Choose Country :",
                                                                choices = unique(icppt_entso$Country),
                                                                selected = "France"
                                                    )
                                                  ),
                                                 mainPanel(
                                                   formattableOutput("table_icppt")
                                                 ),
                                                 sidebarPanel(position="left",
                                                              fluid=F
                                                 ),
                                                 mainPanel(
                                                   plotOutput("graph_icppt")
                                                 )
                                              )
                                      )
                                    )
                                  ),
                           
                           tabPanel(h5("IC-PPU"),
                                    tabsetPanel(
                                      tabPanel("About",
                                               fluidPage(htmltools::includeMarkdown("md_files/1.md"))
                                               ),
                                      tabPanel("Stats")
                                    )
                                  ),
                           tabPanel(h5("AG-PPT"),
                                    tabsetPanel(
                                      tabPanel("About",
                                               fluidPage(htmltools::includeMarkdown("md_files/1.md"))
                                               ),
                                      tabPanel("Stats")
                                    )
                                  ),
                           tabPanel(h5("AG-PPU"),
                                    tabsetPanel(
                                      tabPanel("About",
                                               fluidPage(htmltools::includeMarkdown("md_files/1.md"))
                                               ),
                                      tabPanel("Stats")
                                    )
                                  )
                                )
                              )












server <- function(input, output) {
  
  output$table_icppt <- renderFormattable({
    
    entso <- icppt_entso %>% filter(Country == input$ctry)
    eur   <- icppt_eur %>% filter(Country == input$ctry)
    
    df    <- inner_join(entso, eur, by = "Date", suffix = c(".entso", ".eur"))
    
    var_entso <- paste(input$nrg_type, "entso", sep=".")
    var_eur   <- paste(input$nrg_type, "eur", sep=".")
    
    df <- df %>% 
          select(Date, var_entso, var_eur) %>% 
          mutate(Difference = .[[var_entso]]-.[[var_eur]], 
                 Percentage = ((.[[var_entso]]-.[[var_eur]])/.[[var_eur]]))
    
    df$Percentage <- percent(df$Percentage)
    
    df %>% formattable(align =c("l","c","c","c","c", "c", "c", "c", "r"), 
                       caption = paste("Installed", 
                                       input$nrg_type, 
                                       "Capacity in", 
                                       input$ctry,
                                       "from",
                                       min(df$Date),
                                       "to",
                                       max(df$Date)),
                       list(
       Date = formatter("span", style = ~ style(color = "grey",font.weight = "bold")), 
       Percentage = color_bar("lightblue", fun = unit.scale) 
    ))
  })
  
  output$graph_icppt <- renderPlot({
    
    entso <- icppt_entso %>% filter(Country == input$ctry)
    eur   <- icppt_eur %>% filter(Country == input$ctry)
    
    var_entso <- paste(input$nrg_type, "entso", sep=".")
    var_eur   <- paste(input$nrg_type, "eur", sep=".")
    
    df    <- inner_join(entso, eur, by = "Date", 
                                    suffix = c(".entso", ".eur")) %>% 
             select(Date, var_entso, var_eur)
      
    
    var_entso <- paste(input$nrg_type, "entso", sep=".")
    var_eur   <- paste(input$nrg_type, "eur", sep=".")
    
    df %>% 
    ggplot(aes(x = Date)) +
      geom_line(aes(y = .data[[var_entso]], color = "entso"), size = 1) +
      geom_line(aes(y = .data[[var_eur]], color = "eurostat"), size = 1) +
      geom_ribbon(aes(ymin=.data[[var_entso]], ymax=pmin(.data[[var_entso]], .data[[var_eur]])), 
                  alpha=0.3, 
                  fill = "lightgreen") +
      geom_ribbon(aes(ymin=.data[[var_eur]], ymax=pmin(.data[[var_entso]], .data[[var_eur]])), 
                  alpha=0.3, 
                  fill = "red") +
      scale_color_manual(values = c("#3D85F7", "#C32E5A")) +
      labs(title = paste())
    
  })

}





shinyApp(ui = ui, server = server)
