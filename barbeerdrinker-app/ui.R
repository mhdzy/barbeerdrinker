# Matthew Handzy
# barbeerdrinker extended shiny app
# ui.R

source("utilities.R")

# LIBRARY REQUIREMENTS
# markdown
# shiny
# shinythemes
# tidyverse
ensure_pkgs()
ensure_tbls()

ui <- tagList(fluidPage(
  
  # initialize shinyjs package
  useShinyjs(),
  
  theme = shinytheme("paper"),
      
  navbarPage("barbeerdrinker!",
  
    # ---------- about page ---------- #
    tabPanel("about",
      fluidRow(
          includeMarkdown("www/README.rmd")
      )         
    ),
    
    # ---------- bar page ---------- #
    tabPanel("bar",
      sidebarLayout(
        
        sidebarPanel(
          pickerInput(inputId = "bar_selection",
                      label = "select bar(s):",
                      choices = bars$name,
                      options = list(
                        `actions-box` = TRUE, 
                        size = 10,
                        `selected-text-format` = "count > 3"
                      ),
                      selected = bars$name,
                      multiple = TRUE),
          width = 3
        ),
        
        mainPanel(
          tabsetPanel( 
            
            type = "tabs",
            
            tabPanel(
              "hours",
              
              tags$br(),
              
              fluidRow(
                
              )
            ),
            
            tabPanel(
              "top drinkers @ bar(s)",
              
              tags$br(),
              
              fluidRow(
                plotOutput("bars_topdrinkers")
              )
            ),
            
            tabPanel(
              "top beers @ bar(s)",
              
              tags$br(),
              
              fluidRow(
                plotOutput("bars_topbeers")
              )
            ),
            
            tabPanel(
              "top manfs @ bar(s)",
              
              tags$br(),
              
              fluidRow(
                plotOutput("bars_topmanfs")
              )
            ),
                       
            tabPanel(
              "table",
              
              tags$br(),
              
              fluidRow(
                dataTableOutput("barsTable")
              )
            )
            
          ) # end tabset panel
        ) # end main panel
      ) # end sidebar layout
    ), # end bar panel
    
    # ---------- beer page ---------- #
    tabPanel(
      "beer",
      
      sidebarLayout(
        sidebarPanel(
          pickerInput(inputId = "beer_selection",
                      label = "select beer(s):",
                      choices = beers$name,
                      options = list(
                        `actions-box` = TRUE, 
                        size = 10,
                        `selected-text-format` = "count > 3"
                      ),
                      selected = beers$name,
                      multiple = TRUE),
          width = 3
        ), # end sidebar panel
      
        mainPanel(
          tabsetPanel( 
            
            type = "tabs",
            
            tabPanel(
              "top selling bars",
              
              tags$br(),
              
              fluidRow(
                plotOutput("beers_topbars")
              )
            ), # end tabpanel top selling bars
            
            tabPanel(
              "top buying drinkers",
              
              tags$br(),
              
              fluidRow(
                plotOutput("beers_topdrinkers")
              )
            ), # end tabpanel top buying drinkers
            
            tabPanel(
              "top selling time",
              
              tags$br(),
              
              fluidRow(
                plotOutput("bars_toptime")
              )
            ), # end tabpanel top selling time
            
            tabPanel(
              "table",
              
              tags$br(),
              
              fluidRow(
                dataTableOutput("beersTable")
              )
            ) # end table panel
            
          ) # end tabset panel
        ) # end main panel
      ) # end sidebar layout
    ), # end tabpanel beer
    
    # ---------- drinker page ---------- #
    tabPanel(
      "drinker",
      
      sidebarLayout(
        sidebarPanel(
          pickerInput(inputId = "drinker_selection",
                      label = "select drinkers(s):",
                      choices = drinkers$name,
                      options = list(
                        `actions-box` = TRUE, 
                        size = 10,
                        `selected-text-format` = "count > 3"
                      ),
                      selected = drinkers$name,
                      multiple = TRUE),
          width = 3
        ), # end sidebar panel
        
        mainPanel(
          
          tabsetPanel( 
            type = "tabs",
            
            tabPanel(
              "lookup",
              
              tags$br(),
              
              fluidRow(
                textAreaInput("lookup_query",
                              "drinker lookup",
                              "albert",
                              width = "124%",
                              rows = 1
                              )
              ),
              
              fluidRow(
                dataTableOutput("drinkersLookupTable")
              )
              
            ),
            
            tabPanel(
              "transactions",
              
              tags$br(),
              
              fluidRow(
                dataTableOutput("drinkers_tsns")
              )
            ), # end transactions panel
            
            tabPanel(
              "bill lookup",
              
              tags$br(),
              
              fluidRow(
                numericInput("t_id",
                             label = "transaction id",
                             value = 100028,
                             min = 100000,
                             max = 999999,
                             step = 1
                             )
              ),
              
              tags$br(),
              
              fluidRow(
                dataTableOutput("drinkers_tid")
              )
            ), # end bill lookup panel
            
            tabPanel(
              "top beers ordered",
              
              tags$br(),
              
              fluidRow(
                plotOutput("drinkers_topbeers")
              )
            ), # end top beers ordered panel
            
            tabPanel(
              "timeseries",
              
              tags$br(),
              
              fluidRow(
                plotOutput("drinkers_timeseries")
              )
            ), # end timeseries panel
            
            tabPanel(
              "table",
              
              tags$br(),
              
              fluidRow(
                dataTableOutput("drinkersTable")
              )
            ) # end table panel
            
          ) # end tabset panel
          
        ) # end main panel
      ) # end sidebar layout
    ), # end drinker tabPanel
    
    # ---------- modification page ---------- #
    tabPanel(
      
      "modification",
      
      fluidRow(
        h5("input custom MySQL DML command")
      ),
      
      tags$br(),
      
      fluidRow(
        column(1, dropdownButton(includeMarkdown("www/mod_dropdown.md"),
                       circle = T,
                       status = "danger",
                       icon = icon("question-circle"),
                       width = "500px",
                       tooltip = tooltipOptions(title = "click for DML command usage"))),
        
        column(6,
          textAreaInput("mod_query",
                      "custom sql modification query",
                      "alter table tbl_name\nadd column_name data_type",
                      width = "124%",
                      rows = 3
                      )
        )
    
      )
      
    
    ), # end tabpanel modification
    
    # ---------- sql page ---------- #
    tabPanel(
      
      "sql interface",
             
      tags$br(),
             
      fluidRow(
        textAreaInput("sql_query",
                      "custom sql query",
                      "select *\nfrom barbeerdrinker.bars\nwhere city = 'New York'",
                      width = "124%",
                      rows = 3
                      ),
        dataTableOutput("customSQLTable")
      )
      
    ) # end tabpanel sql interface
    
  )
                
))