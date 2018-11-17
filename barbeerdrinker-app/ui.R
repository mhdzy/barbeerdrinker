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

# __GLOBALVARS__
bar_limit <- 10
beer_limit <- 10
drinker_limit <- 25

ui <- tagList(fluidPage(

  # initialize shinyjs package
  useShinyjs(),

  # initialize theme
  theme = shinytheme("paper"),

  navbarPage("barbeerdrinker!",
             

    # ---------- landing page ---------- #
    tabPanel("home",
      fluidRow(
        setBackgroundImage(src = "www/landing_background.jpg")
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
                      selected = bars$name[1:bar_limit],
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
                      selected = beers$name[4:(4+beer_limit-1)],
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
                      selected = drinkers$name[1:drinker_limit],
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

      img(src='mod_header.png',
          align = "center",
          width = "100%"),

      tags$br(), tags$br(), tags$br(),

      fluidRow(
        column(1, dropdownButton(includeMarkdown("www/mod_dropdown.md"),
                       circle = T,
                       status = "danger",
                       icon = icon("question-circle"),
                       width = "500px",
                       tooltip = tooltipOptions(title = "help"))
          ),

        column(5,

               textAreaInput("mod_query",
                             "click ? for dml command usage",
                             "insert into table_name (column1, column2, ..., column_n)\nvalues (value1, value2, ..., value_n)",
                             width = "150%",
                             rows = 3
                             ),

               actionBttn(inputId = "mod_confirm_bttn",
                          label = "execute",
                          style = "unite",
                          color = "danger")
                          )
      ), # end fluidrow

      tags$br(),

      dataTableOutput("mod_status")

    ), # end tabpanel modification

    # ---------- sql page ---------- #
    tabPanel(

      "sql interface",

      img(src='sql_header.png',
          align = "center",
          width = "100%"),

      tags$br(), tags$br(), tags$br(),



      fluidRow(
        column(1, dropdownButton(includeMarkdown("www/sql_dropdown.md"),
                                 circle = T,
                                 status = "primary",
                                 icon = icon("question-circle"),
                                 width = "500px",
                                 tooltip = tooltipOptions(title = "help"))
        ),
        column(5,
          textAreaInput("sql_query",
                        "click ? for table descriptions",
                        "select *\nfrom barbeerdrinker.bars\nwhere city = 'New York'",
                        width = "124%",
                        rows = 3
                        ),

          actionBttn(inputId = "sql_confirm_bttn",
                     label = "execute",
                     style = "unite",
                     color = "success"
                     )
        )
      ),

      tags$br(),

      dataTableOutput("customSQLTable")

    ), # end tabpanel sql interface

    # ---------- readme page ---------- #
    tabPanel("readme",
             fluidRow( includeHTML("www/README.html") )
    ) # end readme page

  ) # end navbarpage

)) # end taglist, fluidpage
