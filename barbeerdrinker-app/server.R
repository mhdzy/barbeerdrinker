# Matthew Handzy
# barbeerdrinker extended shiny app
# server.R

source("utilities.R")

# LIBRARY REQUIREMENTS
# markdown
# shiny
# shinythemes
# tidyverse
ensure_pkgs()
ensure_tbls()

# server function
server <- function(input, output, session) {

  # reactive functions (modification & sql query pages)
  
  update_mod_msg <- eventReactive(input$mod_confirm_bttn, {
    # rules and logic!
    get_tbl_mod(input$mod_query)
  })
  
  update_sql_table <- eventReactive(input$sql_confirm_bttn, {
    get_tbl(input$sql_query)
  })
  
  # bars page
  output$bars_hours <- renderTimevis(tv_bars_hours(input$bar_selection, 10))
  output$bars_topdrinkers <- renderPlot(g_bars_topspenders(input$bar_selection, 10))
  output$bars_topbeers <- renderPlot(g_bars_topbeers(input$bar_selection, 10))
  output$bars_topmanfs <- renderPlot(g_bars_topmanfs(input$bar_selection, 10))
  output$barsTable <- renderDataTable(get_tbl("select * from bars"))

  # beers page
  output$beers_topbars <- renderPlot(g_beers_topbars(input$beer_selection, 10))
  output$beers_topdrinkers <- renderPlot(g_beers_topdrinkers(input$beer_selection, 10))
  output$beers_toptime <- renderPlot(g_beers_toptimeseries(input$beer_selection, 10))
  output$beersTable <- renderDataTable(get_tbl("select * from beers"))

  # drinkers page
  output$drinkersLookupTable <- renderDataTable(t_drinkers_lookup(input$lookup_query))
  output$drinkers_tsns <- renderDataTable(t_drinkers_tsns(input$drinker_selection))
  output$drinkers_tid <- renderDataTable(t_drinkers_tid(input$t_id))
  output$drinkers_topbeers <- renderPlot(g_drinkers_beers(input$drinker_selection, 10))
  output$drinkers_timeseries <- renderPlot(g_drinkers_timeseries(input$drinker_selection))
  output$drinkersTable <- renderDataTable(get_tbl("select * from drinkers"))

  # modification page
  output$mod_status <- renderDataTable(update_mod_msg())
  
  # custom sql input page
  output$customSQLTable <- renderDataTable(update_sql_table())

}
