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
  
  bar_limit <- 10
  beer_limit <- 10
  drinker_limit <- 5

  # reactive functions (modification & sql query pages)
  
  update_mod_msg <- eventReactive(input$mod_confirm_bttn, {
    # rules and logic!
    get_tbl_mod(input$mod_query)
  })
  
  update_sql_table <- eventReactive(input$sql_confirm_bttn, {
    get_tbl(input$sql_query)
  })
  
  output$bars_timeseries_day <- renderPlot(g_bars_timeseries_day(input$bar_selection, bar_limit, input$bar_color_selection))
  output$bars_timeseries_week <- renderPlot(g_bars_timeseries_week(input$bar_selection, bar_limit, input$bar_color_selection))
  
  # bars page
  output$bars_hours <- renderTimevis(tv_bars_hours(input$bar_selection, bar_limit, input$bar_color_selection))
  output$bars_topdrinkers <- renderPlot(g_bars_topspenders(input$bar_selection, bar_limit, input$bar_color_selection))
  output$bars_topbeers <- renderPlot(g_bars_topbeers(input$bar_selection, bar_limit, input$bar_color_selection))
  output$bars_topmanfs <- renderPlot(g_bars_topmanfs(input$bar_selection, bar_limit, input$bar_color_selection))
  output$bars_timeseries <- renderPlot(g_bars_timeseries(input$bar_selection, bar_limit, input$bar_color_selection, input$bars_dywk))
  output$barsTable <- renderDataTable(get_tbl("select * from bars"))

  # beers page
  output$beers_topbars <- renderPlot(g_beers_topbars(input$beer_selection, beer_limit, input$bar_color_selection))
  output$beers_topdrinkers <- renderPlot(g_beers_topdrinkers(input$beer_selection, beer_limit, input$bar_color_selection))
  output$beers_timeseries <- renderPlot(g_beers_timeseries(input$beer_selection, beer_limit, input$bar_color_selection))
  output$beersTable <- renderDataTable(get_tbl("select * from beers"))

  # drinkers page
  output$drinkersLookupTable <- renderDataTable(t_drinkers_lookup(input$lookup_query))
  output$drinkers_tsns <- renderDataTable(t_drinkers_tsns(input$drinker_selection))
  output$drinkers_tid <- renderDataTable(t_drinkers_tid(input$t_id))
  output$drinkers_topbeers <- renderPlot(g_drinkers_beers(input$drinker_selection, drinker_limit, input$bar_color_selection))
  output$drinkers_timeseries <- renderPlot(g_drinkers_timeseries(input$drinker_selection, input$bar_color_selection))
  output$drinkersTable <- renderDataTable(get_tbl("select * from drinkers"))

  # modification page
  output$mod_status <- renderDataTable(
    #update_mod_msg()
    get_tbl("select name as DO_NOT, name as EXECUTE_UPDATES, name as PLEASE from beers where name = 'Bud Lite'")
  )
  
  # custom sql input page
  output$customSQLTable <- renderDataTable(update_sql_table())

}
