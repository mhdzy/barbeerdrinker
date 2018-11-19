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
    d <- get_tbl_mod(input$mod_query)
    if (d == "success") {
      return("success")
    } else {
      return(tbl_df(d[[1]])$value)
    }
  })
  
  update_mod_bars <- eventReactive(input$mod_bars, {
    # rules and logic!
    
    # construct query from params
    mod_query <- 
      sprintf("insert into %s values ('%s', '%s', '%s', '%s', '%s', '%s', '%s')",
              "bars",
              input$bars_name,
              input$bars_license,
              input$bars_city,
              input$bars_phone,
              input$bars_addr,
              input$bars_open,
              input$bars_close)
    
    d <- get_tbl_mod(mod_query)
    if (d == "success") {
      return("success")
    } else {
      return(tbl_df(d[[1]])$value)
    }
  })
  
  update_mod_beers <- eventReactive(input$mod_beers, {
    # rules and logic!
    
    # construct query from params
    mod_query <- 
      sprintf("insert into %s values ('%s', '%s', '%s', '%s')",
              "beers",
              input$beers_name,
              input$beers_manf,
              input$beers_rating,
              input$beers_abv)
    
    d <- get_tbl_mod(mod_query)
    if (d == "success") {
      return("success")
    } else {
      return(tbl_df(d[[1]])$value)
    }
  })
  
  update_mod_bills <- eventReactive(input$mod_bills, {
    # rules and logic!
    
    # construct query from params
    mod_query <- 
      sprintf("insert into %s values ('%s', '%s', '%s', '%s')",
              "bills",
              input$bills_tid,
              input$bills_item,
              input$bills_quantity,
              input$bills_price)
    
    d <- get_tbl_mod(mod_query)
    if (d == "success") {
      return("success")
    } else {
      return(tbl_df(d[[1]])$value)
    }
  })
  
  update_mod_drinkers <- eventReactive(input$mod_drinkers, {
    # rules and logic!
    
    # construct query from params
    mod_query <- 
      sprintf("insert into %s values ('%s', '%s', '%s', '%s')",
              "drinkers",
              input$drinkers_name,
              input$drinkers_city,
              input$drinkers_phone,
              input$drinkers_addr)
    
    d <- get_tbl_mod(mod_query)
    if (d == "success") {
      return("success")
    } else {
      return(tbl_df(d[[1]])$value)
    }
  })
  
  update_mod_frequents <- eventReactive(input$mod_frequents, {
    # rules and logic!
    
    # construct query from params
    mod_query <- 
      sprintf("insert into %s values ('%s', '%s')",
              "frequents",
              input$frequents_drinker,
              input$frequents_bar)
    
    # DRINKER CANNOT FREQUENT OUT OF STATE!
    # yes, these are horrible variable names
    # yes, I was in a time crunch and had to ensure unique names for these vars
    # no, I won't do it again
    # drinker must exist (constraint also set by SQL)
    if (input$frequents_drinker %in% drinkers$name) {
      uq_drinker <- input$frequents_drinker
      uq_bar <- input$frequents_bar
      
      uq_d <- 
        drinkers %>% 
        filter(name == uq_drinker)
      
      uq_b <- 
        (bars %>% 
        filter(name == uq_bar))$city
      
      if (uq_b == "New York") {
        uq_b <- "NY"
      } else if (uq_b == "Hoboken") {
        uq_b <- "NJ"
      } else if (uq_b == "Hartford") {
        uq_b <- "CT"
      }
      
      # compare drinker city vs bar city
      state <- substr(uq_d$city, nchar(uq_d$city)-1, nchar(uq_d$city))
      
      # match
      if (state == uq_b) {
        cat("freq assertion asserted")
      } else {
        return("FAILURE.\nfrequents assertion has not been satisfied. your drinker is frequenting an out-of-state bar.")
      }
    }
    
    d <- get_tbl_mod(mod_query)
    
    if (d == "success") {
      return("success")
    } else {
      return(tbl_df(d[[1]])$value)
    }
  })
  
  update_mod_likes <- eventReactive(input$mod_likes, {
    # rules and logic!
    
    # construct query from params
    mod_query <- 
      sprintf("insert into %s values ('%s', '%s')",
              "likes",
              input$likes_drinker,
              input$likes_beer)
    
    d <- get_tbl_mod(mod_query)
    if (d == "success") {
      return("success")
    } else {
      return(tbl_df(d[[1]])$value)
    }
  })
  
  update_mod_sells <- eventReactive(input$mod_sells, {
    # rules and logic!
    
    # construct query from params
    mod_query <- 
      sprintf("insert into %s values ('%s', '%s', '%s')",
              "sells",
              input$sells_bar,
              input$sells_item,
              input$sells_price)
    
    d <- get_tbl_mod(mod_query)
    if (d == "success") {
      return("success")
    } else {
      return(tbl_df(d[[1]])$value)
    }
  })
  
  update_mod_tsns <- eventReactive(input$mod_tsns, {
    # rules and logic!
    
    # construct query from params
    mod_query <- 
      sprintf("insert into %s values ('%s', '%s', '%s', '%s', '%s', '%s', '%s')",
              "transactions",
              input$tsns_tid,
              input$tsns_drinker_name,
              input$tsns_bar_name,
              input$tsns_time,
              input$tsns_date,
              input$tsns_subtotal,
              input$tsns_total)
    
    d <- get_tbl_mod(mod_query)
    if (d == "success") {
      return("success")
    } else {
      return(tbl_df(d[[1]])$value)
    }
  })
  
  update_sql_table <- eventReactive(input$sql_confirm_bttn, {
    get_tbl(input$sql_query)
  })
  
  # core output functions
  
  output$bars_timeseries_day <- renderPlot(g_bars_timeseries_day(input$bar_selection, input$bar_n, input$bar_color_selection))
  output$bars_timeseries_week <- renderPlot(g_bars_timeseries_week(input$bar_selection, input$bar_n, input$bar_color_selection))
  
  # bars page
  output$bars_topdrinkers <- renderPlot(g_bars_topspenders(input$bar_selection, input$bar_n, input$bar_color_selection))
  output$bars_topbeers <- renderPlot(g_bars_topbeers(input$bar_selection, input$bar_n, input$bar_color_selection))
  output$bars_topmanfs <- renderPlot(g_bars_topmanfs(input$bar_selection, input$bar_n, input$bar_color_selection))
  output$bars_timeseries <- renderPlot(g_bars_timeseries(input$bar_selection, input$bar_n, input$bar_color_selection, input$bars_dywk))
  output$barsTable <- renderDataTable(get_tbl("select * from bars"))

  # beers page
  output$beers_topbars <- renderPlot(g_beers_topbars(input$beer_selection, input$beer_n, input$beer_color_selection))
  output$beers_topdrinkers <- renderPlot(g_beers_topdrinkers(input$beer_selection, input$beer_n, input$beer_color_selection))
  output$beers_timeseries <- renderPlot(g_beers_timeseries(input$beer_selection, input$beer_n, input$beer_color_selection))
  output$beersTable <- renderDataTable(get_tbl("select * from beers"))

  # drinkers page
  output$drinkersLookupTable <- renderDataTable(t_drinkers_lookup(input$lookup_query))
  output$drinkers_tsns <- renderDataTable(t_drinkers_tsns(input$drinker_selection))
  output$drinkers_tid <- renderDataTable(t_drinkers_tid(input$t_id))
  output$drinkers_topbeers <- renderPlot(g_drinkers_beers(input$drinker_selection, input$drinker_n, input$drinker_color_selection))
  output$drinkers_timeseries <- renderPlot(g_drinkers_timeseries(input$drinker_selection, input$drinker_color_selection))
  output$drinkersTable <- renderDataTable(get_tbl("select * from drinkers"))

  # modification page
  output$mod_status <- renderText(update_mod_msg())
  output$mod_status_bars <- renderText(update_mod_bars())
  output$mod_status_beers <- renderText(update_mod_beers())
  output$mod_status_bills <- renderText(update_mod_bills())
  output$mod_status_drinkers <- renderText(update_mod_drinkers())
  output$mod_status_frequents <- renderText(update_mod_frequents())
  output$mod_status_likes <- renderText(update_mod_likes())
  output$mod_status_sells <- renderText(update_mod_sells())
  output$mod_status_tsns <- renderText(update_mod_tsns())
  
  # custom sql input page
  output$customSQLTable <- renderDataTable(update_sql_table())

}
