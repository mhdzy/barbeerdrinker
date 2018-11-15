# Matthew Handzy
# barbeerdrinker extended shiny app
# utilities.R

# ========== ========== pool ========== ========== #

# pool utility function
poolConnect <- function() {
  return(
    pool::dbPool(
      drv = RMySQL::MySQL(),
      dbname = "barbeerdrinker",
      host = "barbeerdrinker.cwbowizjcrlm.us-west-1.rds.amazonaws.com",
      username = "juanmatthew",
      password = "password"
    )
  )
}

# ========== ========== sql ========== ========== #

# sql pull utility functions
get_tbl <- function(query) {
  
  # connect to database
  pool <- poolConnect()
  
  # push query through pool object
  tmp <- tbl_df(dbGetQuery(pool, query))
  
  # cleanup pool object
  poolClose(pool)
  rm(pool)
  
  # return result of sql query
  return(tmp)
}

# sql DML command executor
get_tbl_mod <- function(query) {
  
  # connect to database
  pool <- poolConnect()
  
  # attempt a DML command (requires checks)
  result = tryCatch({
    tmp <- tbl_df(dbGetQuery(pool, query))
    
    poolClose(pool)
    rm(pool)
    
    return(tmp)
  }, warning = function(w) {
    return(w)
  }, error = function(e) {
    return(e)
  }, finally = {
    # cleanup code
  })
}

# ========== ========== utility functions ========== ========== #

# ensures the proper installation & loading of R packages
ensure_pkgs <- function() {
  if (!require(markdown))     { install.packages("markdown");     require(markdown)       }
  if (!require(tidyverse))    { install.packages("tidyverse");    require(tidyverse)      }
  if (!require(pool))         { install.packages("pool");         require(pool)           }
  if (!require(RMySQL))       { install.packages("RMySQL");       require(RMySQL)         }
  if (!require(timevis))      { install.packages("timevis");      require(timevis)        }
  if (!require(shiny))        { install.packages("shiny");        require(shiny)          }
  if (!require(shinyjs))      { install.packages("shinyjs");      require(shinyjs)        }
  if (!require(shinythemes))  { install.packages("shinythemes");  require(shinythemes)    }
  if (!require(shinyWidgets)) { install.packages("shinyWidgets"); require(shinyWidgets)   }
}

# ensures the local existence of barbeerdrinker tables
# use <<- for global var declaration
ensure_tbls <- function() {
  if (!exists("bars"))       { bars        <<- get_tbl("select * from bars")           }
  if (!exists("beers"))      { beers       <<- get_tbl("select * from beers")          }
  if (!exists("bills"))      { bills       <<- get_tbl("select * from bills")          }
  if (!exists("drinkers"))   { drinkers    <<- get_tbl("select * from drinkers")       }
  if (!exists("frequents"))  { frequents   <<- get_tbl("select * from frequents")      }
  if (!exists("likes"))      { likes       <<- get_tbl("select * from likes")          }
  if (!exists("sells"))      { sells       <<- get_tbl("select * from sells")          }
  if (!exists("tsns"))       { tsns        <<- get_tbl("select * from transactions")   }
}

# resets the local instances of barbeerdrinker tables
# use <<- for global var declaration
reset_tbls <- function() {
  bars        <<- get_tbl("select * from bars") 
  beers       <<- get_tbl("select * from beers") 
  bills       <<- get_tbl("select * from bills") 
  drinkers    <<- get_tbl("select * from drinkers") 
  frequents   <<- get_tbl("select * from frequents") 
  likes       <<- get_tbl("select * from likes") 
  sells       <<- get_tbl("select * from sells") 
  tsns        <<- get_tbl("select * from transactions") 
}

# ========== ========== table/graphing functions ========== ========== #

# ========== ========== bars ========== ========== #

# timevis hours chart for top bars
# requires: bars
tv_bars_hours <- function(bar, n) {
  
  bar %>%
  select(name, open, close)
  
}

# bar graph for top drinkers who are largest spenders (by dollar)
# requires: transactions
g_bars_topspenders <- function(bar, n) {
  
  ensure_tbls()
  
  tmp <- tsns %>%
    subset(tsns$bar_name %in% tbl_df(bar)[[1]]) %>% 
    group_by(drinker_name) %>% 
    mutate(drinker_total = sum(total)) %>% 
    group_by(drinker_total, add = T) %>% 
    summarise() %>% 
    ungroup() %>% 
    arrange(desc(drinker_total)) %>% 
    top_n(n, drinker_total) 
  
  p <- tmp %>%
    ggplot(aes(x = reorder(drinker_name, drinker_total), y = drinker_total)) +
    geom_bar(stat = "identity") +
    labs(title = "Drinkers Who Are Largest Spenders",
         caption = "spenders calculated by $ amount spent",
         x = "drinker name",
         y = "total $ spent") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(plot.title = element_text(hjust = 0.5))
  
  rm(tmp)
  
  return(p)
  
}

# bar graph for beers which are most popular
# requires: beers, bills
g_bars_topbeers <- function(bar, n) {
  
  ensure_tbls()
  
  tmp <- left_join(bills, tsns, by = "transaction_id")
  
  tmp <- tmp %>% 
    subset(tmp$bar_name %in% tbl_df(bar)[[1]])
  
  tmp <- tmp %>% 
    subset(tmp$item %in% beers$name) %>%
    group_by(item) %>%
    mutate(count = sum(quantity)) %>%
    group_by(count, add = T) %>%
    summarise() %>%
    ungroup() %>% 
    arrange(desc(count)) %>% 
    top_n(n, count)
  
  p <- tmp %>%
    ggplot(aes(x = reorder(item, count), y = count)) +
    geom_bar(stat = "identity") +
    labs(title = "Most Popular Beers",
         caption = "popularity calculated by total sales volume",
         x = "beer name",
         y = "total sales count") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(plot.title = element_text(hjust = 0.5))
  
  rm(tmp)
  
  return(p)
  
}

# bar graph for manufacturers who sell the most beers
# requires: beers, bills
g_bars_topmanfs <- function(bar, n) {
  
  ensure_tbls()
  
  tmp <- left_join(bills, tsns, by = "transaction_id")
  
  tmp <- tmp %>% 
    subset(tmp$bar_name %in% tbl_df(bar)[[1]])
  
  tmp <- tmp %>%
    subset(tmp$item %in% beers$name) %>%
    group_by(item) %>%
    mutate(count = sum(quantity)) %>%
    group_by(count, add = T) %>%
    summarise() %>%
    ungroup()
  
  colnames(tmp) <- c("name",
                     "count")
  
  tmp <- left_join(tmp, beers, by = "name") %>%
    select(manf, count) %>%
    group_by(manf) %>%
    mutate(total = sum(count)) %>%
    group_by(total, add = T) %>%
    summarise() %>%
    ungroup() %>% 
    arrange(desc(total)) %>% 
    top_n(n, total)
  
  colnames(tmp) <- c("name",
                     "count")
  
  p <- tmp %>%
    ggplot(aes(x = reorder(name, count), y = count)) +
    geom_bar(stat = "identity") +
    labs(title = "Most Popular Manufacturers",
         caption = "popularity calculated by total sales volume",
         x = "manufacturer name",
         y = "total sales count") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(plot.title = element_text(hjust = 0.5))
  
  rm(tmp)
  
  return(p)
  
}

# ========== ========== beers ========== ========== #

# given a beer, show bars where this beer sells the most (again only top)
# requires: bills, transactions
g_beers_topbars <- function(beer, n) {
  
  ensure_tbls()
  
  tmp <- left_join(subset(bills, bills$item %in% tbl_df(beer)[[1]]), 
                   tsns,
                   by = "transaction_id") %>% 
    select(bar_name, item, quantity) %>% 
    group_by(bar_name, item) %>% 
    mutate(count = sum(quantity)) %>% 
    group_by(count, add = T) %>% 
    summarise() %>% 
    ungroup() %>% 
    arrange(desc(count)) %>% 
    top_n(n, count)
  
  p <- tmp %>%
    ggplot(aes(x = reorder(bar_name, count), y = count)) +
    geom_bar(stat = "identity") +
    labs(title = "Top Bars Which Sell Your Beer(s)",
         caption = "top bars calculated by total sales volume",
         x = "bar name",
         y = "total sales count") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(plot.title = element_text(hjust = 0.5))
  
  rm(tmp)
  
  return(p)
  
}

# given a beer, show also drinkers who are the biggest consumers of this beer
# requires: bills, transactions
g_beers_topdrinkers <- function(beer, n) {
  
  ensure_tbls()
  
  tmp <- left_join(subset(bills, bills$item %in% tbl_df(beer)[[1]]), 
                   tsns, 
                   by = "transaction_id") %>% 
    select(drinker_name, item, quantity) %>% 
    group_by(drinker_name, item) %>%
    mutate(count = sum(quantity)) %>% 
    group_by(count, add = T) %>% 
    summarise() %>% 
    ungroup() %>% 
    arrange(desc(count)) %>% 
    top_n(n, count)
  
  p <- tmp %>%
    ggplot(aes(x = reorder(drinker_name, count), y = count)) +
    geom_bar(stat = "identity") +
    labs(title = "Top Drinkers Who Drink Your Beer(s)",
         caption = "top drinkers calculated by total sales volume",
         x = "drinker name",
         y = "total sales count") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(plot.title = element_text(hjust = 0.5))
  
  rm(tmp)
  
  return(p)
  
}

# given a beer, show time distribution of when this beer sells the most
g_beers_toptimeseries <- function(beer, n) {
  
  ensure_tbls()
  
  # TODO
  
}

# ========== ========== drinkers ========== ========== #

t_drinkers_lookup <- function(drinker) {
  
  ensure_tbls()
  
  tmp <- subset(drinkers, grepl(tbl_df(drinker)[[1]], drinkers$name)) %>% 
    group_by(name) %>% 
    arrange(desc(name))
  
  return(tmp)
  
}

# given a drinker, show all his/her transactions ordered by time and grouped by different bars
# requires: ..., transactions
t_drinkers_tsns <- function(drinker) {
  
  ensure_tbls()
  
  tmp <- subset(tsns, tsns$drinker_name %in% tbl_df(drinker)[[1]]) %>% 
    group_by(bar_name) %>% 
    arrange(bar_name, transaction_time)
  
  return(tmp)
  
}

# given a transaction_id, show all items on that bill
# requires: bills
t_drinkers_tid <- function(t_id) {
  
  ensure_tbls()
  
  return(bills %>% subset(bills$transaction_id %in% tbl_df(t_id)[[1]]))
  
}

# show bar graph of beers he/she orders the most
# requires: beers, bills, transactions
g_drinkers_beers <- function(drinker, n) {
  
  ensure_tbls()
  
  tmp <- bills %>%
    subset(bills$transaction_id %in% subset(tsns, tsns$drinker_name %in% tbl_df(drinker)[[1]])$transaction_id) %>%
    group_by(item) %>%
    count() %>%
    ungroup()
  
  colnames(tmp) <- c("name", "count")
  
  tmp <- tmp %>% 
    arrange(desc(count)) %>% 
    top_n(n, count)
  
  p <- tmp %>%
    ggplot(aes(x = reorder(name, count), y = count)) +
    geom_bar(stat = "identity") +
    labs(title = "Your Drinker(s)'s Favorite Beers",
         caption = "favorite calculated by total sales volume",
         x = "drinker name",
         y = "total sales volume") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(plot.title = element_text(hjust = 0.5))
  
  rm(tmp)
  
  return(p)
}

# given a drinker, bar graph of his/her spending in different bars on different dates/weeks/months.
# requires: ..., transactions
g_drinkers_timeseries <- function(drinker) {
  
  ensure_tbls()
  
  # TODO
  
}
