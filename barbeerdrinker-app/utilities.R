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
  
  query <- gsub("‘", "'", query)
  query <- gsub("’", "'", query)
  
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
    
    query <- gsub("‘", "'", query)
    query <- gsub("’", "'", query)
    
    tmp <- tbl_df(dbGetQuery(pool, query))
    
    poolClose(pool)
    rm(tmp, pool)
    
    return("success")
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
  if (!require(markdown))       { install.packages("markdown");       require(markdown)       }
  if (!require(tidyverse))      { install.packages("tidyverse");      require(tidyverse)      }
  if (!require(pool))           { install.packages("pool");           require(pool)           }
  if (!require(RMySQL))         { install.packages("RMySQL");         require(RMySQL)         }
  if (!require(timevis))        { install.packages("timevis");        require(timevis)        }
  if (!require(shiny))          { install.packages("shiny");          require(shiny)          }
  if (!require(shinydashboard)) { install.packages("shinydashboard"); require(shinydashboard) }
  if (!require(shinyjs))        { install.packages("shinyjs");        require(shinyjs)        }
  if (!require(shinythemes))    { install.packages("shinythemes");    require(shinythemes)    }
  if (!require(shinyWidgets))   { install.packages("shinyWidgets");   require(shinyWidgets)   }
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

# ensures the existence of bar, beer, drinker for debug
ensure_dev_tbls <- function() {
  
  ensure_tbls()
  
  if (!exists("bar"))     { bar <<- bars$name[1:10] }
  if (!exists("beer"))    { beer <<- beers$name[1:10] }
  if (!exists("drinker")) { drinker <<- drinkers$name[1:25] }
  
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
g_bars_topspenders <- function(bar, n, clr) {
  
  ensure_tbls()
  
  # computes top drinkers
  tmp <- 
    tsns %>%
    filter(bar_name %in% tbl_df(bar)[[1]]) %>% 
    group_by(drinker_name) %>% 
    mutate(drinker_total = sum(total)) %>% 
    group_by(drinker_total, add = T) %>% 
    summarise() %>% 
    ungroup() %>% 
    top_n(n, drinker_total) %>% 
    select(drinker_name)
  
  # computes $ amount spent per bar
  tmp <- 
    tsns %>% 
    filter(drinker_name %in% tmp$drinker_name) %>% 
    filter(bar_name %in% tbl_df(bar)[[1]]) %>% 
    group_by(drinker_name, bar_name) %>% 
    summarise(drinker_total = sum(total)) %>% 
    ungroup()
  
  fip <- factor(tmp$drinker_name, levels = unique(tmp$drinker_name))
  tmp$drinker_name <- fip
  tmp$drinker_name <- reorder(tmp$drinker_name, tmp$drinker_total)
  
  # plots output
  p <- 
    tmp %>%
    ggplot(aes(x = drinker_name, y = drinker_total)) +
    labs(title = "Drinkers Who Are Largest Spenders",
         caption = "spenders calculated by $ amount spent",
         x = "drinker name",
         y = "total expenditures ($)") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(plot.title = element_text(hjust = 0.5))
  
  rm(tmp)
  
  if (clr == "YES") {
    return(p + geom_bar(stat = "identity",
                        aes(fill = bar_name)))
  } else {
    return (p + geom_bar(stat = "identity"))
  }
  
}

# bar graph for beers which are most popular
# requires: beers, bills, tsns
g_bars_topbeers <- function(bar, n, clr) {
  
  ensure_tbls()
  
  # computes most popular beers
  tmp <- 
    left_join(bills, tsns, by = "transaction_id") %>% 
    filter(bar_name %in% tbl_df(bar)[[1]]) %>% 
    filter(item %in% beers$name) %>%
    group_by(item) %>%
    mutate(sales = sum(quantity*price)) %>%
    group_by(sales, add = T) %>%
    summarise() %>%
    ungroup() %>% 
    top_n(n)
  
  # assigns bar distributions of beer sales
  tmp <- 
    left_join(bills, tsns, by = "transaction_id") %>% 
    filter(item %in% tmp$item) %>% 
    filter(bar_name %in% tbl_df(bar)[[1]]) %>% 
    group_by(item, bar_name) %>% 
    summarise(item_total = sum(total)) %>% 
    ungroup() %>% 
    select(item,
           bar_name,
           sales = item_total)
  
  fip <- factor(tmp$item, levels = unique(tmp$item[order(tmp$sales)]), ordered = T)
  tmp$item <- fip
  #tmp$item <- reorder(tmp$item, tmp$sales)
    
  # plots output
  p <- 
    tmp %>%
    ggplot(aes(x = item, y = sales)) +
    labs(title = "Most Popular Beers",
         caption = "popularity calculated by total sales in dollars",
         x = "beer name",
         y = "total sales count") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(plot.title = element_text(hjust = 0.5))
  
  rm(tmp)
  
  if (clr == "YES") {
    return(p + geom_bar(stat = "identity",
                        aes(fill = bar_name)))
  } else {
    return (p + geom_bar(stat = "identity"))
  }
  
}

# bar graph for manufacturers who sell the most beers
# requires: beers, bills
g_bars_topmanfs <- function(bar, n, clr) {
  
  ensure_tbls()
  
  # computes total beer sales per bar
  tmp <- 
    left_join(bills, tsns, by = "transaction_id") %>% 
    filter(bar_name %in% tbl_df(bar)[[1]]) %>% 
    filter(item %in% beers$name) %>%
    group_by(item, bar_name) %>%
    mutate(sales = sum(quantity*price)) %>%
    group_by(sales, add = T) %>%
    summarise() %>%
    ungroup() %>% 
    select(name = item,
           bar_name,
           sales) %>% 
      
    # computes total manufacturer sales per bar
    left_join(beers, by = "name") %>%
    select(manf,
           bar_name, 
           sales) %>% 
    group_by(manf, bar_name) %>% 
    mutate(total = sum(sales)) %>% 
    select(-sales) %>% 
    group_by(total, add = T) %>% 
    summarise()
  
  # side-calculation of top performing manf's
  tmp2 <- 
    tmp %>% 
    group_by(manf) %>% 
    summarise(total = sum(total)) %>% 
    arrange(desc(total)) %>% 
    top_n(n)
    
  # merges top performing manf's with bar sales
  tmp <- 
    tmp %>% 
    filter(manf %in% tmp2$manf) %>% 
    left_join(tmp2, by = "manf") %>% 
    select(name = manf, 
           bar_name, 
           btotal = total.x, 
           sales = total.y)
  
  # plots output
  p <- 
    tmp %>%
    arrange(desc(sales)) %>% 
    ggplot(aes(x = name, y = sales)) +
    labs(title = "Most Popular Manufacturers",
         caption = "popularity calculated by total sales in dollars",
         x = "manufacturer name",
         y = "total sales ($)") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(plot.title = element_text(hjust = 0.5))
  
  rm(tmp)
  
  if (clr == "YES") {
    return(p + geom_bar(stat = "identity",
                        aes(fill = bar_name)))
  } else {
    return (p + geom_bar(stat = "identity"))
  }
  
}

# wrapper for g_b_t_day and g_b_t_week
g_bars_timeseries <- function(bar, n, clr, status) {
  
  if (status == "day") {
    return(g_bars_timeseries_day(bar, n, clr))
  } else if (status == "week") {
    return(g_bars_timeseries_week(bar, n, clr))
  } else {
    return ("error lol")
  }
  
}

# demonstrate time distribution of sales, show what are the busiest periods of the day and of the week
# requires: transactions
g_bars_timeseries_day <- function(bar, n, clr) {
  
  tmp <- 
    tsns %>% 
    filter(bar_name %in% tbl_df(bar)[[1]]) %>% 
    mutate(datetime = lubridate::ymd_hms(paste(date, time))) %>% 
    select(datetime) %>% 
    group_by(hour = lubridate::hour(datetime)) %>% 
    count() %>% 
    ungroup() %>% 
    mutate(datetime = lubridate::ymd_hms(paste("2019-01-01", paste0(hour, ":00:00")))) %>% 
    select(datetime,
           totaltsns = n)
  
  p <- 
    tmp %>% 
    ggplot(aes(x = datetime, y = totaltsns)) +
    geom_bar(stat = "identity") +
    scale_x_datetime(date_labels = "%H:%M") +
    labs(title = "Timeseries of Most Popular Hours",
         caption = "popular calculated by total transactions / hour",
         x = "hour",
         y = "total transactions count") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(plot.title = element_text(hjust = 0.5))
  
  rm(tmp)
  
  return(p)
  
}

g_bars_timeseries_week <- function(bar, n, clr) {
  
  tmp <- 
    tsns %>% 
    filter(bar_name %in% tbl_df(bar)[[1]]) %>% 
    mutate(datetime = lubridate::ymd_hms(paste(date, time))) %>% 
    select(datetime) %>% 
    group_by(week = lubridate::wday(datetime, label = T)) %>% 
    count() %>% 
    ungroup() %>% 
    select(week,
           totaltsns = n)
  
  p <- 
    tmp %>% 
    ggplot(aes(x = week , y = totaltsns)) +
    geom_bar(stat = "identity") +
    labs(title = "Timeseries of Most Popular Days of the Week",
         caption = "popular calculated by total transactions / day",
         x = "day",
         y = "total transactions count") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(plot.title = element_text(hjust = 0.5))
  
  rm(tmp)
  
  return(p)
  
}


# ========== ========== beers ========== ========== #

# given a beer, show bars where this beer sells the most (again only top)
# requires: bills, transactions
g_beers_topbars <- function(beer, n, clr) {
  
  ensure_tbls()
  
  # compute top bar (sellers) of this beer
  tmp <- 
    left_join(filter(bills, item %in% tbl_df(beer)[[1]]), 
                   tsns,
                   by = "transaction_id") %>% 
    select(bar_name, item, quantity, price) %>% 
    group_by(bar_name, item) %>% 
    mutate(sales = sum(quantity*price)) %>% 
    group_by(sales, add = T) %>% 
    summarise() %>% 
    
    group_by(bar_name, add = F) %>% 
    mutate(total = sum(sales)) %>% 
    group_by(total, add = T) %>% 
    summarise() %>% 
    ungroup() %>% 
    top_n(n)
  
  # compute beer sales at these bars
  tmp <- 
    left_join(filter(bills, item %in% tbl_df(beer)[[1]]), 
                    tsns,
                    by = "transaction_id") %>% 
    filter(bar_name %in% tmp$bar_name) %>% 
    select(item, bar_name, quantity, price) %>% 
    group_by(item, bar_name) %>% 
    summarise(sales = sum(quantity*price))
  
  # plot output
  p <- 
    tmp %>%
    arrange(desc(sales)) %>% 
    ggplot(aes(x = bar_name, y = sales)) +
    labs(title = "Top Bars Which Sell Your Beer(s)",
         caption = "top bars calculated by total sales in dollars for beers in list",
         x = "bar name",
         y = "total sales count") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(plot.title = element_text(hjust = 0.5))
  
  rm(tmp)
  
  if (clr == "YES") {
    return(p + geom_bar(stat = "identity",
                        aes(fill = item)))
  } else {
    return (p + geom_bar(stat = "identity"))
  }
  
}

# given a beer, show also drinkers who are the biggest consumers of this beer
# requires: bills, transactions
g_beers_topdrinkers <- function(beer, n, clr) {
  
  ensure_tbls()
  
  # compute drinkers order # per bar per drink
  tmp <- 
    left_join(filter(bills, item %in% tbl_df(beer)[[1]]), 
                   tsns, 
                   by = "transaction_id") %>% 
    select(drinker_name, item, quantity) %>% 
    group_by(drinker_name, item) %>%
    mutate(count = sum(quantity)) %>% 
    group_by(count, add = T) %>% 
    summarise() %>% 
    group_by(drinker_name, add = F) %>%   # resets groups
    mutate(total = sum(count)) %>% 
    ungroup()
  
  # get top drinkers
  tmp2 <- 
    tmp %>%
    group_by(drinker_name, total) %>% 
    summarise() %>%
    ungroup() %>% 
    top_n(n)
  
  # filter top drinkers
  tmp <- 
    tmp %>% 
    filter(drinker_name %in% tmp2$drinker_name)
  
  # plot output
  p <- 
    tmp %>%
    arrange(desc(total)) %>% 
    ggplot(aes(x = drinker_name, y = total)) +
    labs(title = "Top Drinkers Who Drink Your Beer(s)",
         caption = "top drinkers calculated by total sales volume",
         x = "drinker name",
         y = "total sales count") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(plot.title = element_text(hjust = 0.5))
  
  rm(tmp)
  
  if (clr == "YES") {
    return(p + geom_bar(stat = "identity",
                        aes(fill = item)))
  } else {
    return (p + geom_bar(stat = "identity"))
  }
  
}

# given a beer, show time distribution of when this beer sells the most
g_beers_timeseries <- function(beer, n, clr) {
  
  ensure_tbls()
  
  # beer sells most = most 
  
  tmp <- 
    left_join(bills, 
            tsns, 
            by = "transaction_id") %>% 
    filter(item %in% tbl_df(beer)[[1]]) %>% 
    select(item, quantity, time, date) %>% 
    mutate(datetime = lubridate::ymd_hms(paste(date, time))) %>% 
    select(item, quantity, datetime) %>% 
    group_by(hour = lubridate::hour(datetime)) %>% 
    mutate(totalsales = sum(quantity)) %>% 
    ungroup() %>% 
    mutate(time = lubridate::ymd_hms(paste("2019-01-01", paste0(hour, ":00:00", sep = "")))) %>% 
    group_by(item, time, totalsales) %>% 
    summarise() %>% 
    ungroup()
  
  p <- 
    tmp %>% 
    ggplot(aes(x = time, y = totalsales)) +
    scale_x_datetime(date_labels = "%H:%M") +
    labs(title = "Timeseries of When Your Beer is Most Popular (over all time)",
         caption = "popular calculated by transactions / hour",
         x = "hour",
         y = "total transactions") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(plot.title = element_text(hjust = 0.5))
  
  rm(tmp)
  
  if (clr == "YES") {
    return(p + geom_bar(stat = "identity",
                        aes(fill = item)))
  } else {
    return (p + geom_bar(stat = "identity"))
  }
  
}

# ========== ========== drinkers ========== ========== #

# given a drinker (usually a list), output table of drinkers
# requires: drinkers
t_drinkers_lookup <- function(drinker) {
  
  ensure_tbls()
  
  if (nchar(drinker) == 0) {
    tmp <- drinkers %>% 
      group_by(name) %>% 
      arrange(name)
    return(tmp)
  } else {
  tmp <- drinkers %>% 
    filter(grepl(drinker, name, ignore.case = T)) %>% 
    group_by(name) %>% 
    arrange(name)
    return(tmp)
  }
  
}

# given a drinker, show all his/her transactions ordered by time and grouped by different bars
# requires: ..., transactions
t_drinkers_tsns <- function(drinker) {
  
  ensure_tbls()
  
  # filter and arrange output
  tmp <- filter(tsns, drinker_name %in% tbl_df(drinker)[[1]]) %>% 
    group_by(drinker_name, bar_name) %>% 
    arrange(drinker_name, bar_name, date, time)
  
  return(tmp)
  
}

# given a transaction_id, show all items on that bill
# requires: bills
t_drinkers_tid <- function(t_id) {
  
  ensure_tbls()
  
  return(bills %>% filter(transaction_id %in% tbl_df(t_id)[[1]]))
  
}

# given a drinker, show bar graph of beers he/she orders the most (the top)
# requires: beers, bills, transactions
g_drinkers_beers <- function(drinker, n, clr) {
  
  ensure_tbls()
  
  # compute total sales per drinker per beer
  tmp <- 
    left_join(bills, tsns, by = "transaction_id") %>% 
    filter(drinker_name %in% tbl_df(drinker)[[1]]) %>% 
    filter(item %in% beers$name) %>% 
    select(drinker_name,
           item,
           quantity) %>% 
    group_by(drinker_name, item) %>% 
    summarise(sales = sum(quantity)) %>% 
    mutate(total = sum(sales)) %>% 
    ungroup()
  
  # side-compute: find top beers
  tmp2 <- 
    tmp %>% 
    group_by(item) %>% 
    summarise(total = sum(sales)) %>% 
    ungroup() %>% 
    top_n(n)
  
  # filter out non-top beers
  tmp <- 
    tmp %>%
    filter(item %in% tmp2$item)
  
  # plot output
  p <- 
    tmp %>%
    arrange(desc(total)) %>% 
    ggplot(aes(x = item, y = total)) +
    labs(title = "Your Drinker(s)'s Favorite Beers",
         caption = "favorite calculated by total sales volume",
         x = "beer name",
         y = "total sales volume") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(plot.title = element_text(hjust = 0.5))
  
  rm(tmp)
  
  if (clr == "YES") {
    return(p + geom_bar(stat = "identity",
                        aes(fill = drinker_name)))
  } else {
    return (p + geom_bar(stat = "identity"))
  }
}

# given a drinker, bar graph of his/her spending in different bars on different dates/weeks/months.
# requires: ..., transactions
g_drinkers_timeseries <- function(drinker, clr) {
  
  ensure_tbls()
  
  tmp <- 
    tsns %>% 
    filter(drinker_name %in% tbl_df(drinker)[[1]]) %>% 
    group_by(drinker_name, bar_name, date) %>% 
    summarise(total = sum(total))
  
  tmp$date <- as.Date(tmp$date)
  
  p <- 
    tmp %>% 
    ggplot(aes(x = date, y = total)) +
    labs(title = "Your Drinker(s)'s Spending Patterns",
         caption = "",
         x = "day",
         y = "total sales ($)") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(plot.title = element_text(hjust = 0.5))
    
  rm(tmp)
  
  if (clr == "YES") {
    return(p + geom_bar(stat = "identity",
                        aes(fill = bar_name)))
  } else {
    return (p + geom_bar(stat = "identity"))
  }
  
}
