
# Custom functions for R Basics ----

# Libraries
# pacman::p_load(tidyverse,tidyquant,dygraphs,rvest)
# Try it out
# c("AAPL","MSFT","NFLX") %>% 
#   tq_get(from = "2015-01-01") %>% 
#   select(symbol, date, adjusted) -> stock_tbl
# stock_tbl

stock_tbl_to_xts <- function(your_stock_tbl,stock_symbol){
  your_stock_tbl %>% 
    filter(symbol == stock_symbol) %>% 
    select(date,adjusted) %>% 
    column_to_rownames("date") %>% 
    as.xts()
}

# Yep it works
# stock_tbl_to_xts(stock_tbl,"AAPL") %>% dygraph() %>% 
#   dySeries("adjusted", label = "AAPL") %>% 
#   dyRangeSelector()


my_scrape <- function(your_url,integer_vector,the_node){
  integer_vector %>% 
    map_df(.f = function(X){
      your_url %>% 
        httr::GET(config = httr::config(ssl_verifypeer = FALSE)) %>% 
        read_html() %>% 
        html_nodes(the_node) %>% 
        .[[X]] %>% 
        # Listify, then unlistify
        xml2::as_list() %>% unlist() %>% 
        as_tibble() #%>% 
      # Remove debris rows 
      # filter(! (str_detect(value,"^\\,") | str_detect(value,"^\n")) )
    }) 
}


my_jitter_coords <- function(coords_tbl,sd=.01){
  coords_tbl %>% unite("location",c(lat,lng),remove = F) -> coords_tbl
  
  coords_tbl %>% 
    count(location) %>% 
    filter(n > 1) -> coords_identical
  
  coords_tbl %>% 
    filter(location %in% coords_identical$location) %>% 
    rowwise() %>% 
    mutate(
      lat = lat + rnorm(1,sd=sd),
      lng = lng + rnorm(1,sd=sd)
    ) %>% 
    bind_rows(
      coords_tbl %>% filter(!(location %in% coords_identical$location))
    ) -> coords_tbl
  
  return(coords_tbl)
}



