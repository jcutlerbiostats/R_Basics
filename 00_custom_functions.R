
# Custom functions for R Basics ----

# Libraries
# pacman::p_load(tidyverse,tidyquant,dygraphs)
# Try it out
# c("AAPL","MSFT","NFLX") %>% 
#   tq_get(from = "2015-01-01") %>% 
#   select(symbol, date, adjusted) -> stock_tbl
# 
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








