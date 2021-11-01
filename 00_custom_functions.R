
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


horizontal_lollipop <- function(your_tibble,x_var,y_var,my_color="skyblue"){
  your_tibble %>% 
    ggplot(aes({{x_var}},{{y_var}})) + 
    
    geom_segment(
      aes(x = 0        ,xend = {{x_var}},
          y = {{y_var}},yend = {{y_var}}),
      color = my_color
    ) + 
    geom_point(color = my_color) + 
    hrbrthemes::theme_ipsum(grid = "X")
}


tidied_model_glm <- function(model_fit){
  model_fit %>% 
    tidy(exponentiate = T,conf.int = T) %>% 
    mutate(p.value = pvalue(p.value)) %>% 
    filter(!grepl("Intercept",term)) %>% 
    select(-c(std.error,statistic)) %>% 
    mutate(
      term = factor(term) %>% fct_reorder(estimate) %>% fct_rev()
    )
}


tidied_model_lm <- function(model_fit){
  model_fit %>% 
    tidy(conf.int = T) %>% 
    mutate(p.value = pvalue(p.value)) %>% 
    filter(!grepl("Intercept",term)) %>% 
    select(-c(std.error,statistic)) %>% 
    mutate(
      term = factor(term) %>% fct_reorder(estimate) %>% fct_rev()
    )
}


my_bold_plus <- function(your_gt,fmt_number=T,decimals = 2){
  if(isTRUE(fmt_number)){
    your_gt %>% 
      tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_column_labels()
      ) %>% 
      fmt_number(
        columns = where(is.numeric),
        decimals = decimals
      )
  } else{
    your_gt %>% 
      tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_column_labels()
      )
  }
}


plot_dotwhisker <- function(
  tidied_model,
  round_estimate = 3,
  my_title,
  x_axis = "Estimate of Linear Association (and 95% CI)",
  x_intercept = 0,
  n_breaks = 10,
  labels = T
){
  if(labels){
    tidied_model %>% 
      ggplot(aes(estimate,term)) +
      geom_vline(xintercept = x_intercept,
                 lty   = 2,
                 color = "gray80",
                 size  = 1.3) +
      
      geom_point() +
      geom_segment(aes(
        x = conf.low, xend = conf.high,
        y = term    , yend = term
      )) +
      ggrepel::geom_label_repel(aes(
        x = estimate,
        label = str_c(
          round(estimate,round_estimate)," (",round(conf.low,round_estimate),", ",round(conf.high,round_estimate),")",
          sep = ""
        )
      ),
      size = 2) + 
      
      theme_test() +
      scale_x_continuous(breaks = pretty_breaks(n=n_breaks)) +
      labs(title = str_glue("{my_title}"),
           x = str_glue("{x_axis}"), y = "")
  } else{
    tidied_model %>% 
      ggplot(aes(estimate,term)) +
      geom_vline(xintercept = x_intercept,
                 lty   = 2,
                 color = "gray80",
                 size  = 1.3) +
      
      geom_point() +
      geom_segment(aes(
        x = conf.low, xend = conf.high,
        y = term    , yend = term
      )) +
      
      theme_test() +
      scale_x_continuous(breaks = pretty_breaks(n=n_breaks)) +
      labs(title = str_glue("{my_title}"),
           x = str_glue("{x_axis}"), y = "")
  }
}




