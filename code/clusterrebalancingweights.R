cluster_rebalancing_weights <- function(df, months_to_rebalance, Day){
  
  # lets define our rebalancing time 
  
  Rebalance_Days <- 
    df %>% 
    mutate(Year = format(date, "%Y"), Month = format(date, "%b"), Day = format(date, "%a")) %>% 
    filter(Month %in% months_to_rebalance ) %>% 
    select(date, Year,  Month, Day ) %>% unique() %>% 
    group_by(Month) %>% 
    filter(Day == Day) %>% 
    group_by(Year, Month) %>% 
    filter(date == first(date)) %>% 
    pull(date)
  
  # lets create the weights for the capping, here we assume that each cluster has is equally weighted and lets trim down to 5 assets per cluster for equal comparison 
  
  rebalance_weights <- df %>%
    filter(date %in% Rebalance_Days) %>% 
    mutate(YM = format(date, "%Y %b")) %>% 
    group_by(YM, cluster) %>% 
    mutate(weight = 1/n()) %>%
    ungroup() %>% 
    select(date, stock, weight, px, cluster, YM)
  
  rebalance_weights
}
