Cluster_Portfolio <- function(Cap, monthly, cluster){ 
  
  wts <- Cap %>% 
    tbl2xts::tbl_xts(cols_to_xts = weight, spread_by = stock)
  
  rts <- monthly %>% 
    filter(stock %in% unique(monthly$stock)) %>% 
    tbl2xts::tbl_xts(cols_to_xts = rtn, spread_by = stock)
  
  wts[is.na(wts)] <- 0
  rts[is.na(rts)] <- 0
  
  Idx <- rmsfuns::Safe_Return.portfolio(R = rts, weights = wts, lag_weights = TRUE) %>% 
    tbl2xts::xts_tbl() %>% 
    rename(!!paste0("comp_rtn_", cluster) := portfolio.returns)
  
  Idx
}