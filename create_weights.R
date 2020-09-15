#create weights 

#target weight
hake_wt <- 1
#bycatch weights
rfish_wt <- 0.5
sal_wt <- 0.5

wts <- matrix(c("hake", "",
             hake_wt, "",
             "rockfish", "salmon",
             rfish_wt, sal_wt), ncol = 2, byrow = TRUE) %>% 
  as.data.frame

write_csv(wts, paste0(out_drive, "ashop_weights.csv"), 
          col_names = FALSE)