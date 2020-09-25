#This script expands the bucketed dwell time data




###########################################

to_expand <- dir("build/cache/weekly/",full.names = T) %>%
  future_map_dfr(function(x){
    temp <- read_parquet(x,
                         col_select = c("safegraph_place_id","date_start","bucketed_dwell_times")
                         ) 
  })  

if(!dir.exists("build/cache/dwell")){
  dir.create("build/cache/dwell",recursive = T)
}

date_exclude <- dir("build/cache/dwell/",pattern = "dwell") %>% 
  str_remove("dwell_") %>%
  ymd()




#Exploding columns
to_expand %>%
  filter(!(date_start %in% date_exclude)) %>%
  group_split(date_start) %>%
  future_map(function(df_date){
    temp_out <- expand_cat_json(as.data.table(df_date),expand = "bucketed_dwell_times",by=F,fast = T)
    
    out <- inner_join(temp_out,
                      df_date %>%
                        select(-bucketed_dwell_times) %>%
                        mutate(initial_rowno=row_number())
                      ) %>%
      select(safegraph_place_id,date_start,dwell=index,value=bucketed_dwell_times)
    
    
    write_parquet(out,sink = str_c("build/cache/dwell/dwell_",out$date[1],".parquet"))
    
    return(NULL)
  })





