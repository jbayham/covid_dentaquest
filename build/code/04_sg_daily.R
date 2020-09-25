#This script expands the daily visit data


###########################################

to_expand <- dir("build/cache/weekly/",full.names = T) %>%
  future_map_dfr(function(x){
    temp <- read_parquet(x,
                         col_select = c("safegraph_place_id","date_start","visits_by_day")
                         ) 
  })  

if(!dir.exists("build/cache/daily")){
  dir.create("build/cache/daily",recursive = T)
}

date_exclude <- dir("build/cache/daily/",pattern = "daily") %>% 
  str_remove("daily_") %>%
  ymd()




#Exploding columns
to_expand %>%
  filter(!(date_start %in% date_exclude)) %>%
  group_split(date_start) %>%
  future_map(function(df_date){
    temp_out <- expand_integer_json(as.data.table(df_date),expand = "visits_by_day",by=F,fast = T)
    
    out <- inner_join(temp_out,
                      df_date %>%
                        select(-visits_by_day) %>%
                        mutate(initial_rowno=row_number())
                      ) %>%
      mutate(date=date_start+index-1) %>%
      select(safegraph_place_id,date,value=visits_by_day)
    
    
    write_parquet(out,sink = str_c("build/cache/daily/daily_",out$date[1],".parquet"))
    
    return(NULL)
  })



