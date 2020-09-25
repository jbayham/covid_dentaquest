#This script expands the same day related brands data


plan(multisession(workers = 2))
###########################################


to_expand <- dir("build/cache/weekly/",full.names = T) %>%
  future_map_dfr(function(x){
    temp <- read_parquet(x,col_select = c("safegraph_place_id","date_start","related_same_day_brand")) %>%
     filter(related_same_day_brand != "{}")
  })  

if(!dir.exists("build/cache/same_day")){
  dir.create("build/cache/same_day",recursive = T)
}

date_exclude <- dir("build/cache/same_day/",pattern = "same_day") %>% 
  str_remove("same_day_") %>%
  ymd()


#Exploding columns
to_expand %>%
  filter(!(date_start %in% date_exclude)) %>%
  group_split(date_start) %>%
  future_map(function(df_date){
    temp_out <- expand_cat_json(as.data.table(df_date),expand = "related_same_day_brand",by=F)
    
    out <- inner_join(temp_out,
                      df_date %>%
                        select(-related_same_day_brand) %>%
                        mutate(initial_rowno=row_number())) %>%
      select(safegraph_place_id,date_start,sd_brand=index,value=related_same_day_brand)
        
    
    write_parquet(out,sink = str_c("build/cache/same_day/same_day_",out$date_start[1],".parquet"))
    
    return(NULL)
  })

