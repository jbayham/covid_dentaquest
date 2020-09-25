#This script extracts the same day visit data for dental offices

plan(multisession(workers = 2))

#VROOM_TEMP_PATH
Sys.setenv(VROOM_TEMP_PATH = "F:/tmp")

#Convenience function
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

load("build/cache/poi_subsets.Rdata")
############################################ 
#Extract weekly patterns
weekly_list <- dir("build/inputs/weekly",full.names = T) 

#############
#Need to modify to select certain dates
#weekly_list <- weekly_list[86,]
#############


if(!dir.exists("build/cache/weekly")){
  dir.create("build/cache/weekly",recursive = T)
}



#Reading data
weekly_list %>%
  future_map(function(x){
    message(x)
    
    #Read in data
    out <- vroom(x, 
                 col_select = c(safegraph_place_id,date_range_start,date_range_end,raw_visit_counts,raw_visitor_counts,poi_cbg,
                                visits_by_day,visits_by_each_hour,visitor_daytime_cbgs,
                                distance_from_home,median_dwell,bucketed_dwell_times,related_same_day_brand,
                                related_same_week_brand)) 
    
    out <- out %>%
      mutate(date_start=getmode(as_date(date_range_start)),
             date_end=getmode(as_date(date_range_end))) %>%
      select(-starts_with("date_range")) %>%
      inner_join(poi_subset %>% select(safegraph_place_id)) #Subset based on criteria NAICS dentist
    
    file_name <- str_c("weekly_",min(out$date_start))
    
    write_parquet(out,str_c("build/cache/weekly/",file_name,".parquet"))
    
    return(NULL) 
  },.progress = T)




######################################################
#Extract home summary files for device counts
load("analysis/cache/home_summary.Rdata")
#Reading data
home_app <- dir("build/inputs/home_summary/",full.names = T) %>%
  future_map_dfr(function(x){
    out <- vroom(x, 
                 col_select = c(census_block_group,number_devices_residing,date_range_start,date_range_end)) %>%
      mutate_at(vars(date_range_start,date_range_end),~getmode(as_date(.)))
    
    return(out) 
  })

home_summary <- bind_rows(home_summary,home_app) %>%
  distinct()

#home_summary <- distinct(home_summary)
unique(home_summary$date_range_start)

save(home_summary,file = "analysis/cache/home_summary.Rdata")

###################################################

load("analysis/cache/normalization.Rdata")

#Construct normalization data
norm_app <- dir("build/inputs/normalization/",full.names = T) %>%
  map_dfr(vroom) %>% 
  mutate(date=ymd(str_c(year,month,day,sep = "-"))) %>%
  group_by(year,month,day,date) %>%
  summarize_at(vars(total_visits:total_home_visitors),sum) %>%
  ungroup()

normalization <- bind_rows(normalization,norm_app) %>%
  distinct()

unique(normalization$date)

save(normalization,file = "analysis/cache/normalization.Rdata")


################################################################
#temp dataset containing poi cbg
poi_ref <- dir("build/cache/weekly",full.names = T)[1] %>%
  future_map_dfr(function(x){
    message(x)
    temp <- read_parquet(x,col_select = c("safegraph_place_id","poi_cbg"))
    }) %>%
  distinct() %>%
  inner_join(poi_subset %>% select(safegraph_place_id,parent_safegraph_place_id,state=region,zip=postal_code)) %>%
  mutate(fips=str_sub(poi_cbg,1,5))

save(poi_ref,file = "analysis/cache/poi_ref.Rdata")

