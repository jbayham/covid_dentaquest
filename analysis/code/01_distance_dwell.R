#This script produces the weekly distance and dwell figures.

load("build/cache/poi_subsets.Rdata")
load("analysis/cache/cdc.Rdata")

isnt_out_z <- function(x, thres = 3, na.rm = TRUE) {
  abs(x - mean(x, na.rm = na.rm)) <= thres * sd(x, na.rm = na.rm)
}

#####################################################
#Distance traveled

weekly <- dir("build/cache/weekly/",full.names = T) %>%
  future_map_dfr(function(x){
    temp <- read_parquet(x,
                         col_select = c("safegraph_place_id","date_start","distance_from_home","raw_visit_counts","poi_cbg")
    ) 
  })  

weekly_distance <- as.data.table(weekly) %>% 
  merge.data.table(.,as.data.table(poi_subset)[str_detect(top_category,"Dentist"),.(safegraph_place_id)],by = c("safegraph_place_id")) %>%
  .[!is.na(distance_from_home) | isnt_out_z(distance_from_home,2),  #checked for influence of outliers 3 sd away from mean
    .(distance=weighted.mean(distance_from_home,raw_visit_counts)/1609),
    #.(distance=mean(distance_from_home)/1609)#,
    by=.(date=date_start)
    ]

fwrite(weekly_distance,file = "outputs/data_share/weekly_distance.csv")

max_date <- max(weekly_distance$date)

weekly_distance %>%
  filter(year(date)>2018) %>%
  mutate(year=year(date),
         # date=as_date(ifelse(year<2020,date+365,date))) %>%
         date=as_date(str_replace(date,"2019","2020"))) %>%
  filter(date<max_date) %>%
  ggplot(.,aes(x=date,y=distance,color=factor(year))) +
  #geom_col(width = 0) +
  #geom_line(alpha=.3) +
  #geom_smooth(method = "gam",formula = y ~ splines::bs(x,3)) +
  geom_smooth() +
  theme_classic(base_size = 15) +
  scale_color_manual(name="",values = dq_colors) +
  scale_x_date(date_breaks = "1 month",date_labels = "%b") +
  scale_y_continuous(labels = scales::comma) +
  labs(x="",y="",title = "Average Miles Traveled to Dentist Office")

ggsave("outputs/distance_national.png",width = 6,height = 4,units = "in")
ggsave("outputs/distance_national.pdf",width = 6,height = 4,units = "in")

#######################
weekly_distance <- as.data.table(weekly) %>%
  merge.data.table(.,as.data.table(poi_subset)[str_detect(top_category,"Dentist"),.(safegraph_place_id)],by = c("safegraph_place_id")) %>%
  .[!is.na(distance_from_home),
    .(distance=weighted.mean(distance_from_home,raw_visit_counts)/1609),
    #.(distance=mean(distance_from_home)),
    by=.(date=date_start,state=str_sub(poi_cbg,1,2))] %>%
  inner_join(.,states,by=c("state"="state_fips"))

fwrite(weekly_distance,file = "outputs/data_share/weekly_distance_state.csv")
  
max_date <- max(weekly_distance$date)

weekly_distance %>%
  filter(year(date)>2018,
         statename %in% state_subset) %>%
  mutate(year=year(date),
         # date=as_date(ifelse(year<2020,date+365,date))) %>%
         date=as_date(str_replace(date,"2019","2020"))) %>%
  filter(date<max_date) %>%
  ggplot(.,aes(x=date,y=distance,color=factor(year))) +
  #geom_col(width = 0) +
  #geom_line(alpha=.3) +
  #geom_smooth(method = "gam",formula = y ~ splines::bs(x,3)) +
  geom_smooth() +
  theme_classic(base_size = 15) +
  scale_color_manual(name="",values = dq_colors) +
  scale_y_continuous(labels = scales::comma) +
  labs(x="",y="",title = "Average Miles Traveled to Dentist Office") +
  facet_wrap(~statename,ncol = 3)

ggsave("outputs/distance_state.png",width = 7,height = 4,units = "in")


#######################
weekly_distance <- as.data.table(weekly) %>%
  merge.data.table(.,as.data.table(poi_subset)[str_detect(top_category,"Dentist"),.(safegraph_place_id)],by = c("safegraph_place_id")) %>%
  .[!is.na(distance_from_home),
    .(distance=weighted.mean(distance_from_home,raw_visit_counts)/1609),
    #.(distance=mean(distance_from_home)),
    by=.(date=date_start,fips_code=str_sub(poi_cbg,1,5))] %>%
  inner_join(cdc %>% select(fips_code,pop=county_2012_pop,metro)) %>%
  as.data.table() %>%
  .[,
    .(distance=weighted.mean(distance,pop)),
    by=.(date,metro)]

fwrite(weekly_distance,file = "outputs/data_share/weekly_distance_metro.csv")

max_date <- max(weekly_distance$date)

weekly_distance %>%
  filter(year(date)>2018) %>%
  mutate(year=year(date),
         # date=as_date(ifelse(year<2020,date+365,date))) %>%
         date=as_date(str_replace(date,"2019","2020"))) %>%
  filter(date<max_date) %>%
  ggplot(.,aes(x=date,y=distance,color=factor(year))) +
  #geom_col(width = 0) +
  #geom_line(alpha=.3) +
  #geom_smooth(method = "gam",formula = y ~ splines::bs(x,3)) +
  geom_smooth() +
  theme_classic(base_size = 15) +
  scale_color_manual(name="",values = dq_colors) +
  scale_y_continuous(labels = scales::comma) +
  labs(x="",y="",title = "Average Miles Traveled to Dentist Office") +
  facet_wrap(~metro,ncol = 2)

ggsave("outputs/distance_metro.png",width = 7,height = 4,units = "in")
ggsave("outputs/distance_metro.pdf",width = 7,height = 4,units = "in")


weekly_distance %>%
  group_by(metro) %>%
  summarise(mean(distance))


#####################################################
#Dwell time
weekly_dwell <- as.data.table(weekly) %>%
  .[!is.na(median_dwell),
    .(dwell=weighted.mean(median_dwell,raw_visit_counts)),
    #.(dwell=mean(median_dwell)),
    by=.(date)]

fwrite(weekly_dwell,file = "outputs/data_share/weekly_dwell.csv")

max_date <- max(weekly_dwell$date)

weekly_dwell %>%
  mutate(year=year(date),
         date=as_date(str_replace(date,"2019","2020"))) %>%
  filter(date<max_date) %>%
  ggplot(.,aes(x=date,y=dwell/60,color=factor(year))) +
  #geom_line(alpha=.3) +
  geom_smooth() +
  theme_classic(base_size = 15) +
  scale_color_brewer(name="",palette = "Dark2") +
  scale_y_continuous(labels = scales::comma) +
  labs(x="",y="",title = "Average Hours Spent at Dentist Office")

ggsave("outputs/dwell.png")

###########################################
#Dwell time
weekly_dwell <- as.data.table(weekly) %>%
  .[!is.na(median_dwell),
    .(dwell=weighted.mean(median_dwell,raw_visit_counts)),
    #.(dwell=mean(median_dwell)),
    by=.(date,state=str_sub(poi_cbg,1,2))] %>%
  inner_join(.,states,by=c("state"="state_fips"))

fwrite(weekly_dwell,file = "outputs/data_share/weekly_dwell_states.csv")

max_date <- max(weekly_dwell$date)

weekly_dwell %>%
  mutate(year=year(date),
         date=as_date(str_replace(date,"2019","2020"))) %>%
  filter(date<max_date,
         statename %in% state_subset) %>%
  ggplot(.,aes(x=date,y=dwell/60,color=factor(year))) +
  #geom_line(alpha=.3) +
  geom_smooth() +
  theme_classic(base_size = 15) +
  scale_color_brewer(name="",palette = "Dark2") +
  scale_y_continuous(labels = scales::comma) +
  labs(x="",y="",title = "Average Hours Spent at Dentist Office") +
  facet_wrap(~statename,scales = "free",ncol = 3)

ggsave("outputs/dwell_state.png",width = 6,height = 4,units = "in")

###########################################
#Dwell time
weekly_dwell <- as.data.table(weekly) %>%
  .[!is.na(median_dwell),
    .(dwell=weighted.mean(median_dwell,raw_visit_counts)),
    #.(dwell=mean(median_dwell)),
    by=.(date,fips_code=str_sub(poi_cbg,1,5))] %>%
  inner_join(cdc %>% select(fips_code,pop=county_2012_pop,metro)) %>%
  as.data.table() %>%
  .[,
    .(dwell=weighted.mean(dwell,pop)),
    by=.(date,metro)]

fwrite(weekly_dwell,file = "outputs/data_share/weekly_dwell_metro.csv")

max_date <- max(weekly_dwell$date)

weekly_dwell %>%
  mutate(year=year(date),
         date=as_date(str_replace(date,"2019","2020"))) %>%
  filter(date<max_date) %>%
  ggplot(.,aes(x=date,y=dwell/60,color=factor(year))) +
  #geom_line(alpha=.3) +
  geom_smooth() +
  theme_classic(base_size = 15) +
  scale_color_brewer(name="",palette = "Dark2") +
  scale_y_continuous(labels = scales::comma) +
  labs(x="",y="",title = "Average Hours Spent at Dentist Office") +
  facet_wrap(~metro,ncol = 1)

ggsave("outputs/dwell_metro.png",width = 6,height = 4,units = "in")

