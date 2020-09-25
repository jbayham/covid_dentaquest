#This script analyzes daily visitation


load("analysis/cache/poi_ref.Rdata")
load("analysis/cache/home_summary.Rdata")
load("analysis/cache/normalization.Rdata")
load("build/cache/poi_subsets.Rdata")
load("analysis/cache/cdc.Rdata")

########################################################

daily_visits <- dir("build/cache/daily",full.names = T) %>%
  future_map_dfr(read_parquet)  

daily_dentists <- as.data.table(daily_visits) %>%
  merge.data.table(.,as.data.table(poi_subset)[str_detect(top_category,"Dentist"),.(safegraph_place_id)],by = c("safegraph_place_id")) 

#Construct daily national dentist visits
daily_visits_total <- daily_dentists %>%
  .[,.(visit_count=sum(value)),by=.(date)] %>%
  merge.data.table(normalization,by = "date") %>%
  .[,visits_norm:=visit_count/total_devices_seen]


max_date <- max(daily_visits_total$date)

daily_visits_total %>%
  filter(year(date)>2018) %>%
  mutate(year=year(date),
         date=as_date(str_replace(date,"2019","2020"))) %>%
  filter(date<max_date) %>%
  ggplot(.,aes(x=date,y=visits_norm,color=factor(year))) +
  #geom_line(alpha=.3) +
  geom_smooth() +
  theme_classic(base_size = 15) +
  scale_color_manual(name="",values = dq_colors) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_date(date_breaks = "1 month",date_labels = "%b") +
  labs(x="",y="",subtitle = "Dental Office Visitation Rate")

ggsave("outputs/daily_visits_national.png",width = 6,height = 4,units = "in")
ggsave("outputs/daily_visits_national.pdf",width = 6,height = 4,units = "in")

fwrite(daily_visits_total,file = "outputs/data_share/daily_visit_count.csv")

daily_visits_total %>%
  group_by(year,month) %>%
  summarize(report=mean(visits_norm))

############################################################
#Construct county home summary
home_summary_county <- home_summary %>%
  group_by(fips_code=str_sub(census_block_group,1,5),date_range_start,date_range_end) %>%
  summarise(devices=sum(number_devices_residing)) %>%
  ungroup()

###########################################################
#Visits by metro



daily_dentist_metro <- 
  merge.data.table(daily_dentists,
                   poi_ref %>% select(safegraph_place_id,fips_code=fips),
                   by = "safegraph_place_id") %>%
  .[,
    .(visits=sum(as.numeric(value))),
    by=.(date,fips_code)] %>%
  merge.data.table(.,
                   cdc %>% select(fips_code,metro) %>% as.data.table(),
                   by = "fips_code") %>%
  merge.data.table(.,
                   home_summary_county %>% rename(date=date_range_start),
                   by = c("fips_code","date"),
                   all.x = T) %>%
  fill(devices,.direction = "down") %>%
  as.data.table() %>%
  .[,
    .(visits=sum(visits)/sum(devices)),
    by=.(date,metro)]
  
  
metro_plot <- daily_dentist_metro %>%
  filter(year(date)>2018) %>%
  mutate(year=year(date),
         date=as_date(str_replace(date,"2019","2020"))) %>%
  filter(date<max_date) 

metro_plot %>%
  ggplot(.,aes(x=date,y=visits,color=factor(year))) +
  geom_smooth() +
  theme_classic(base_size = 15) +
  scale_color_manual(name="",values = dq_colors) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_date(date_breaks = "2 month",date_labels = "%b") +
  labs(x="",y="",subtitle = "Dental Office Visitation Rate") +
  facet_wrap(~metro) +
  theme(panel.spacing = unit(2, "lines"))

ggsave("outputs/daily_visits_metro.png",width = 8,height = 4,units = "in")
ggsave("outputs/daily_visits_metro.pdf",width = 8,height = 4,units = "in")

fwrite(metro_plot,file = "outputs/data_share/daily_visit_metro.csv")

metro_plot %>%
  group_by(year,month=month(date,abbr = T)) %>%
  summarize(report=mean(visits))


############################################################
# Visits by state


daily_visits_state_temp <- as.data.table(daily_dentists) %>%    #[sample.int(nrow(daily_dentists),10000),] %>%
  merge.data.table(.,poi_ref[,c("safegraph_place_id","state","poi_cbg")],by="safegraph_place_id") %>%
  #.[state %in% state_filter,] %>%
  merge.data.table(.,home_summary,by.x=c("poi_cbg","date"),by.y = c("census_block_group","date_range_start"),all.x = T) %>%
  setorder(poi_cbg,date) %>%
  group_by(poi_cbg) %>%
  fill(number_devices_residing,.direction = "down")

daily_visits_state <- as.data.table(daily_visits_state_temp) %>%
  .[!is.na(value),
    .(daily_visits=sum(value,na.rm = T)/sum(number_devices_residing,na.rm = T)),
    by=.(date,state=str_sub(poi_cbg,1,2))] %>%
  inner_join(.,states,by=c("state"="state_fips"))

fwrite(daily_visits_state,file = "outputs/data_share/daily_visits_state.csv")
 
# max_date <- max(daily_visits_state$date)
# 
# daily_visits_state %>%
#   filter(year(date)>2018,
#          statename %in% state_subset) %>%
#   mutate(year=year(date),
#          date=as_date(str_replace(date,"2019","2020"))) %>%
#   filter(date<max_date) %>%
#   ggplot(.,aes(x=date,y=daily_visits,color=factor(year))) +
#   #geom_col(width = 0) +
#   #geom_line(alpha=.3) +
#   #geom_smooth(method = "gam",formula = y ~ splines::bs(x,3)) +
#   geom_smooth() +
#   theme_classic(base_size = 15) +
#   scale_color_manual(name="",values = dq_colors) +  
#   scale_y_continuous(labels = scales::percent_format(accuracy = .1)) +
#   labs(x="",y="",title = "Daily Visitation to Dentist Offices") +
#   facet_wrap(~statename,ncol = 3)
# 
# ggsave("outputs/daily_visits_state.png",width = 7,height = 4,units = "in")
