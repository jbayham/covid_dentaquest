#This script distills the same day visits to the top places 

plan(multisession(workers = 2))
######################################################
#Load all same_day files for processing
same_day <- dir("build/cache/same_day",full.names = T) %>%
  future_map_dfr(read_parquet)  

brand_info <- fread("build/inputs/core/brand_info.csv")[,.(brand_name,naics_code,top_category,sub_category)]

load("build/cache/poi_subsets.Rdata")

weekly <- dir("build/cache/weekly/",full.names = T) %>%
  future_map_dfr(function(x){
    temp <- read_parquet(x,
                         col_select = c("safegraph_place_id","date_start","raw_visitor_counts")
    ) 
  })

weekly_dentists <- as.data.table(weekly) %>%
  merge.data.table(.,as.data.table(poi_subset)[str_detect(top_category,"Dentist"),.(safegraph_place_id)],keyby = c("safegraph_place_id")) 

weekly_counts <- weekly_dentists[,.(.N),by=.(date_start)]


same_day_top <- as.data.table(same_day) %>%
  merge.data.table(.,weekly,by=c("safegraph_place_id","date_start")) %>%
  .[,.(sum_visit=sum(value/100*raw_visitor_counts)),by=.(sd_brand,date_start)] %>%
  merge.data.table(.,weekly_counts,by="date_start") %>%
  .[,visit_per:=sum_visit/N] %>%
  merge.data.table(.,brand_info,by.x="sd_brand",by.y="brand_name") %>%
  .[order(date_start,-visit_per)]

same_day_top_ten <- same_day_top[,.SD[1:100],date_start]

plot_out <- map(c(3:8),
                function(x){
  same_day_rank <- same_day_top_ten %>%
    mutate(year=factor(year(date_start)),
           date_start=as_date(str_replace(date_start,"2019","2020"))) %>%
    filter(month(date_start) %in% c(x)) %>%
    group_by(brand=sd_brand,year) %>%
    summarize(mean_visits=mean(sum_visit)) %>%
    ungroup() %>%
    arrange(year,desc(mean_visits)) %>%
    group_by(year) %>%
    mutate(rank=row_number()) %>%
    ungroup() %>%
    filter(rank<=15)
  
  labvals <- levels(same_day_rank$year)[2:3]
  same_day_rank <- mutate(same_day_rank,year=as.integer(year))
  
  #bump plot
  out <- ggplot(data = same_day_rank, aes(x = year, y = rank, group = brand)) +
    geom_line(aes(color = brand, alpha = 1), size = 2) +
    geom_vline(xintercept = c(2,3),color="#888888") +
    geom_point(aes(color = brand, alpha = 1), size = 4) +
    geom_point(color = "#FFFFFF", size = 1) +
    scale_y_reverse() +
    geom_text(data = same_day_rank %>% filter(year == 2),
              aes(label = brand, x = 1.8) , hjust = 1, fontface = "bold", color = "#888888", size = 2.7) +
    geom_text(data = same_day_rank %>% filter(year == 3),
              aes(label = brand, x = 3.2) , hjust = 0, fontface = "bold", color = "#888888", size = 2.7) +
    scale_x_continuous(limits = c(-.5,5.5),
                       breaks = c(2,3),
                       labels = c('2019','2020')) +
    labs(x="",y="",title = str_c(month.name[x])) +
    my_theme() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank())
  
  ggplot2::ggsave(str_c("outputs/same_day_rank_",month.name[x],".pdf"),height = 5,width = 5,units = "in")
  
  return(out)
  
})

plot_grid(plotlist = plot_out,nrow = 2) %>%
  cowplot::ggsave(plot = .,"outputs/same_day_panel.pdf",width = 9,height = 7,units = "in")


#############################################
#Individual

map(c(3:7),
    function(x){
      same_day_rank <- same_day_top_ten %>%
        mutate(year=factor(year(date_start)),
               date_start=as_date(str_replace(date_start,"2019","2020"))) %>%
        filter(month(date_start) %in% c(x)) %>%
        group_by(brand=sd_brand,year) %>%
        summarize(mean_visits=mean(sum_visit)) %>%
        ungroup() %>%
        arrange(year,desc(mean_visits)) %>%
        group_by(year) %>%
        mutate(rank=row_number()) %>%
        ungroup() %>%
        filter(rank<=15)
      
      labvals <- levels(same_day_rank$year)[2:3]
      same_day_rank <- mutate(same_day_rank,year=as.integer(year))
      
      #bump plot
      ggplot(data = same_day_rank, aes(x = year, y = rank, group = brand)) +
        geom_line(aes(color = brand, alpha = 1), size = 2) +
        geom_vline(xintercept = c(2,3),color="#888888") +
        geom_point(aes(color = brand, alpha = 1), size = 4) +
        geom_point(color = "#FFFFFF", size = 1) +
        scale_y_reverse() +
        geom_text(data = same_day_rank %>% filter(year == 2),
                  aes(label = brand, x = 1.9) , hjust = 1, fontface = "bold", color = "#888888", size = 3) +
        geom_text(data = same_day_rank %>% filter(year == 3),
                  aes(label = brand, x = 3.1) , hjust = 0, fontface = "bold", color = "#888888", size = 3) +
        scale_x_continuous(limits = c(1,4),
                           breaks = c(2,3),
                           labels = c('2019','2020')) +
        labs(x="",y="",title = str_c(month.name[x])) +
        my_theme() +
        theme(panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank())
      
      

      ggsave(str_c("outputs/same_day_rank_",month.name[x],".png"),height = 5,width = 5,units = "in")
    })



##############################################
#By category
map(c(3:7),function(x){
  same_day_rank <- same_day_top_ten %>%
    mutate(year=factor(year(date_start)),
           date_start=as_date(str_replace(date_start,"2019","2020"))) %>%
    filter(month(date_start) %in% c(x)) %>%
    group_by(sub_category,year) %>%
    summarize(mean_visits=mean(sum_visit)) %>%
    ungroup() %>%
    arrange(year,desc(mean_visits)) %>%
    group_by(year) %>%
    mutate(rank=row_number()) %>%
    ungroup() %>%
    filter(rank<=15)
  
  #bump plot
  ggplot(data = same_day_rank, aes(x = year, y = rank, group = sub_category)) +
    geom_line(aes(color = sub_category, alpha = 1), size = 2) +
    geom_point(aes(color = sub_category, alpha = 1), size = 4) +
    geom_point(color = "#FFFFFF", size = 1) +
    scale_y_reverse() +
    geom_text(data = same_day_rank %>% filter(year == "2019"),
              aes(label = sub_category, x = .9) , hjust = 1, fontface = "bold", color = "#888888", size = 4) +
    geom_text(data = same_day_rank %>% filter(year == "2020"),
              aes(label = sub_category, x = 2.1) , hjust = 0, fontface = "bold", color = "#888888", size = 4) +
    labs(x="",y="",title = str_c("Locations visited on \nsame day as dentist (",month.name[x],")")) +
    my_theme()
  
  ggsave(str_c("outputs/same_day_rank_category_",month.name[x],".png"),height = 6,width = 8,units = "in")
})

#Graphing category
same_day_top_ten_category <- same_day_top[,.(category_visits=sum(sum_visit)/sum(N)),.(date_start,sub_category)] %>%
  .[order(date_start,-category_visits),sub_category:=str_sub(sub_category,1,45)]


#Time series plots
same_day_top_ten_category %>%
  filter(sub_category %in% same_day_top_ten_category$sub_category[1:5],
         #month(date_start,label = T)=="Apr",
         month(date_start) %in% c(1:5)) %>%
  mutate(year=factor(year(date_start)),
         date_start=as_date(str_replace(date_start,"2019","2020")),
         year_fade=ifelse(year=="2019",.4,1)) %>%
  ggplot(.,aes(x=date_start,y=category_visits,color=year)) +
  geom_smooth() +
  theme_minimal(base_size = 15) +
  labs(x="",y="",title = "Same Day Visit Index") +
  scale_color_discrete(name="") +
  facet_wrap(~sub_category,scales = "free")


#The value shown for each brand is a percentage representing the median of the
#following calculation for each day in the month: 
#(same-day visitors to both the brand and the POI / total daily visitors to the POI) - (daily visitors to the
#brand / all visitors in SafeGraph panel). This column will only contain values
#if there are at least 5 visitors to the POI.
#
#fraction of poi visitors that go to brand - nomalized national brand visits

