#This script extracts the poi data and filters dentist offices

file_list <- get_bucket_df('colo-epi',max = Inf)

#####################################################
#Determine Dentist POIs

poi_list <- file_list %>%
  filter(str_detect(Key,"Core-USA-July2020-Release-CORE_POI-2020_06-2020-07-13.zip"))

save_object(object = poi_list$Key[1], 
            bucket = poi_list$Bucket[1],
            file = str_c("build/inputs/",basename(poi_list$Key[1])))

unzip(zipfile = str_c("build/inputs/",basename(poi_list$Key[1])),
      exdir = "build/inputs/core/")



poi_all <- 
  map_dfr(dir("build/inputs/core",pattern = "core_poi*"),
      function(x){
        vroom(file = str_c("build/inputs/core/",x)
        )
      })
               
#Subset Dental offices
poi_subset <- poi_all %>%
  filter(naics_code %in% c(621210,621320,812111,812112,812113))



save(poi_subset,file = "build/cache/poi_subsets.Rdata")

#load("build/cache/poi_subsets.Rdata")








