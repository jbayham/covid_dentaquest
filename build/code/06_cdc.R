#Get urban-rural classification


#download.file("https://www.cdc.gov/nchs/data/data_acces_files/NCHSURCodes2013.xlsx",destfile = "build/inputs/NCHSUR.xlsx")

cdc <- read_excel("build/inputs/NCHSUR.xlsx") %>%
  clean_names() %>%
  mutate(fips_code=str_pad(fips_code,5,"left",0),
         metro=ifelse(x2013_code<5,"Metro","Non-Metro"),
         county_2012_pop=as.numeric(county_2012_pop),
         county_2012_pop=ifelse(is.na(county_2012_pop),0,county_2012_pop))

save(cdc,file="analysis/cache/cdc.Rdata")

# cdc %>%
#   ggplot(aes(x=metro)) +
#   geom_histogram()
