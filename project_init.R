#This script initializes the project and should be run at the beginning of each
#session

#########################
#Load init functions
source("functions/init_functions.R")

#Loading and installing packages
init.pacs(c("tidyverse",      #shortcut to many useful packages (eg, dplyr, ggplot)
            "conflicted",     #resolves function conflict across packages
            "lubridate",      #working with dates
            "sf",             #for GIS
            "data.table",
            "readxl",
            "janitor",
            "RcppRoll",
            "vroom",
            "furrr",
            "SafeGraphR",
            "cowplot",
            "arrow",
            "R.utils",
            "zoo",
            "aws.s3"
))

#Note: SafeGraphR is a package under development and needs to be installed with
# remotes::install_github('SafeGraphInc/SafeGraphR')

#Setting package::function priority with conflicted package
conflict_prefer("filter", "dplyr")
conflict_prefer("between", "dplyr")
conflict_prefer("first", "dplyr")
conflict_prefer("intersect", "dplyr")
conflict_prefer("year", "lubridate")
conflict_prefer("month", "lubridate")
conflict_prefer("lag", "dplyr")
conflict_prefer("ggsave", "ggplot2")
#########################
#Loading project helper functions (all scripts within folder)
run.script("functions")


##########################################
##########################################
#Function to download the project data (on first run, google should prompt you to login with credentials)
#if data folder doesn't exist, build data
#get_data("url")


folder.setup()

#source("aws_credentials.R")

state_subset <- c("California","Massachusetts","Florida","Washington","Colorado","Texas")
state_filter <- c("CA","MA","FL","WA","CO","TX")

states <- fips_to_names %>%
  select(state_fips,statename) %>%
  mutate(state_fips=str_pad(state_fips,2,"left",0)) %>%
  distinct()

counties <- fips_to_names %>%
  #select(state_fips,statename) %>%
  mutate(county_fips=str_c(str_pad(state_fips,2,"left",0),str_pad(county_fips,3,"left",0))) %>%
  select(-state_fips) %>%
  distinct()

dq_colors <- c("#87D4D4","#0205BA")

#plan(multisession(workers = 4))

#dlgMessage("Do you need to pull the repo?")
