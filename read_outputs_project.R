## Read Aquacrop plugin outputs
# Simulacion Aquacrop - Guyana
# By https://github.com/jrodriguez88 
# 2019

library(tidyverse)
library(data.table)
library(lubridate)
library(skimr)

path <- "D:/03_DEVELOPER/rice_guyana/climate_future2/"

setwd(path)

# season files
files <- list.files(path, pattern = "season")


read_aquacrop_season <- function(file, path){
    
    
    names <- read_lines(paste0(path, file))[[3]] %>%
                                    str_trim() %>%
                                    str_split(pattern = "[ ]+") %>%
                                    flatten_chr() %>% 
                                    c(., "File")
    
  data <- fread(paste0(path, file), skip = 4) %>%
        setNames(names)
  
  return(data)
    
}

read_aquacrop_day <- function(file, path){

    day_file <- read_lines(paste0(path, file))
    
    find_run <- day_file %>%
        str_detect("Run:") %>%
        which()+2
    
    nlines <- c(find_run[-1], length(day_file))-find_run-2
    
    arg_list <- list(file= file,
                     skip = find_run, 
                     nrows = nlines)
    
    # Read_pmap
    dat <- suppressWarnings(pmap(arg_list, fread))%>%
        bind_rows(.id = "run")

   return(dat)
    
}    

###Name variables 

name_var <- c("crop", "pdate", "crop_sys", "clim_scenario")

#read_aquacrop_season(file, path)

data <- pmap(list(files, path), read_aquacrop_season) %>%
    bind_rows() %>% 
    mutate(File = str_replace(File, ".PRM", "")) %>%
    separate(File, name_var, sep = "_")

data$pdate <- factor(data$pdate, levels = c("March15","April15", "April30", "May15","May30", "June15", "June30"))

### Histogram summary all data (one location or scenario)
data %>% select(Yield, BioMass, Cycle, Irri, clim_scenario, crop_sys) %>% 
    gather("var", "value", -clim_scenario) %>% ggplot(aes(value)) +
    geom_histogram(bins = 10, color="grey") + facet_wrap(var ~., scales = "free") + 
    theme_classic()

data %>% select(Yield, BioMass, Cycle, Irri, clim_scenario, crop_sys) %>%
    gather("var", "value", -c(clim_scenario, crop_sys)) %>% 
    ggplot(aes(crop_sys, value)) +
    geom_boxplot(aes(fill=clim_scenario)) + facet_wrap(var ~., scales = "free") + 
    theme_classic()


### density plot for Yield, all location
data %>% mutate(crop_sys =  case_when(crop_sys == "Rainfall" ~ "Rainfed",
                                      TRUE ~ crop_sys),
                location = case_when(str_detect(clim_scenario, "RCP45") ~ str_replace(clim_scenario, "RCP45", ""),
                                     str_detect(clim_scenario, "RCP85") ~ str_replace(clim_scenario, "RCP85", ""),
                                     TRUE ~ clim_scenario),
                clim_scenario = case_when(str_detect(clim_scenario, "RCP45") ~ "RCP45",
                                          str_detect(clim_scenario, "RCP85") ~ "RCP85",
                                          TRUE ~ "Reference 1998-2018"),
                location = str_replace(location, "New", "")) %>% 
#    filter(str_detect(clim_scenario, pattern = "Borasire")) %>%
    select(Yield, clim_scenario, crop_sys, location) %>%    ##### Change the Yield by parameter
#    gather("var", "value", -c(clim_scenario, crop_sys)) %>% 
    ggplot(aes(Yield)) +   #Change parameter
    geom_density(aes(fill=clim_scenario), alpha = 0.3) + facet_grid(crop_sys ~location, scales = "free") + 
    theme_classic()


##### Plot to compare multiple climate scenarios

title_name <- "Guyana - Aquacrop rice crop simulation"
plot_data <- data %>% mutate(crop_sys =  case_when(crop_sys == "Rainfall" ~ "Rainfed",
                                                   TRUE ~ crop_sys),
                             location = case_when(str_detect(clim_scenario, "RCP45") ~ str_replace(clim_scenario, "RCP45", ""),
                                                  str_detect(clim_scenario, "RCP85") ~ str_replace(clim_scenario, "RCP85", ""),
                                                  TRUE ~ clim_scenario),
                             clim_scenario = case_when(str_detect(clim_scenario, "RCP45") ~ "RCP45",
                                                       str_detect(clim_scenario, "RCP85") ~ "RCP85",
                                                       TRUE ~ "Reference 1998-2018"),
                             location = str_replace(location, "New", ""),
                             region = case_when(location == "Karasabia" | location == "Lethem" ~ "Region 9",
                                                TRUE ~ "Region 3"))%>% 
    mutate(clim_scenario = factor(clim_scenario, levels = c("Reference 1998-2018", "RCP45", "RCP85")))

 plot_data %>%
    #    filter(str_detect(clim_scenario, pattern = "Borasire")) %>%
    select(Yield, clim_scenario, crop_sys, location, region) %>% 
    #    gather("var", "value", -c(clim_scenario, crop_sys)) %>% 
    ggplot(aes(clim_scenario, Yield)) +
    geom_boxplot(aes(x = clim_scenario, fill = clim_scenario), alpha=0.7) + 
    facet_grid(crop_sys ~ region+location) + 
    theme_bw() +
    theme(
        axis.text.x = element_blank(),
        legend.position="bottom",
        legend.title = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background=element_rect(fill="white", size=1.5, linetype="solid"),
        strip.text = element_text(face = "bold")) +
    scale_fill_manual(values=c("darkgreen", "yellow", "red"))+
    labs(x = "Climate Scenario", 
         y= "Yield (Tn/ha)", 
         title = title_name)



#data %>% group_by(clim_scenario) %>%
#    skim()


### plots

#data %>%
#    ggplot(aes(Year1, Yield, color = pdate)) + 
#    geom_line() +
#    facet_grid(location ~ pdate) +
#    theme_bw() +
#    theme(legend.position="bottom")




names(plot_data)
plot_data %>%
    select(Year1, Day1, Month1, pdate, location, crop_sys, Yield, clim_scenario) %>%
    mutate(date = make_date(Year1, Month1, Day1)) %>%
#    gather("Sim_variable", "Value", -c(1:6, 11, 12)) %>%
    ggplot(aes(pdate,  Yield, fill=clim_scenario)) + 
#    scale_x_date(labels = function(x) format(x, "%B-%d")) +
    geom_boxplot(outlier.shape=NA,  alpha=0.7) +
#    facet_wrap(riego ~. ) +
    theme_bw() +
    theme(legend.position="bottom",
          legend.title = element_blank(),
          panel.grid.minor = element_blank(),
          strip.background=element_rect(fill="white", size=1.5, linetype="solid"),
          strip.text = element_text(face = "bold")) +
    labs(title = "Guyana Rice Production") +
    facet_grid(location~crop_sys, scales = "free") +
    scale_fill_manual(values=c("darkgreen", "yellow", "red"))+
    labs(x = "Climate Scenario", 
         y= "Yield (Tn/ha)", 
         title = title_name)
#    scale_fill_viridis_d()




#$### Plot for one (1) location
mean_yield <- data %>% select(Year1, Day1, Month1, pdate, location, crop_sys, Yield, BioMass, Cycle) %>%
    mutate(date = make_date(Year1, Month1, Day1), 
           jdate = yday(date)) %>%
    gather("Sim_variable", "Value", -c(1:6, 10, 11)) %>% 
    filter(Sim_variable=="Yield") %>% group_by(crop_sys, location, Year1) %>% 
    summarise(Value=mean(Value)) %>%
    ungroup()

data %>% select(Year1, Day1, Month1, pdate, location, crop_sys, Yield, BioMass, Cycle) %>%
    mutate(date = make_date(Year1, Month1, Day1), 
           jdate = yday(date)) %>%
    gather("Sim_variable", "Value", -c(1:6, 10, 11)) %>% 
    filter(Sim_variable=="Yield") %>%
    ggplot(aes(Year1,  Value)) + 
    #    scale_x_date(labels = function(x) format(x, "%B-%d")) +
    geom_point() + geom_line(data=mean_yield, aes(x=Year1, y=Value), color = "red") + 
    
    #    facet_wrap(riego ~. ) +
    theme_bw() +
    theme(legend.position="bottom", legend.title = element_blank()) +
    labs(title = "Guyana Rice Production", 
         y="Yield (Tonnes/ha)",
         x = "Year") +
    facet_grid(location~crop_sys, scales = "free_y") 






###### OOOOTher Plot

plot_data %>%
    #    filter(str_detect(clim_scenario, pattern = "Borasire")) %>%
    select(Yield, Tr, Cycle, clim_scenario, crop_sys, location, region, Year1, Irri) %>% 
    mutate(clim_scenario = factor(clim_scenario, levels = c("Reference 1998-2018", "RCP45", "RCP85"))) %>%
    gather("var", "value", -c(clim_scenario, crop_sys, location, region, Year1)) %>% 
    ggplot(aes(crop_sys, value)) +
    geom_boxplot(aes(fill=clim_scenario)) + 
    facet_wrap(location+var ~., scales = "free") + 
    theme_classic()




#### Net Irrigation plots

title_name <- "Guyana - Aquacrop rice crop simulation"
plot_data %>%
    #    filter(str_detect(clim_scenario, pattern = "Borasire")) %>%
    select(pdate, Yield, clim_scenario, crop_sys, location, region, Irri) %>% filter(crop_sys=="NetIrrigation") %>%
    mutate(clim_scenario = factor(clim_scenario, levels = c("Reference 1998-2018", "RCP45", "RCP85"))) %>%
    #    gather("var", "value", -c(clim_scenario, crop_sys)) %>% 
    ggplot(aes(clim_scenario, Irri)) +
    geom_boxplot(aes(x = clim_scenario, fill = clim_scenario), alpha=0.7) + 
    facet_grid(pdate ~ region+location, scales = "free") + 
    theme_bw() +
    theme(
        axis.text.x = element_blank(),
        legend.position="bottom",
        legend.title = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background=element_rect(fill="white", size=1.5, linetype="solid"),
        strip.text = element_text(face = "bold")) +
    scale_fill_manual(values=c("darkgreen", "yellow", "red"))+
    labs(x = "Climate Scenario", 
         y= "Net_irrigation", 
         title = title_name)

