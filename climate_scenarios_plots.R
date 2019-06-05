#### Analize climate files --- reference vs RCP 


library(tidyverse)
library(readxl)
library(data.table)
library(lubridate)

# Read data and set paths
path <- paste0(getwd(), "/data")   ## For climate data (.txt) import format 
files <- list.files(path, pattern = ".txt")

path_future <- paste0(getwd(), "/climate_future/") ## For future climate data (.xlsx). Review names in RC_45 (karasabia name)

names <- files %>% str_remove(".txt")

## Function to read import data , weather txt file    
read_weather_data <- function(path, file) {
    
    fread(paste0(path, "/", file), col.names = c("rain", "srad", "tmax", "tmin")) %>% as_tibble() %>%
        mutate(date = seq.Date(make_date(1998, 1, 1),
                               make_date(2018, 12, 31), "days")) %>%
        select(date, everything())
    
}

data <- map(files, ~read_weather_data(path = path, file = .x)) %>% set_names(names)

reference_data <- data %>% bind_rows(.id = "id") %>% nest(-id) %>%
    separate(id, c("location", "region"), sep="_") %>% unnest(data) %>%
    mutate(clim_scenario = "Reference") %>% 
    select(clim_scenario, location, date, everything()) %>%
    nest(-c(location, clim_scenario))



future_data <- map(list.files(path = path_future, pattern = "MaxTemp", full.names = T),
            ~read_xlsx(.x) %>% gather("location", "tmax", -date)) %>%
            set_names(c("RCP_45", "RCP_85")) %>%
            bind_rows(.id =  "clim_scenario") %>%
            mutate(date=as.Date(date)) %>% 
    left_join(
        map(list.files(path = path_future, pattern = "MinTemp", full.names = T),
            ~read_xlsx(.x) %>% gather("location", "tmin", -date)) %>%
            set_names(c("RCP_45", "RCP_85")) %>%
            bind_rows(.id =  "clim_scenario") %>%
            mutate(date=as.Date(date))) %>% 
    left_join(
        map(list.files(path = path_future, pattern = "Rainfall", full.names = T),
            ~read_xlsx(.x) %>% gather("location", "rain", -date)) %>%
            set_names(c("RCP_45", "RCP_85")) %>%
            bind_rows(.id =  "clim_scenario")%>%
            mutate(date=as.Date(date))) %>%
    nest(-c(location, clim_scenario))


climate_data <- bind_rows(reference_data, future_data) %>% 
    unnest(data) %>% 
    mutate(
        clim_scenario = factor(clim_scenario, 
            levels = c("Reference", "RCP_45", "RCP_85")))


### Plot temperature
climate_data %>%
    ggplot(aes(date, tmax)) + geom_line(color = "red") + 
    geom_line(aes(y=tmin), color = "orange") +
    facet_grid(location ~ clim_scenario, scales = "free_x") +
    theme_bw() +
    theme(
        axis.text.x = element_text(angle = 45),
        #        legend.position="bottom",
        #        legend.title = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background=element_rect(fill="white", size=1.5, linetype="solid"),
        strip.text = element_text(face = "bold")) +  
    labs(x = "Date", 
         y= "Temperature (oC)", 
         title = "Guyana Climate Data")

## Plot rain
climate_data %>%
    ggplot(aes(date, rain)) + geom_line(color = "blue") + 
    facet_grid(location ~ clim_scenario, scales = "free_x") +
    theme_bw() +
    theme(
        axis.text.x = element_text(angle = 45),
        #        legend.position="bottom",
        #        legend.title = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background=element_rect(fill="white", size=1.5, linetype="solid"),
        strip.text = element_text(face = "bold")) +  
    labs(x = "Date", 
         y= "Rain (mm)", 
         title = "Guyana Climate Data")



## Plot Solar Radiation - We have some probles here/ Solar.
climate_data %>% filter(clim_scenario == "Reference") %>%
    ggplot(aes(date, srad)) + geom_line(color = "orange") + 
    facet_grid(location ~ ., scales = "free_x") +
    theme_bw() +
    theme(
        axis.text.x = element_text(angle = 45),
        #        legend.position="bottom",
        #        legend.title = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background=element_rect(fill="white", size=1.5, linetype="solid"),
        strip.text = element_text(face = "bold")) +  
    labs(x = "Date", 
         y= "Srad (MJ/m² d)", 
         title = "Guyana Climate Data")
