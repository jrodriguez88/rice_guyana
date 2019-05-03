### Guyana weather data from Excel 
# Author: Rodriguez-Espinoza J.
# Repository: https://github.com/jrodriguez88/rice_guyana
# 2019


# load packages
library(tidyverse)
library(lubridate)

# Function to import data from excel, only require copy "clipboard"
import_data <- function(varname="rain") {
    
    obs_data <- read.table("clipboard", header = T, stringsAsFactors = F, sep = "\t")
    
    varn = paste0(varname, "_obs")
    
    data <- obs_data %>% gather("day", "var", -c(Year, Month)) %>% 
        mutate(day = str_extract(day, "[0-9]+") %>% as.numeric(),
               date = make_date(Year, Month, day)) %>%
        select(date, var) %>% setNames(c("date", varn)) %>% 
        replace_with_na_all(condition = ~.x == -9999)
    
    return(data)
    
}


rain_obs <- import_data("rain")
tmax_obs <- import_data("tmax")
tmin_obs <- import_data("tmin")
suns_obs <- import_data("suns")
wind_obs <- import_data("wind")


# create date matrix
dates <- seq.Date(make_date(1988, 1, 1), make_date(2018, 12, 31), by = "day")


# make a DF with all data
obs_data_gt <- dates %>% enframe(value = "date", name=NULL) %>% 
    left_join(rain_obs) %>%
    left_join(tmax_obs) %>%
    left_join(tmin_obs) %>% 
    left_join(suns_obs) %>%
    left_join(wind_obs %>% filter(wind_obs<10))

## make a plot for all obs data
obs_data_gt %>% gather("var", "value", -date) %>% ggplot(aes(date, value))+
    geom_line()+ facet_grid(var ~. , scales = "free") + theme_bw()



### require load nasapower script

nasa_pw <- data %>% replace_with_na_all(condition = ~.x == -999) %>%
    select(date, PRECTOT,T2M_MAX, T2M_MIN, ALLSKY_SFC_SW_DWN, WS2M, RH2M, WS2M_MAX)




obs_data_gt %>% ggplot(aes(x = date))+
    geom_line(aes(y=rain_obs)) +
    geom_line(data = nasa_pw, aes(y=PRECTOT), color="red", alpha=0.3)

obs_data_gt %>% ggplot(aes(x = date))+
    geom_line(aes(y=wind_obs)) +
    geom_line(data = nasa_pw, aes(y=WS2M), color="green", alpha=0.3) +
    geom_line(data = nasa_pw, aes(y=WS2M_MAX), color="red", alpha=0.3)

obs_data_gt %>% ggplot(aes(x = date))+
    geom_line(aes(y=tmin_obs)) +
    geom_line(data = nasa_pw, aes(y=T2M_MIN), color="red")


mean(nasa_pw$WS2M, na.rm = T)


### to estimate solar radiation

library(sirad)
test1 <- suns_obs %>%
    mutate(srad = ap(days = date,
                     lat = 6.8, 
                     lon = -58.1,
                     A = 0.29,
                     B = 0.45,
                     SSD = suns_obs)) %>%
    arrange(date)
 
##plot
nasa_pw %>%
    mutate(srad_np = ALLSKY_SFC_SW_DWN*3.6) %>%
    left_join(test1) %>%
    ggplot(aes(date, srad_np))+
    geom_line() +
    geom_line(aes(y=srad), color="red", alpha=0.3) +
    theme_bw()

nasa_pw %>%
    mutate(srad_np = ALLSKY_SFC_SW_DWN*3.6) %>%
    left_join(test1) %>%
    ggplot(aes(srad_np, srad)) + geom_point(alpha=0.3)





write.csv(test1, "srad.csv")





