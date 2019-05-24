#### Aquacrop make Project files
# By https://github.com/jrodriguez88
# Author: Rodriguez-Espinoza J.
# 2019

## load packages and scripts
library(tidyverse)
library(data.table)
library(lubridate)


### select weather station
data_wth <- all_data %>% filter(Loc=="Boerasire") %>% unnest(data)

## calculate GDDs 
clim_data <- data_wth %>% mutate(HUH = map2(tmax, tmin, HUH_cal) %>% flatten_dbl(),
                    HUH2 = ((tmax + tmin)/2) - 8)


# Path of aquacrop files
aquacrop_files <- paste0(path, "/aquacrop_files/")
aquacrop_path <- "C:\\AQUACROP\\DATA\\"
###file names 
clim_file <- list.files(aquacrop_files, pattern = "CLI") %>% str_remove(".CLI")
co2_file <- list.files(aquacrop_files, ".CO2")
crop_file <- list.files(aquacrop_files, ".CRO")
irri_file <- list.files(aquacrop_files, ".IRrR") %>% c(., "rainfed")
man_file <- list.files(aquacrop_files, ".MAN")
soil_file <- list.files(aquacrop_files, ".SOL")
ini_file <- list.files(aquacrop_files, ".SW0")

### Set sowing dates 
max_crop_duration <- 150
sowing_date <- seq.Date(make_date(year = 1998, month = 5, day = 15), 
                 make_date(year = 2000, month = 5, day = 15), by="weeks")


## Function to calculate and create crop growing cycles
cal_cycles_project <- function(clim_data,
                               aquacrop_files,
                               clim_file,
                               co2_file,
                               crop_file,
                               irri_file, 
                               man_file,
                               soil_file,
                               ini_file,
                               max_crop_duration,
                               sowing_date) {
    


### extract "GDDays: from sowing to maturity" from CRO_file
gdd_mt <- read_lines(file = paste0(aquacrop_files, crop_file)) %>%
    str_subset("GDDays: from sowing to maturity") %>% 
    str_extract("[0-9]+") %>% as.numeric


# calculate crop duration 
crop_duration <- clim_data %>% 
    dplyr::filter(date >= sowing_date,
           date <= sowing_date + max_crop_duration) %>%
    mutate(sum_gdd = cumsum(HUH)) %>%
    dplyr::filter(sum_gdd<= gdd_mt) %>% 
    count() %>% pull(n)+3

# Calculate numeric dates
first_day <- as.numeric(sowing_date - make_date(1900, 12, 31))
last_day <- first_day + crop_duration
mat_date <- as.Date(last_day, origin = make_date(1900, 12, 31))

#Write grow cycles
path_data <- function(){
    
    cat(paste0(first_day, "    : First day of simulation period - ", format(sowing_date, "%d %b %Y")))
    cat('\n')
    cat(paste0(last_day,  "    : Last day of simulation period - ",  format(mat_date, "%d %b %Y")))
    cat('\n')
    cat(paste0(first_day, "    : First day of cropping period - " , format(sowing_date, "%d %b %Y")))
    cat('\n')
    cat(paste0(last_day,  "    : Last day of cropping period - "  , format(mat_date, "%d %b %Y")))
    cat('\n')    
cat("-- 1. Climate (CLI) file", sep = '\n')
    cat(paste0(clim_file, ".CLI"), sep = '\n')
    aquacrop_files %>% str_replace_all(pattern = "/", replacement = "\\\\") %>% writeLines
    cat("1.1 Temperature (TMP) file", sep = '\n')
    cat(paste0(clim_file, ".Tnx"), sep = '\n') 
    aquacrop_files %>% str_replace_all(pattern = "/", replacement = "\\\\") %>% writeLines
    cat("1.2 Reference ET (ETo) file", sep = '\n')
    cat(paste0(clim_file, ".ETo"), sep = '\n')
    aquacrop_files %>% str_replace_all(pattern = "/", replacement = "\\\\") %>% writeLines
    cat("1.3 Rain (PLU) file", sep = '\n')
    cat(paste0(clim_file, ".PLU"), sep = '\n')
    aquacrop_files %>% str_replace_all(pattern = "/", replacement = "\\\\") %>% writeLines
    cat("1.4 Atmospheric CO2 (CO2) file", sep = '\n')
    cat(paste(co2_file), sep = '\n')
    aquacrop_files %>% str_replace_all(pattern = "/", replacement = "\\\\") %>% writeLines
cat("-- 2. Crop (CRO) file", sep = '\n')
    cat(paste(crop_file), sep = '\n')
    aquacrop_files %>% str_replace_all(pattern = "/", replacement = "\\\\") %>% writeLines
cat("-- 3. Irrigation (IRR) file", sep = '\n')
if(irri_file=="rainfed"){
    cat("(None)", sep = '\n')
    cat("(None)", sep = '\n')
} else {
    cat(paste(irri_file), sep = '\n')
    aquacrop_files %>% str_replace_all(pattern = "/", replacement = "\\\\") %>% writeLines
}
cat("-- 4. Management (MAN) file", sep = '\n')
    cat(paste(man_file), sep = '\n')
    aquacrop_files %>% str_replace_all(pattern = "/", replacement = "\\\\") %>% writeLines
cat("-- 5. Soil profile (SOL) file", sep = '\n')
    cat(paste(soil_file), sep = '\n')
    aquacrop_files %>% str_replace_all(pattern = "/", replacement = "\\\\") %>% writeLines
cat("-- 6. Groundwater (GWT) file", sep = '\n')
    cat("(None)", sep = '\n')
    cat("(None)", sep = '\n')
cat("-- 7. Initial conditions (SW0) file", sep = '\n')
    cat(paste(ini_file), sep = '\n')
    aquacrop_files %>% str_replace_all(pattern = "/", replacement = "\\\\") %>% writeLines
cat("-- 8. Off-season conditions (OFF) file", sep = '\n')
    cat("(None)", sep = '\n')
    cat("(None)", sep = '\n')
}

list(capture.output(path_data()))

}


###########################

### Create multiple combinations of params

params <- expand.grid(aquacrop_files,
    clim_file,
    co2_file,
    crop_file,
    irri_file, 
    man_file,
    soil_file,
    ini_file,
    max_crop_duration,
    sowing_date) %>% as_tibble() %>%
    setNames(c("aquacrop_files",
             "clim_file",
             "co2_file",
             "crop_file",
             "irri_file", 
             "man_file",
             "soil_file",
             "ini_file",
             "max_crop_duration",
             "sowing_date"))

runs_cal <- function(params, clim_data) {
    
params %>% mutate(runs = cal_cycles_project(clim_data, 
                       aquacrop_files,
                       clim_file,
                       co2_file,
                       crop_file,
                       irri_file, 
                       man_file,
                       soil_file,
                       ini_file,
                       max_crop_duration,
                       sowing_date)) 
    
}

sim_cycles <- split(params, 1:nrow(params)) %>% 
    map(., ~runs_cal(., clim_data)) %>%
    bind_rows()




sink(file = "test22.PRM", append = T)
cat("description here")
cat('\n')
cat("6.0       : AquaCrop Version (March 2017)")
cat('\n')
writeLines(sim_cycles$runs[[1]][1:4])
writeLines(def_param)
writeLines(sim_cycles$runs[[1]][-c(1:4)])
walk(.x=sim_cycles$runs[-1], ~writeLines(.x))
sink()


write_projects <- function(pfile = "project_sample.PRM", info = "by JRE", list_sett)
    
    
### Default parameters,  
def_param <- read_lines("data/project_sample.PRM", skip = 6, n_max = 21) 
#writeLines(def_param)





path_data()






                    