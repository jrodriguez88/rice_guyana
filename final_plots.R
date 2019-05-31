
### Guyana plot weather
library(lubridate)
library(plotly)

all_data %>% unnest(data) %>%
    group_by(year(date), month(date), Loc, Region) %>%
    summarise(sum(rain)) %>% ungroup() %>% setNames(c("year", "month", "loc", "region", "rain")) %>%
    ggplot(aes(month, rain, color=factor(year))) + 
    geom_line()+
    geom_point(aes(color=factor(year))) +
    stat_summary(aes(group=month), fun.data = mean_cl_boot) + 
    scale_x_continuous(breaks = 1:12, labels=month.abb) +
    theme_bw() +
    facet_wrap(loc ~ .)


summary_df <- all_data %>% unnest(data) %>%
    group_by(year(date), month(date), Loc, Region) %>%
    summarise(sum(rain), mean(tmax), mean(tmin), mean(srad)) %>% 
    ungroup() %>% 
    setNames(c("year", "month", "loc", "region", "rain", "tmax", "tmin", "srad"))

write.csv(summary_df, "summary.csv")


### Plot all variables and localities
summary_df %>%
    gather("var", "value", -c(1:4)) %>%
    ggplot(aes(month, value, color=factor(year))) + 
    geom_line()+
#    geom_point(aes(color=factor(year))) +
    stat_summary(aes(group=month), fun.data = mean_cl_boot) + 
    scale_x_continuous(breaks = 1:12, labels=month.abb) +
    theme_bw() +
    facet_grid(var ~ loc, scales = "free") +
    theme(
        axis.text.x = element_text(angle = 90),
#        legend.position="bottom",
#        legend.title = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background=element_rect(fill="white", size=1.5, linetype="solid"),
        strip.text = element_text(face = "bold")) +  
    labs(x = "Month", 
         y= "Value - rain(mm), srad(MJ/m²d), temp(oC)", 
         title = "Guyana Climate Data",
         color = "Year")

summary_df %>%
    gather("var", "value", -c(1:4)) %>%
    ggplot(aes(month, value, color=factor(loc))) + 
    geom_jitter(aes(color=factor(loc))) +
    stat_summary(aes(group=month), fun.data = mean_cl_boot) + 
    stat_summary(aes(group=loc), fun.y = mean, geom='line', size=1) +
    scale_x_continuous(breaks = 1:12, labels=month.abb) +
    theme_bw() +
    facet_grid(var ~ region, scales = "free") +
    theme(
        axis.text.x = element_text(angle = 90),
        #        legend.position="bottom",
        #        legend.title = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background=element_rect(fill="white", size=1.5, linetype="solid"),
        strip.text = element_text(face = "bold")) +  
    labs(x = "Month", 
         y= "Value - rain(mm), srad(MJ/m²d), temp(oC)", 
         title = "Guyana Climate Data",
         color = "Locality")

    
all_data %>% filter(Loc=="Boerasire") %>% unnest(data) %>%
    group_by(year(date), month(date)) %>%
    summarise(sum(rain)) %>% ungroup() %>% 
    setNames(c("year", "month", "rain")) %>% 
    mutate(month = factor(month.abb[month], levels = month.abb)) %>%
    ggplot(aes(month, rain)) +
    geom_boxplot(aes(group=month), outlier.shape = NA) +
    geom_jitter(aes(color=factor(year))) +
#    geom_boxplot(aes(group=month), outlier.shape = NA) +
#    geom_line(aes(color=factor(year))) + theme_bw() +
#    scale_x_continuous(breaks = 0:12) +
    theme_bw()



