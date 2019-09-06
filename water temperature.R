temp <- read_excel("data/raw.xlsx", sheet = "Balcony temp")

# Water temperature on Marine Biotech balcony
temp %>% 
  group_by(`Trial number`) %>%    
  summarize(mean_temp=mean(`Temp, °C`, na.rm=TRUE))
