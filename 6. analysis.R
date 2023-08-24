########### Libraries
library(lme4) 
library(arm) 
library(tidyverse)
library(stats)
library(RPostgreSQL)
library(dplyr)
library(sqldf)
library(car)
library(stargazer)
library(ggResidpanel)
library(reshape2)
library(DescTools)
library(MASS)
library(patchwork)
library(sf)
library(spatstat)
library(tmap)
library(geojsonio)

########### Load data, relevel, and cut 
data <- read_csv('full_dataset.csv')
print(boxcox(data$price ~ 1))
#take natural log
### Relevelling
#propertytype: D=detached S=semi-d T=terraced F=flats
data$propertytype <- factor(data$propertytype)
data$propertytype <- relevel(data$propertytype, ref = "F") 

#const_age_band: invalid! as ref
data$CONSTRUCTION_AGE_BAND <- factor(data$CONSTRUCTION_AGE_BAND)
data$CONSTRUCTION_AGE_BAND <- relevel(data$CONSTRUCTION_AGE_BAND, ref = "INVALID!") 

#SecCatchment: BACA as ref
data$SecCatchment <- factor(data$SecCatchment)
data$SecCatchment <- relevel(data$SecCatchment, ref = "Aldridge") 


#nearest rail name: not near rail as ref
data$nearest_rail_name <- factor(data$nearest_rail_name)
data$nearest_rail_name <- relevel(data$nearest_rail_name, ref = "Not near rail") 

#railwalktime: not near rail as ref
data$rail_walk_time <- factor(data$rail_walk_time)
data$rail_walk_time <- relevel(data$rail_walk_time, ref = "Not near rail") 

##nearestpriname: City Academy Whitehawk as ref
data$nearest_pri_name <- factor(data$nearest_pri_name)
data$nearest_pri_name <- relevel(data$nearest_pri_name, ref = "City Academy Whitehawk") 

##nearestsecname: BACA as ref
data$nearest_sec_name <- factor(data$nearest_sec_name)
data$nearest_sec_name <- relevel(data$nearest_sec_name, ref = "Brighton Aldridge Community Academy") 

##cc_walk_time: 
data$cc_walk_time <- factor(data$cc_walk_time)
data$cc_walk_time <- relevel(data$cc_walk_time, ref = "Not near city centre") 

##cc_bus_time: 
data$cc_bus_time <- factor(data$cc_bus_time)
data$cc_bus_time <- relevel(data$cc_bus_time, ref = "Not near city centre") 

##X2019_imd_decile
data$X2019_imd_decile <- factor(data$X2019_imd_decile)
data$X2019_imd_decile <- relevel(data$X2019_imd_decile, ref = 1) 

##YearSold_Good_Pri_Sch_WalkTime 
data$YearSold_Good_Pri_Sch_WalkTime <- factor(data$YearSold_Good_Pri_Sch_WalkTime)
data$YearSold_Good_Pri_Sch_WalkTime <- relevel(data$YearSold_Good_Pri_Sch_WalkTime, ref = "Not near good pri school")
##YearSold_Good_Sec_Sch_WalkTime
data$YearSold_Good_Sec_Sch_WalkTime <- factor(data$YearSold_Good_Sec_Sch_WalkTime)
data$YearSold_Good_Sec_Sch_WalkTime <- relevel(data$YearSold_Good_Sec_Sch_WalkTime, ref = "Not near good sec school")

##oac11 (Supergroups)
# 1     2     3     4     5     6     7     8 
# 200 44905  1242  2562 19116  8208  2470  2949 
data$oac11 <- factor(data$oac11)
data$oac11 <- relevel(data$oac11, ref = "1")
###electoralwards
data$ElectWards <- factor(data$ElectWards)
data$ElectWards <- relevel(data$ElectWards, ref = "Goldsmid") 

###### Data subsetting
##trim dataset by removing outliers (for q-q plot)
data_filtered <- data %>%
  group_by(year) %>%
  mutate(price_lower_bound = quantile(price, 0.05),
         price_upper_bound = quantile(price, 0.95)) %>%
  filter(price >= price_lower_bound, price <= price_upper_bound) %>%
  ungroup()
## subset the data for the years 2000 to 2007
#df_pre <- subset(data, year >= 2000 & year <= 2007)
df_filtered_pre <- subset(data_filtered, year >= 2000 & year <= 2007)
##35256
## subset the data for the years 2008 to 2019
#df_post <- subset(data, year >= 2008 & year <= 2019)
df_filtered_post <- subset(data_filtered, year >= 2008 & year <= 2019)
##38303

############ Descriptive Statistics
# price vs ln price
data_filtered$lnprice <- log(data_filtered$price)
hist_price <- ggplot(data_filtered, aes(x = price)) +
  geom_histogram(binwidth = 5000, fill = "blue", color = "black") +
  labs(title = "Histogram of Price",
       x = "Price",
       y = "Frequency") +
  theme_minimal()

hist_lnprice <- ggplot(data_filtered, aes(x = lnprice)) +
  geom_histogram(binwidth = 0.05, fill = "blue", color = "black") +
  labs(title = "Histogram of Log Price",
       x = "Log Price",
       y = "Frequency") +
  theme_minimal() 
hist_price + hist_lnprice + plot_layout(ncol = 2)

### tf_area, numberrooms, epc
hist_tfarea <- ggplot(data_filtered, aes(x = tfarea)) +
  geom_histogram(fill = "blue", color = "black") +
  labs(title = "Histogram of Total Floor Area",
       x = "Total Floor Area",
       y = "Frequency") +
  theme_minimal() 
hist_numberrooms <- ggplot(data_filtered, aes(x = numberrooms)) +
  geom_histogram(bins = 15, fill = "blue", color = "black") +
  labs(title = "Histogram of Number of rooms",
       x = "Number of rooms",
       y = "Frequency") +
  theme_minimal() 
hist_epc <- ggplot(data_filtered, aes(x = CURRENT_ENERGY_EFFICIENCY)) +
  geom_histogram(fill = "blue", color = "black") +
  labs(title = "Histogram of Current EPC",
       x = "Current EPC",
       y = "Frequency") +
  theme_minimal() 
hist_rail <- ggplot(data_filtered, aes(x = walk_nearest_rail)) +
  geom_histogram(fill = "blue", color = "black") +
  labs(title = "Histogram of Walk time to nearest rail station",
       x = "Walk time to nearest rail station",
       y = "Frequency") +
  theme_minimal() 
hist_sec <- ggplot(data_filtered, aes(x = walk_nearest_sec)) +
  geom_histogram(fill = "blue", color = "black") +
  labs(title = "Histogram of Walk time to nearest secondary school",
       x = "Walk time to nearest secondary school",
       y = "Frequency") +
  theme_minimal() 
hist_pri <- ggplot(data_filtered, aes(x = walk_nearest_pri)) +
  geom_histogram(fill = "blue", color = "black") +
  labs(title = "Histogram of Walk time to nearest primary school",
       x = "Walk time to nearest primary school",
       y = "Frequency") +
  theme_minimal() 

hist_tfarea + hist_numberrooms + hist_epc + 
  hist_rail + hist_sec + hist_pri + plot_layout(ncol = 3)


### Barplots
bar_prop <- ggplot(data_filtered, aes(x=propertytype)) + 
  geom_bar(fill = "skyblue") + 
  labs(title = "Property Types", x = "Property Type", y = "Count") + 
  theme_minimal() + 
  scale_x_discrete(labels = c("Flat", "Detached", "Semi-Detached", "Terrace")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

constage_temp <- data_filtered %>% 
  filter(CONSTRUCTION_AGE_BAND != "England and Wales: 2007-2011" & CONSTRUCTION_AGE_BAND != "England and Wales: 2012 onwards")
custom_order <- c("England and Wales: before 1900", "England and Wales: 1900-1929", 
                  "England and Wales: 1930-1949", "England and Wales: 1950-1966", 
                  "England and Wales: 1967-1975", "England and Wales: 1976-1982",
                  "England and Wales: 1983-1990", "England and Wales: 1991-1995",
                  "England and Wales: 1996-2002", "England and Wales: 2003-2006",
                  "England and Wales: 2007 onwards", "INVALID!")
constage_temp <- constage_temp %>%
  mutate(CONSTRUCTION_AGE_BAND = factor(CONSTRUCTION_AGE_BAND, levels = custom_order))
bar_const <- ggplot(constage_temp, aes(x = CONSTRUCTION_AGE_BAND)) + 
  geom_bar(fill = "skyblue") + 
  labs(title = "Construction Age Bands", x = "Age Bands", y = "Count") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

## Rail descriptive: nearest_rail_name
bar_rail <- ggplot(data_filtered, aes(x=nearest_rail_name)) + 
  geom_bar(fill = "skyblue") + 
  labs(title = "Houses near a rail station", x = "Rail Stations", y = "Count") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

## City Centre
custom_order_cc <- c("0-5_mins", "5-10_mins", "10-15_mins",
                  "15-20_mins", "20-25_mins", "25-30_mins", "Not near city centre")
data_filtered$cc_walk_time <- factor(data_filtered$cc_walk_time, levels = custom_order_cc)
bar_cc <- ggplot(data_filtered, aes(x=cc_walk_time)) + 
  geom_bar(fill = "skyblue") + 
  labs(title = "Walk time to the city centre", x = "Walk Time Bands", y = "Count") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

#SecCatchment
bar_sc <- ggplot(data_filtered, aes(x=SecCatchment)) + 
  geom_bar(fill = "skyblue") + 
  labs(title = "Secondary school catchments", x = "Catchment Areas", y = "Count") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
#YearSold Pri
custom_order_pri <- c("0-5_mins", "5-10_mins", "10-15_mins",
                  "15-20_mins", "20-25_mins", "25-30_mins", "Not near good pri school")
data_filtered$YearSold_Good_Pri_Sch_WalkTime <- factor(data_filtered$YearSold_Good_Pri_Sch_WalkTime, levels = custom_order_pri)
bar_pri <- ggplot(data_filtered, aes(x=YearSold_Good_Pri_Sch_WalkTime)) + 
  geom_bar(fill = "skyblue") + 
  labs(title = "Proximity to a good primary school", x = "Walk times", y = "Count") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

#YearSold Sec
custom_order_sec <- c("0-5_mins", "5-10_mins", "10-15_mins",
                      "15-20_mins", "20-25_mins", "25-30_mins", "Not near good sec school")
data_filtered$YearSold_Good_Sec_Sch_WalkTime <- factor(data_filtered$YearSold_Good_Sec_Sch_WalkTime, levels = custom_order_sec)
bar_sec <- ggplot(data_filtered, aes(x=YearSold_Good_Sec_Sch_WalkTime)) + 
  geom_bar(fill = "skyblue") + 
  labs(title = "Proximity to a good secondary school", x = "Walk times", y = "Count") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

##IMD
bar_imd <- ggplot(data_filtered, aes(x=X2019_imd_decile)) + 
  geom_bar(fill = "skyblue") + 
  labs(title = "IMD deciles (2019)", x = "IMD Deciles", y = "Count") + 
  theme_minimal()

bar_oac <- ggplot(data_filtered, aes(x=oac11)) + 
  geom_bar(fill = "skyblue") + 
  labs(title = "OAC supergroups (2011)", x = "OAC supergroups", y = "Count") + 
  theme_minimal() + 
  scale_x_discrete(labels = c("Rural residents", "Cosmopolitans", "Ethnicity central",
                              "Multicultural metropolitans", "Urbanites", "Suburbanites",
                              "Constrained city dwellers", "Hard-pressed living")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

bar_year <- ggplot(data_filtered, aes(x=year)) + 
  geom_bar(fill = "skyblue") + 
  labs(title = "Years (as categorical variable)", x = "Years", y = "Count") + 
  theme_minimal()

bar_wards <- ggplot(data_filtered, aes(x=ElectWards)) + 
  geom_bar(fill = "skyblue") + 
  labs(title = "Electoral Wards", x = "Ward names", y = "Count") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

#combine
bar_year + bar_prop + bar_const + bar_rail + 
  bar_cc + bar_sc + bar_sec + bar_pri + 
  bar_imd + bar_oac + plot_layout(ncol = 5)
#tilt everything except proprtype and imd deciles


########### Modelling and VIF checks
options(digits=5,scipen=10)

####### Dependent Variable of log(price)
###### Property-Level (all) compared to Property-Level + Contextual (all)
model1 <- lm(log(price) ~ as.factor(year) + propertytype + numberrooms + CONSTRUCTION_AGE_BAND + CURRENT_ENERGY_EFFICIENCY + log(tfarea), data = data_filtered)
summary(model1)
resid_panel(model1)
vif(model1)

modelbase <- lm(log(price) ~ as.factor(year) + propertytype + numberrooms + CONSTRUCTION_AGE_BAND + CURRENT_ENERGY_EFFICIENCY + log(tfarea) + cc_bus_time + nearest_rail_name + log(walk_nearest_rail) + walk_nearest_sec + walk_nearest_pri +  as.factor(X2019_imd_decile), data = data_filtered)
summary(modelbase)
vif(modelbase)
resid_panel(modelbase)

#basemodel + catchment
model2catch <- lm(log(price) ~ as.factor(year) + propertytype + numberrooms + CONSTRUCTION_AGE_BAND + CURRENT_ENERGY_EFFICIENCY + log(tfarea) + cc_bus_time + nearest_rail_name + log(walk_nearest_rail) +  walk_nearest_pri + walk_nearest_sec + as.factor(X2019_imd_decile) + SecCatchment, data = data_filtered)
summary(model2catch)
vif(model2catch)
#0.864

#basemodel + proximity to schools
model2schools <- lm(log(price) ~ as.factor(year) + propertytype + numberrooms + CONSTRUCTION_AGE_BAND + CURRENT_ENERGY_EFFICIENCY + log(tfarea) + cc_bus_time + nearest_rail_name + log(walk_nearest_rail) + walk_nearest_pri + walk_nearest_sec + as.factor(X2019_imd_decile) + YearSold_Good_Sec_Sch_WalkTime + YearSold_Good_Pri_Sch_WalkTime, data = data_filtered)
summary(model2schools)
vif(model2schools)
#0.862

#basemodel + electoral wards
model2wards <- lm(log(price) ~ as.factor(year) + propertytype + numberrooms + CONSTRUCTION_AGE_BAND + CURRENT_ENERGY_EFFICIENCY + log(tfarea) + cc_bus_time + nearest_rail_name + log(walk_nearest_rail) + walk_nearest_pri + walk_nearest_sec + as.factor(X2019_imd_decile) + ElectWards, data = data_filtered)
summary(model2wards)
vif(model2wards)
#0.867
#### modelbase + all
model2all <- lm(log(price) ~ as.factor(year) + propertytype + numberrooms + CONSTRUCTION_AGE_BAND + CURRENT_ENERGY_EFFICIENCY + log(tfarea) + cc_bus_time + nearest_rail_name + log(walk_nearest_rail) + walk_nearest_pri + walk_nearest_sec + as.factor(X2019_imd_decile) + 
                  SecCatchment + YearSold_Good_Sec_Sch_WalkTime + YearSold_Good_Pri_Sch_WalkTime + ElectWards, data = data_filtered)
summary(model2all)
#0.868
#### best model, for whole dataset
model2bestlog <- lm(log(price) ~ as.factor(year) + propertytype + numberrooms + CONSTRUCTION_AGE_BAND + CURRENT_ENERGY_EFFICIENCY + log(tfarea) + cc_bus_time + nearest_rail_name + log(walk_nearest_rail) + walk_nearest_pri + walk_nearest_sec + as.factor(X2019_imd_decile) + 
                  YearSold_Good_Sec_Sch_WalkTime + YearSold_Good_Pri_Sch_WalkTime + ElectWards, data = data_filtered)
summary(model2bestlog)
vif(model2bestlog)


######All Variables (pre vs post)
##pre-reform
model3a <- lm(log(price) ~ as.factor(year) + propertytype + numberrooms + CONSTRUCTION_AGE_BAND + CURRENT_ENERGY_EFFICIENCY + log(tfarea) + cc_bus_time + nearest_rail_name + log(walk_nearest_rail) + walk_nearest_pri + walk_nearest_sec + as.factor(X2019_imd_decile) + 
                 YearSold_Good_Sec_Sch_WalkTime + YearSold_Good_Pri_Sch_WalkTime + ElectWards, data = df_filtered_pre)
summary(model3a)
vif(model3a)
model3asec <- lm(log(price) ~ as.factor(year) + propertytype + numberrooms + CONSTRUCTION_AGE_BAND + CURRENT_ENERGY_EFFICIENCY + log(tfarea) + cc_bus_time + nearest_rail_name + log(walk_nearest_rail) + walk_nearest_pri + walk_nearest_sec + as.factor(X2019_imd_decile) + 
                YearSold_Good_Sec_Sch_WalkTime + ElectWards, data = df_filtered_pre)
summary(model3asec)
resid_panel(model3a)

#post-reform
model3b <- lm(log(price) ~ as.factor(year) + propertytype + numberrooms + CONSTRUCTION_AGE_BAND + CURRENT_ENERGY_EFFICIENCY + log(tfarea) + cc_bus_time + nearest_rail_name + log(walk_nearest_rail) + walk_nearest_pri + walk_nearest_sec + as.factor(X2019_imd_decile) + 
                YearSold_Good_Sec_Sch_WalkTime + YearSold_Good_Pri_Sch_WalkTime + ElectWards, data = df_filtered_post)
summary(model3b)
model3bsec <- lm(log(price) ~ as.factor(year) + propertytype + numberrooms + CONSTRUCTION_AGE_BAND + CURRENT_ENERGY_EFFICIENCY + log(tfarea) + cc_bus_time + nearest_rail_name + log(walk_nearest_rail) + walk_nearest_pri + walk_nearest_sec + as.factor(X2019_imd_decile) + 
                YearSold_Good_Sec_Sch_WalkTime + ElectWards, data = df_filtered_post)
summary(model3bsec)
vif(model3b)
resid_panel(model3b)
#compare coefficients with stargazer
stargazer(model3a, model3b, title = "Comparing Pre-Post-Reform (Property-Level and Geographical Variables)",
          align = TRUE, single.row = TRUE, type = "text")

#compare residual plots among all 3 regressions
resid_compare(models = list(model2bestlog, 
                            model3a, 
                            model3b),
              smoother = FALSE,
              qqbands = TRUE,
              title.opt = FALSE)



######Geo objects
#### house points and brighton 
###write residuals back into dataframe
data_filtered$resids1 <- model1$residuals
data_filtered$resids2 <- model2bestlog$residuals
df_filtered_pre$resids2a <- model2a$residuals
df_filtered_post$resids2b <- model2b$residuals
data_filtered <- data_filtered %>%
  mutate(latitude = X, longitude = geometry) %>%
  select(-X, -geometry)

data_filtered$latitude <- as.numeric(gsub("[^0-9.-]", "", data_filtered$latitude))
data_filtered$longitude <- as.numeric(gsub("[^0-9.-]", "", data_filtered$longitude))

points_sf <- st_as_sf(data_filtered, coords = c("longitude", "latitude"))
#write df to shp for plotting in qgis 
st_write(points_sf, "house_points2.shp")
brighton_lsoa <- st_read('geo/brighton_lsoa.geojson')
houses <- st_read('geo/house_points.shp', crs=4326)
houses <- houses %>%
  st_transform(., crs=27700)

##check crs
st_crs(houses)
st_crs(brighton_lsoa)

### points lsoa
points_lsoa <- st_join(houses, brighton_lsoa, join=st_intersects)

avg_prices <- points_lsoa %>%
  group_by(year, LSOA21CD) %>%
  summarize(avg_price = mean(price))

lsoa_avg_prices <- st_join(brighton_lsoa, avg_prices, by = "LSOA21CD")

filter_lsoa_avg_prices <- lsoa_avg_prices %>%
  filter(year >= 2000 & year <= 2019)
lsoa_avg_prices_2019 <- lsoa_avg_prices %>%
  filter(year == 2019)
#plot
plots <- lapply(unique(filter_lsoa_avg_prices$year), function(year) {
  p <- ggplot(filter_lsoa_avg_prices %>% filter(year == year)) +
    geom_sf(aes(fill = avg_price)) +
    scale_fill_viridis_c() +
    labs(title = paste("(", year, ")", sep = ""), fill = "Average Price") +
    theme_minimal()
  return(p)
})

plots_arranged <- wrap_plots(plots, ncol = 4)
plots_arranged

ggplot(lsoa_avg_prices_2019) +
  geom_sf( aes(fill=avg_price)) +
  scale_fill_viridis_c() +  
  labs(title = "Average House Prices by LSOA (2019)", fill = "Average Price") +
  theme_minimal()  
###############
background <- tm_shape(brighton) +
  tm_borders(lwd = 0) +
  tm_fill("lightgray")
houses_within <- points[brighton, ,op=st_intersects]


### KDE
window <- as.owin(brighton)
plot(window)

houses_sp<- houses_within %>%
  as(., 'Spatial')
#create a ppp object
houses_sp.ppp <- ppp(x=houses_sp@coords[,1],
                        y=houses_sp@coords[,2],
                        window=window)
houses_sp.ppp %>%
  density(., sigma=100) %>%
  plot(col = viridis(100), 
       main = "Kernel Density Estimation Map of Houses",
       colorkey = list(at = seq(0, 0.01, by = 0.001), space = "bottom"))





