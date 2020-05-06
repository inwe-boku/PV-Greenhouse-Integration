 library(tidyverse)
# # #
# # # ##### These 2 lines have to be run only once!
# # # library(devtools)
# # # #install_github('lolow/gdxtools')
# # #
   library(gdxtools)
# # #
# # #
# # # #### IF THIS DOES NOT WORK, GAMS DIRECTORY HAS TO BE SET MANUALLY
# # # #### E.G: i
#  #igdx("C:/GAMS/win64/30.2")
   igdx(dirname(Sys.which('gams')))
# # #
 setwd(paste0(dirname(rstudioapi::getActiveDocumentContext()$path),
                "/../../")
   )

############# CREATING INPUT DATA

input_dir <- "data/input/"
output_dir <- "data/output/"

source("src/R/functions.R")

############# pv generation for 2016 in kw.
# pvgis_data <- read.csv("data/input/PV_2016_hr.csv", header=FALSE)        #PVGis hourly data for 2016
#
# pv <- as.vector(pvgis_data$V1[1:48])/1000              #Winter: 1:48 -> 1.-2.January
# # pv <- as.vector(pvgis_data$V1[2521:2568])/1000         #Spring: 2521:2568 -> 15.-16.April
# # pv <- as.vector(pvgis_data$V1[4609:4656])/1000         #Summer: 4609:4656 -> 11.-12.July
# # pv <- as.vector(pvgis_data$V1[6577:6624])/1000         #Fall: 6577:6624 -> 1.-2. October
#
# # pv <- as.vector(pvgis_data$V1)/1000                    #year 2016


############# average pv generation for 2006 - 2016 in kw.
pvgis_data <- read.csv("data/input/PV_avg-06-16_hr.csv", header=TRUE, sep=";") #PVGis average hourly data 2006-2016

pv <- as.vector(pvgis_data$P..W.[1:48])/1000              #Winter: 1:48 -> 1.-2.January
# pv <- as.vector(pvgis_data$P..W.[2521:2568])/1000         #Spring: 2521:2568 -> 15.-16.April
# pv <- as.vector(pvgis_data$P..W.[4609:4656])/1000         #Summer: 4609:4656 -> 11.-12.July
# pv <- as.vector(pvgis_data$P..W.[6577:6624])/1000         #Fall: 6577:6624 -> 1.-2. October

# pv <- as.vector(pvgis_data$P..W.)/1000                 #year avg



timesteps <- length (pv)
days <- timesteps / 24



############# average demand in kw.
prod_area_VF <- 720                                                        #m2 actual production area which has to be illuminated
energy_demand_VF <- 1234.15                                                #kWh/m2/a
photo_time <- 16                                                           #hours
dark_time <- 24-photo_time                                                            #hours
demand_tot_VF <- prod_area_VF*energy_demand_VF/365/photo_time                      #total energy demand in kW/h

avg_demand <- 100
demand_ <- c(rep(0,dark_time/2), rep(demand_tot_VF, photo_time), rep(0,dark_time/2))                       #kW for a production area of 720 m2 in the course of one day
demand <- c(rep(demand_, days))

# avg_demand <- 100                       #kW for a production area of 720 m2
# demand <- c(rep(avg_demand, timesteps))

controllable_demand <- runif(days) * avg_demand *0



interest_rate <- 0.1
run_time <- 20

#Land cost
land_cost <- 6.5                                  #Euro/m2 greenland.

pv_land <- 0                                     #1 KW needs more than 1 m2!, so it has to be multiplied by land-use of PV

pv_invest <- 1200 + land_cost*pv_land # in €/kw
pv_invest_annualized <- annualize(pv_invest,
                                  interest_rate,
                                  run_time,
                                  timesteps)

run_time <- 10

storage_invest <- 200 # in €/kWh
storage_invest_annualized <- annualize(storage_invest,
                                       interest_rate,
                                       run_time,
                                       timesteps)

#Emission cost
co2.price <- 15.5/10^6  #co2 price Euro/g
co2.kWh <- 100.27      #co2 g/kWh
co2 <- co2.price * co2.kWh

#Grid cost
gridcosts <- 0.18 + co2 # power from grid in €/kWh

feed_in_tariff <- 0.06 # subsidy received for feeding power to grid, Euro/kWh

#technical parameters
efficiency_storage <- 0.9
maximum_power_controllable_demand <- 500 # how much power the controllable demand can use at most in one instant of time. In kW


#### this function writes the gdx file to disk for GAMS to use
#### the function is contained in the script "model.R"
create_input_data(timesteps = timesteps,
                  demand_in = demand,
                  pv_gen = pv,
                  controllable_demand_in = controllable_demand,
                  pv_invest_annualized = pv_invest_annualized,
                  storage_invest_annualized = storage_invest_annualized,
                  gridcosts = gridcosts,
                  feed_in_tariff = feed_in_tariff,
                  efficiency_storage = efficiency_storage,
                  maximum_power_controllable_demand = maximum_power_controllable_demand
)

############# Running gams
gams("src/GAMS/pvsimple.gms")

############# Reading results

mygdx <- gdx('data/output/output.gdx')

###### THESE 2 VALUES HAVE TO BE 1, otherwise there was a problem when solving!
if(mygdx["modelstat"] != 1){
  print("Caution: model was not solved properly, results should not be trusted.")
}
if(mygdx["solvestat"] != 1){
  print("Caution: model was not solved properly, results should not be trusted.")
}

############# show all available items in results
all_items(mygdx)

############# extract some interesting items
installed_pv_capacity<-mygdx["x_pv"]
installed_storage_capacity<-mygdx["x_storage"]
electricity_from_grid<-mygdx["x_buy_from_grid"]
sum_electricity_from_grid<-sum(electricity_from_grid$value)
costs <- mygdx["x_cost"]

timeseries <- read_timeseries_from_results(mygdx)

###### figure for storage operation
timeseries %>%
  filter(Var %in% c("SOC",
                    "x_in",
                    "x_out")) %>%
  ggplot(aes(x=time, y=Value)) +
  geom_line(aes(col=Var)) +
  scale_color_manual(values=c( 'orange','dark green','dark blue')) %>%
  labs(title = "Storage Balance", subtitle = " ", x = "day", y = "kWh")

###### figure for operation
demand_original <- timeseries %>%
  filter(Var %in% c("demand"
  ))

controllable_original_demand <- timeseries %>%
  filter(Var %in% c("demand",
                    "control_demand"
  )) %>%
  dplyr::select(time, Var, Value) %>%
  spread(Var, Value) %>%
  mutate(total_demand = control_demand + demand) %>%
  gather(Var, Value, -time) %>%
  filter(Var %in% c("total_demand", "demand"))


gens_positive <- timeseries %>%
  filter(Var %in% c("direct_use",
                    "grid_power",
                    "x_out"
  ))

gens_negative <- timeseries %>%
  filter(Var %in% c("curtailment",
                    "power_fed_in"))

all <- bind_rows(
  gens_positive,
  gens_negative)

###hourly results
all %>%
  ggplot(aes(x = time, y = Value)) +
  geom_area(aes(fill = Var))+
  labs(title = "Energy Supply", subtitle = "hourly", y = "kWh")


###daily aggregation of results
all %>%
  group_by(Var) %>%
  mutate(Day = rep(1:days, each = 24)) %>%
  ungroup() %>%
  group_by(Day, Var) %>%
  summarize(Value = sum(Value)) %>%
  ggplot(aes(x = Day, y = Value)) +
  geom_area(aes(fill = Var))+
  labs(title = "Energy Balance", subtitle = "daily", y = "kWh")




#figure for energy balancing amounts
all %>%
  group_by(Var)   %>%
  summarize(Value_Sum = sum(Value)) %>%
  ggplot(aes(x = Var, y = Value_Sum)) +
  geom_bar(stat = "Identity", aes (fill = Var)) +
  labs(title = "Energy balancing amounts", subtitle = "daily", x = "Energy 'sources'", y = "kWh")



##results in percentage
s_demand <- sum(demand)

all %>%
  group_by(Var)   %>%
  summarize(Value_Sum = sum(Value)) %>%
  ggplot(aes(x = Var, y = 100 * Value_Sum/s_demand)) +
  geom_bar(stat = "Identity", aes (fill = Var)) +
  labs(title = "Energy balancing amounts", subtitle = "daily", x = "Energy 'sources'", y = "% of Demand")


#Solution
installed_pv_capacity
installed_storage_capacity
sum_electricity_from_grid
costs

save.image(file = "Image.RData")

# sum_all <- sum(all$Value)
#
# agg_all <- all %>%
#   group_by(Var) %>%
#   summarise(Value = sum(Value))
#
# percentage <- agg_all$Value/sum_all*100


#####economic considerations####
retail_price <- 3.39                              #Euro/kg Salat
productivity <- 100                               #kg/m2/a
revenue <- retail_price*productivity*prod_area_VF #Euro/a

