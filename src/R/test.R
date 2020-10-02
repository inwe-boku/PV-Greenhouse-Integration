# run<-FALSE

if(!run){
  
  library(tidyverse)
  # # # # # #
  # # # # # #
  library(gdxtools)
  # # # # # #
  # # # # # #
  igdx(dirname(Sys.which('gams')))
  # # # # # # #### IF THIS DOES NOT WORK, GAMS DIRECTORY HAS TO BE SET MANUALLY
  # # # # # # #### E.G: i
  # # igdx("C:/GAMS/win64/30.2")
  
  # # # # # #
  setwd(paste0(dirname(rstudioapi::getActiveDocumentContext()$path),
               "/../../"))
  
  dir.create("results")
  
  run<-TRUE
}
############# CREATING INPUT DATA

results_dir <- "results/"
input_dir <- "data/input/"
output_dir <- "data/output/"

source("src/R/functions.R")


final_results     <- NULL

###Choice of UAS simulation   
VF <- FALSE                 #simulation VF
# GH <- FALSE               #simulation GH

###Choice of Scenario
base <- FALSE               #Base-Scenario
# autarky <- FALSE          #Autarky-Scenario
# mix <- FALSE              #Mix-Scenario



###INPUT VARIABLES###

#Base-Scenario
if(!base){ 
  
  PV.i <- 1000                #investment cost in Euro/kWp
  ES.i <- 600                 #investment cost in Euro/kWh
  G.i <- 0.17                 #grid costs in Euro/kWh
  fit.i <- 0.05               #feed-in-tariff in Euro/kWh
  
  base <- TRUE
}


#Autarky-Scenario  
if(!autarky){    
  
  PV.i <- 1000                #investment cost in Euro/kWp
  ES.i <- 600                 #investment cost in Euro/kWh
  G.i <- 0.17 *10000          #grid costs in Euro/kWh
  fit.i <- 0                  #feed-in-tariff in Euro/kWh
  
  autarky<-TRUE
}


#Mix-Scenario
if(!mix){    
  
  PV.i <- 1000                #investment cost in Euro/kWp
  ES.i <- 600 *0.65           #investment cost in Euro/kWh
  G.i <- 0.17*1.35            #grid costs in Euro/kWh
  fit.i <- 0.05               #feed-in-tariff in Euro/kWh
  
  mix<-TRUE
}

###INPUT-VARIABLES-CONTINUATION###
  VF.i <- 1173.5              #electricity demand in kWh/m2/a
  VF.p.area <- 720            #in m2, production area in the VF
  VF.area <- 100              #in m2, building's ground area VF
  VF.illumination <- 16       #in hours, illuminated hours per day in the VF
  VF_biomass.i <- 79.835      #in kg/m2/a, biomass productivity in the VF 
  VF_invest.i <- 571.97       #in Euro/m2, construction costs for the VF
  GH.area <- 400              #in m2, production area in the GH
  GH_biomass.i <- 4.5         #in kg/m2/a, biomass productivity in the GH
  GH_invest.i <- 262.77       #in Euro/m2/a, construction costs for the GH
  HP.i <- 30000               #in Euro, investment costs heat pump (HP)
  COP.HP.i <- 3.5             #Coefficient of performance (COP) of the HP
  interest.rate.i <- 0.04     #interest rate for annualizing investment costs
  PV_life <- 20               #life time of PV plant in years
  ES_life <- 10               #life time of the Energy Storage in years
  building_life <- 30         #life time of the VF and the GH building
  land.c.i <- 6.5             #Euro/m2 greenland
  co2.i <- 136.81             #co2 g/kWh
  kWp_area.i <- 15            #in m2, area for 1 kwp
  grid_area.i <- 0.005        #in m2/kWh, land use for grid
  es_area.i <- 0.006          #in m2/kWh, land use for energy storage
  
  
############################################################  

###MODEL - PREPARING DATA###

### PV: average pv generation for 2006 - 2016 in kw.
pvgis_data <- read.csv("data/input/PV_avg-06-16_hr.csv", 
                       header=TRUE, sep=";")         #PVGis average hourly data 2006-2016, in W
pv <- as.vector(pvgis_data$P..W.)/1000               #year avg in kW

pv <- c(pv[1:24],pv) 

timesteps <- length (pv)
days <- timesteps / 24



if(!VF){
  ### VF: average demand VF in kw.
  prod_area_VF <- VF.p.area                            #m2 actual production area which has to be illuminated
  energy_demand_VF <- VF.i                           #kWh/m2/a, includes illumination, irrigation, humidification, ventilation, ...
  photo_time <- VF.illumination                      #hours
  dark_time <- 24-photo_time                         #hours, during the dark period there is no illumination
  demand_day <- prod_area_VF*energy_demand_VF/365    #total energy demand in kW/h
  demand_photo <- (demand_day*0.7)/photo_time        #approx. 70 % of total energy demand accounts for illumination
  demand_dark <- (demand_day*0.3)/24
  demand_tot_VF <- demand_photo+demand_dark
  
  demand_ <- c(rep(demand_dark,dark_time/2),
               rep(demand_tot_VF, photo_time),
               rep(demand_dark,dark_time/2))         #kW in the course of one day
  demand <- c(rep(demand_, days-1))
  demand <- c(rep(0,24),demand)
  
  VF<-TRUE
}


if(!GH){
  ### GH: demand  in kw/m2
  GH_demand <- read.csv("data/input/GH-demand.csv", header=TRUE, sep=";")
  
  GH_energy_demand <- sum(GH_demand$Coldhouse)
  # GH_energy_demand <- sum(GH_demand$Hothouse)
  
  GH_lettuce <- as.vector(GH_demand$Coldhouse)
  # GH_tomato <- as.vector(GH_demand$Hothouse)
  
  GH_area <- GH.area                                  #m2
  COP.HP <- COP.HP.i                                  #Coefficient of performance (COP) of the heatpump (HP)
  
  GH_d <- GH_lettuce/COP.HP
  # GH_d <- GH_tomato/COP.HP
  
  GH_demand_ <- GH_d*GH_area
  demand <- c(rep(0,24),GH_demand_) 
  
  GH<-TRUE
}


# controllable demand
avg_demand <- 100
controllable_demand <- runif(days) * avg_demand * 0   #*days statt 0


### COSTS: investment costs

interest_rate <- interest.rate.i
run_time <- PV_life


# Land cost
land_cost <- land.c.i                                  #Euro/m2 greenland.


# PV costs    
pv_land <- kWp_area.i                                  #in m2 for 1 kWp

pv_invest <- PV.i + land_cost*pv_land # in Euro/kw
pv_invest_annualized <- annualize(pv_invest,
                                  interest_rate,
                                  run_time,
                                  timesteps)


# Storage costs   
run_time <- 10

storage_invest <- ES.i                                 # in Euro/kWh
storage_invest_annualized <- annualize(storage_invest,
                                       interest_rate,
                                       run_time,
                                       timesteps)


# Emission
co2.kWh <- co2.i                                       #co2 g/kWh


# Grid cost
gridcosts <- G.i                                       # power from grid in Euro/kWh

feed_in_tariff <- fit.i                                # subsidy received for feeding power to grid, Euro/kWh


# Technical parameters
efficiency_storage <- 0.95
maximum_power_controllable_demand <- 500               # how much power the controllable demand can  
# use at most in one instant of time. In kW


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


############# GAMS: Running gams
gams("src/GAMS/pvsimple.gms")

### Reading results
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
sum_pv <- mygdx["x_direct_use"]
sum_pv_use <- sum(sum_pv$value)
sum_storage_out <- mygdx["x_out"]
sum_storage <- sum(sum_storage_out$value)
sold_pv_energy <- mygdx["x_sell_to_grid"]
sum_pv_sold <- sum(sold_pv_energy$value)

timeseries <- read_timeseries_from_results(mygdx)



########FIGURES###########

### Figure for storage operation
timeseries %>%
  filter(Var %in% c("SOC",
                    "x_in",
                    "x_out")) %>%
  filter(time<72) %>%
  ggplot(aes(x=time, y=Value)) +
  geom_line(aes(col=Var)) +
  scale_color_manual(values=c('dark green','orange','dark blue')) +
  labs(title = "Storage Balance", subtitle = "1. and 2. January ", x = "hour", y = "kWh")+
  theme(plot.title = element_text(size = 18),
        plot.subtitle=element_text(size=16),
        axis.title.y = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 17.5),
        axis.text.x = element_text(size = 15),
        text = element_text(size = 20))   


### Figure for operation
demand_original <- timeseries %>%
  filter(Var %in% c("demand"))


controllable_original_demand <- timeseries %>%
  filter(Var %in% c("demand",
                    "control_demand")) %>%
  dplyr::select(time, Var, Value) %>%
  spread(Var, Value) %>%
  mutate(total_demand = control_demand + demand) %>%
  gather(Var, Value, -time) %>%
  filter(Var %in% c("total_demand", "demand"))


gens_positive <- timeseries %>%
  filter(Var %in% c("direct_use",
                    "grid_power",
                    "x_out"))


gens_negative <- timeseries %>%
  filter(Var %in% c("curtailment",
                    "power_fed_in"))

all <- bind_rows(
  gens_positive,
  gens_negative)

### Hourly results first 48 h
all %>%
  filter(time<48)%>%
  ggplot(aes(x = time, y = Value)) +
  geom_area(aes(fill = Var))+
  labs(title = "Energy Supply", subtitle = "1. and 2. January", y = "kWh")

### Hourly results longest day 21. June
day. <- c(rep(1:24))
all$day <- day.

all %>%
  filter(time<4129, time>4106) %>%
  ggplot(aes(x = day, y = Value)) +
  geom_area(aes(fill = Var))+
  labs(title = "Energy Supply", subtitle = "21.June", x= "hour", y = "kWh")+
  theme(plot.title = element_text(size = 18),
        plot.subtitle=element_text(size=16),
        axis.title.y = element_text(size = 17.5),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 17.5),
        axis.text.x = element_text(size = 15),
        text = element_text(size = 20))

### Hourly results shortest day 22. December
all %>%
  filter(time>8522, time<8545) %>%
  ggplot(aes(x = day, y = Value)) +
  geom_area(aes(fill = Var))+
  labs(title = "Energy Supply", subtitle = "22.December", x= "hour", y = "kWh")+
  theme(plot.title = element_text(size = 18),
        plot.subtitle=element_text(size=16),
        axis.title.y = element_text(size = 17.5),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 17.5),
        axis.text.x = element_text(size = 15),
        text = element_text(size = 20))

### Daily aggregation of results
all %>%
  group_by(Var) %>%
  mutate(Day = rep(1:days, each = 24)) %>%
  ungroup() %>%
  group_by(Day, Var) %>%
  summarize(Value = sum(Value)) %>%
  ggplot(aes(x = Day, y = Value)) +
  geom_area(aes(fill = Var))+
  labs(title = "Energy Balance", subtitle = "daily", y = "kWh")+
  theme(plot.title = element_text(size = 20),
        axis.title.y = element_text(size = 17.5),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 17.5),
        axis.text.x = element_text(size = 15),
        text = element_text(size = 20))


### Figure for energy balancing amounts
all %>%
  group_by(Var)   %>%
  summarize(Value_Sum = sum(Value)) %>%
  ggplot(aes(x = Var, y = Value_Sum)) +
  geom_bar(stat = "Identity", aes (fill = Var)) +
  labs(title = "Energy balancing amounts", x = "Energy 'sources'", y = "kWh")+
  theme(plot.title = element_text(size = 20),
        axis.title.y = element_text(size = 17.5),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 17.5),
        axis.text.x = element_text(size = 15),
        text = element_text(size = 20))+
  theme(axis.text.x = element_text(angle = 90))


### Results in percentage
s_demand <- sum(demand)

all %>%
  group_by(Var)   %>%
  summarize(Value_Sum = sum(Value)) %>%
  ggplot(aes(x = Var, y = 100 * Value_Sum/s_demand)) +
  geom_bar(stat = "Identity", aes (fill = Var)) +
  labs(title = "Energy balancing amounts", x = "Energy 'sources'", y = "% of Demand")+
  theme(text = element_text(size = 20), axis.text.x = element_text(size = 15))+
  theme(axis.text.x = element_text(angle = 90))


#######RESULTS#######
#### LAND_consumption
kWp_area <- kWp_area.i                                #m2/kWp
pv_area <- installed_pv_capacity*kWp_area             #m2
grid_area_consumption <- grid_area.i                  #m2/kWh
grid_area <- sum_electricity_from_grid*grid_area_consumption #m2
es_cap_area <- es_area.i                              #m2/kWh
es_area <- es_cap_area*installed_storage_capacity     #m2

### CO2 emissions
emissions.t <- (sum_electricity_from_grid*co2.kWh)/10^6 #tons


### Results data.frame
results <- data.frame(c("Demand",
                        "PV_costs",
                        "PV_capacity",
                        "PV_energy",
                        "ES_costs",
                        "ES_capacity",
                        "ES_out",
                        "Grid_costs",
                        "Grid",
                        "PV_sold",
                        "Costs",
                        "PV_area",
                        "Emissions"),
                      c(s_demand,
                        pv_invest_annualized,
                        installed_pv_capacity$value,
                        sum_pv_use,
                        storage_invest_annualized,
                        installed_storage_capacity$value,
                        sum_storage,
                        gridcosts,
                        sum_electricity_from_grid,
                        sum_pv_sold,
                        costs$value,
                        pv_area$value,
                        emissions.t),
                      c("kWh","Euro/kWp","kWp", "kWh", "Euro/kWh", "kWh","kWh", "Euro/kWh", "kWh","kWh", "Euro", "m2", "tons"))


names(results) <- c("parameters",
                    "values",
                    "units")
results


#### Economic considerations ###

### Economic considerations lettuce ####
run_time <- building_life                               #years

if(!VF){
  ### Economic considerations VF ####
  VF_productivity <- VF_biomass.i                       #kg/m2/a
  VF_area <- VF.area                                    #in m2
  VF_invest <- VF_invest.i*prod_area_VF                      #Euro/m2
  VF_invest_annualized <- annualize(VF_invest,
                                    interest_rate,
                                    run_time,
                                    timesteps)
  VF_tot_costs <- VF_invest_annualized + costs
  VF_productivity_per_a <- VF_productivity*prod_area_VF
  
  VF_productivity_per_a                                #in kg/a
  VF_tot_costs                                          #in Euro
  
  VF_econ <- VF_tot_costs/VF_productivity_per_a
  
  VF_econ                                              #in Euro/kg
  
  VF_emission <- (emissions.t/VF_productivity_per_a)*10^6 # in g/kg
  VF_emission

  results_VF <- data.frame(c("VF"),
                           c(VF_emission),
                           c((pv_area+es_area+grid_area+VF_area)/VF_productivity_per_a),
                           c(energy_demand_VF/VF_productivity),
                           c(VF_econ[1,1]))
  names(results_VF) <- c("UAS",
                         "CO2",
                         "Land consumption",
                         "Energy consumption",
                         "Average production costs")
  
  print(results_VF)
  
  VF<-TRUE
}


if(!GH){
  ### Economic considerations GH
  GH_area <- GH.area                                        #m2 production area
  GH_productivity <- GH_biomass.i                           #kg/m2/a
  HP_invest <- HP.i                                         #in Euro 
  GH_i <- GH_invest.i                                       #in Euro/m2 (construction costs)
  GH_invest <- (GH_i*GH_area)+HP_invest                     #in Euro
  GH_invest_annualized <- annualize(GH_invest,
                                    interest_rate,
                                    run_time,
                                    timesteps)
  GH_tot_costs <- GH_invest_annualized + costs
  GH_productivity_per_a <- GH_area*GH_productivity
  
  GH_tot_costs                                          #in Euro/a
  GH_productivity_per_a                                 #in kg/a
  
  GH_econ <- GH_tot_costs/GH_productivity_per_a         #in Euro/kg
  
  
  GH_emission <- (emissions.t/GH_productivity_per_a)*10^6 #in g/kg
  
  results_GH <- data.frame(c("GH"),
                           c(GH_emission),
                           c((pv_area+es_area+grid_area++GH_area)/GH_productivity_per_a),
                           c(GH_energy_demand/GH_productivity),
                           c(GH_econ[1,1]))
  names(results_GH) <- c("UAS",
                         "CO2",
                         "Land consumption",
                         "Energy consumption",
                         "Average production costs")  

  final_results <- bind_rows(final_results, results_GH)

  GH <-TRUE
}


  ### OFC ###
  ofc_area <- 1/3.28                        #in m2/kg
  ofc_energy <- 0.2                         #in kWh/kg
  ofc_emission <- 50                        #in g/kg   
  results_OFC <- data.frame("OFC",ofc_emission,ofc_area,ofc_energy,1.32)
  names(results_OFC) <- c("UAS",
                          "CO2",
                          "Land consumption",
                          "Energy consumption",
                          "Average production costs")

final_results <- bind_rows(final_results,results_OFC)


### Results for comparison ###  

final_results

results

save.image(file = "Image.RData")