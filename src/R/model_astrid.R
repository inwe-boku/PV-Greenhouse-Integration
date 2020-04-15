library(tidyverse)

##### These 2 lines have to be run only once!
library(devtools)
#install_github('lolow/gdxtools')

library(gdxtools)


#### IF THIS DOES NOT WORK, GAMS DIRECTORY HAS TO BE SET MANUALLY
#### E.G: igdx("C:/GAMS/win64/30.2")
igdx(dirname(Sys.which('gams')))

setwd(paste0(dirname(rstudioapi::getActiveDocumentContext()$path),
             "/../../")
)

############# CREATING INPUT DATA

input_dir <- "data/input/"
output_dir <- "data/output/"

source("src/R/functions.R")


timesteps <- 24*2

############# average pv generation for random generation in kw. random generation should be replaced by real production data.
pvgis_data <- read.csv("data/input/PV_2016_hr.csv")        #PVGis hourly data for 2016

pv_data <- as.vector(pvgis_data$X0[1: timesteps])       #4609:4681 -> 11.-13.Juli
#pv_data <- as.vector(pvgis_data$X0)

#timesteps <- length(pv_data)


# avg_pv <- 0.1
# pv <- runif(timesteps) * avg_pv

interest_rate <- 0.1
run_time <- 20

pv_invest <- 1200 # in €/kw
pv_invest_annualized <- annualize(pv_invest,
                                  interest_rate,
                                  run_time,
                                  timesteps)

run_time <- 10

storage_invest <- 800 # in €/kWh
storage_invest_annualized <- annualize(storage_invest,
                                       interest_rate,
                                       run_time,
                                       timesteps)

############# average demand for random generation in kw. random generation should be replaced by real load data
avg_demand <- 500
demand <- runif(timesteps) * avg_demand

controllable_demand <- runif(timesteps / 24) * avg_demand * 24




gridcosts <- 0.18 # power from grid in €/kWh

feed_in_tariff <- 0.06 # subsidy received for feeding power to grid

efficiency_storage <- 0.9
maximum_power_controllable_demand <- 1000 # how much power the controllable demand can use at most in one instant of time. In kW

#### this function writes the gdx file to disk for GAMS to use
#### the function is contained in the script "model.R"
create_input_data(timesteps = timesteps,
                  demand_in = demand,
                  pv_gen = pv_data,
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

installed_pv_capacity
installed_storage_capacity
sum_electricity_from_grid

timeseries <- read_timeseries_from_results()

###### figure for storage operation
timeseries %>%
  filter(Var %in% c("SOC",
                    "x_in",
                    "x_out")) %>%
  ggplot(aes(x=time, y=value)) +
  geom_line(aes(col=Var))

###### figure for operation
demand_original <- timeseries %>%
  filter(Var %in% c("demand"
    ))

controllable_original_demand <- timeseries %>%
  filter(Var %in% c("demand",
                    "control_demand"
  )) %>%
  dplyr::select(time, Var, value) %>%
  spread(Var, value) %>%
  mutate(total_demand = control_demand + demand) %>%
  gather(Var, value, -time) %>%
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

all %>% ggplot(aes(x = time, y = value)) +
  geom_area(aes(fill = Var)) +
  geom_line(data = controllable_original_demand, aes(col = Var), fill = NA, size = 1)

