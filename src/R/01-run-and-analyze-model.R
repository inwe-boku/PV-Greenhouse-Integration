library(tidyverse)
library(gdxtools)
# # # # #
# # # # #
# # # # # #### IF THIS DOES NOT WORK, GAMS DIRECTORY HAS TO BE SET MANUALLY
# # # # # #### E.G: i
#igdx("C:/GAMS/win64/30.2")
igdx(dirname(Sys.which('gams')))
setwd(paste0(dirname(rstudioapi::getActiveDocumentContext()$path),
                  "/../../")
)

############# CREATING INPUT DATA
input_dir <- "data/input/"
output_dir <- "data/output/"

source("src/R/functions.R")

### some basic test cases to check functionality
test_case_full_pv_supply()
test_case_controllable_demand_dispatch_single_hour()
test_case_controllable_demand_dispatch_multiple_hour()
test_case_controllable_demand_storage()


### a very simple random model with 8 hours

hours<-8
timesteps<-hours

##### random pv generation -> Here actual PV generation derived from ML model should be used.
pv <- runif(hours)

#### random demand -> Here actual demand derived from ML models hould be used
demand <- runif(hours)

#### controllable demand -> Here actual demand derived from ML models hould be used

## indicates in how many periods the whole time span is split
periods <- 4

## indicates the number of different controllable demands
dt <- 2

## assumption on average controllable demand for this example
avg_demand1 <- 100
avg_demand2 <- 200

## generate random controllable demand
controllable_demand1 <- (runif(periods) * avg_demand1) %>%
  tibble() %>%
  mutate(dt="p1") %>%
  mutate(d=paste0("d",1:periods))

controllable_demand2 <- (runif(periods) * avg_demand2) %>%
  tibble() %>%
  mutate(dt="p2",
         d=paste0("d",1:periods))

controllable_demand_full <- bind_rows(controllable_demand1,
                                      controllable_demand2)

names(controllable_demand_full) <- c("value",
                                     "dt",
                                     "d")

controllable_demand_in <- controllable_demand_full %>%
  dplyr::select(dt, d, value)

## how high is the power that can be dispatched in one time period?
maximum_power_controllable_demand <- c(500, 500)

## for each controllable demand, the periods (d1...dn) have to fit to the timesteps (t1...tn)
## this has to be defined for every controllable demand
## In this example, period 1 (d1) for the first controllable spans t1 and t2 and
## period 2 (d2) spans t3 only.

t_in_d_p1 <- tibble(dt=c("p1"),
                    d=c("d1", "d1","d2","d3","d3","d4","d4","d4"),
                    t=c("t1", "t2","t3","t4","t5","t6","t7","t8"),
                    value=c(1, 1, 1, 1, 1, 1, 1, 1))

t_in_d_p2 <- tibble(dt=c("p2"),
                    d=c("d1", "d1","d1","d2","d2","d3","d3","d4"),
                    t=c("t1", "t2","t3","t4","t5","t6","t7","t8"),
                    value=c(1, 1, 1, 1, 1, 1, 1, 1))

t_in_d <- bind_rows(t_in_d_p1,
                    t_in_d_p2)


#### investment conditions PV
interest_rate <- 0.1
run_time <- 20
pv_invest <- 1500 ##some random costs - should represent real costs

## annualized costs if model is run for one year only.
## Please observe: for shorter periods than one year, this annualization doesn't work well!
pv_invest_annualized <- annualize(pv_invest,
                                  interest_rate,
                                  run_time,
                                  timesteps)
#### investment conditions storage
interest_rate <- 0.1
run_time <- 10
storage_invest <- 600 # in €/kWh

## annualized costs if model is run for one year only.
## Please observe: for shorter periods than one year, this annualization doesn't work well!
storage_invest_annualized <- annualize(storage_invest,
                                       interest_rate,
                                       run_time,
                                       timesteps)
#### Cost of electricity taken from grid
#### in Autarky, this should be set to a very high value
gridcosts <- 0.18

#### feed in tariff received for electricity fed to the grid
#### if no feed-in is allowed, value should be 0
feed_in_tariff <- 0.06

#### technical parameters
## efficiency of battery storage
efficiency_storage <- 0.9

#### this function writes the gdx file to disk for GAMS to use
#### the function is contained in the script "model.R"
create_input_data(timesteps = timesteps,
                  demand_in = demand,
                  pv_gen = pv,
                  controllable_demand_in = controllable_demand_in,
                  pv_invest_annualized = pv_invest_annualized,
                  storage_invest_annualized = storage_invest_annualized,
                  gridcosts = gridcosts,
                  feed_in_tariff = feed_in_tariff,
                  efficiency_storage = efficiency_storage,
                  maximum_power_controllable_demand = maximum_power_controllable_demand,
                  t_in_d
)

############# Running gams
gams("src/GAMS/pvsimple.gms")

############# Reading results

mygdx <- gdx('data/output/output.gdx')

#### THESE 2 VALUES HAVE TO BE 1, otherwise there was a problem when solving!
#### if here a statement is printed, something went wrong and results should not be used.
if(mygdx["modelstat"] != 1){
  print("Caution: model was not solved properly, results should not be trusted.")
}
if(mygdx["solvestat"] != 1){
  print("Caution: model was not solved properly, results should not be trusted.")
}

############# show all available items in results
all_items(mygdx)

#### extract some interesting items
installed_pv_capacity <- mygdx["x_pv"]
installed_storage_capacity <- mygdx["x_storage"]
electricity_from_grid <- mygdx["x_buy_from_grid"]
sum_electricity_from_grid <- sum(electricity_from_grid$value)
costs <- mygdx["x_cost"]

timeseries <- read_timeseries_from_results(mygdx)

#### figure for storage operation
timeseries %>%
  filter(Var %in% c("SOC",
                    "x_in",
                    "x_out")) %>%
  ggplot(aes(x=time, y=Value)) +
  geom_line(aes(col=Var)) +
  scale_color_manual(values=c('dark green','orange','dark blue')) +
  labs(title = "Storage Balance", subtitle = " ", x = "day", y = "kWh")


mygdx["x_control_demand"] %>%
  mutate(Demand_type=V1) %>%
  group_by(V1) %>%
  mutate(t=1:n()) %>%
  ggplot(aes(x=t,y=value)) +
  geom_line(aes(col=Demand_type)) +
  xlab("Time") +
  ylab("Controllable demand dispatch")


#### figure for operation
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

#### hourly results
all %>%
  ggplot(aes(x = time, y = Value)) +
  geom_area(aes(fill = Var))+
  labs(title = "Energy Supply", subtitle = "hourly", y = "kWh")


#### figure for energy balancing amounts
all %>%
  group_by(Var)   %>%
  summarize(Value_Sum = sum(Value)) %>%
  ggplot(aes(x = Var, y = Value_Sum)) +
  geom_bar(stat = "Identity", aes (fill = Var)) +
  labs(title = "Energy balancing amounts", subtitle = "daily", x = "Energy 'sources'", y = "kWh")

#### figure for energy balancing amounts, results in percentage
s_demand <- sum(demand) + sum(controllable_demand_in$value)

all %>%
  group_by(Var)   %>%
  summarize(Value_Sum = sum(Value)) %>%
  ggplot(aes(x = Var, y = 100 * Value_Sum/s_demand)) +
  geom_bar(stat = "Identity", aes (fill = Var)) +
  labs(title = "Energy balancing amounts", subtitle = "daily", x = "Energy 'sources'", y = "% of Demand")

