library(tidyverse)

##### These 2 lines have to be run only once!
library(devtools)
install_github('lolow/gdxtools')

library(gdxtools)

#dirname(Sys.which('gams'))
igdx("C:/GAMS/win64/30.2")

setwd(paste0(dirname(rstudioapi::getActiveDocumentContext()$path),
             "/../../")
)

############# Creating input data

input_dir <- "data/input/"
output_dir <- "data/output/"

timesteps<-3

time_<-paste0("t", 1:timesteps)

demand<-tibble(t=time_,
               value=rep(1000, timesteps))

pv_production<-tibble(c=time_,
                      value=c(60, 40, 20))

cost_parameters<-c("investment_costs_PV",
                   "investment_costs_Storage",
                   "grid_buy_costs",
                   "grid_sell_price")

costs<-tibble(index=cost_parameters,
              value=c(12, 4, 0.6, 0.1))

time.df <- data.frame(t = time_)
costs_parameters.df <- data.frame(c = cost_parameters)

write2.gdx(paste0(input_dir,
            "input.gdx"),
            list(demand = demand,
                 pv_production = pv_production,
                 costs = costs),
           list(t = time.df,
                c = costs_parameters.df))


############# Running gams
gams("src/GAMS/pvsimple.gms")

############# Reading results

mygdx <- gdx('data/output/output.gdx')

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

storage_soc <- mygdx["x_soc"] %>%
  mutate(Var = "SOC")

storage_in <- mygdx["x_in"] %>%
  mutate(Var = "x_in")

storage_out <- mygdx["x_out"] %>%
  mutate(Var = "x_out")

operation_pv_use <- mygdx["x_direct_use"] %>%
  mutate(Var = "direct_use")

operation_grid_power <- mygdx["x_buy_from_grid"] %>%
  mutate(Var = "grid_power")

operation_feed_to_grid_power <- mygdx["x_sell_to_grid"] %>%
  mutate(Var = "power_fed_in")

operation_curtailment <- mygdx["x_curtailment"] %>%
  mutate(Var = "curtailment")

pv_output <- mygdx["x_pv"][1, 1] * as.numeric(mygdx["pv_production"][,2])

operation_pv <- mygdx["demand"]  %>%
  mutate(Var = "pv") %>%
  mutate(value = pv_output) %>%
  mutate(V1 = t) %>%
  dplyr::select(V1, value, Var)


timeseries <- bind_rows(storage_soc,
                        storage_in,
                        storage_out,
                        operation_pv_use,
                        operation_grid_power,
                        operation_curtailment,
                        operation_feed_to_grid_power,
                        operation_pv) %>%
  mutate(time = as.numeric(str_replace(V1, "t", "")))

###### figure for storage operation
timeseries %>%
  filter(Var %in% c("SOC",
                    "x_in",
                    "x_out")) %>%
  ggplot(aes(x=time, y=value)) +
  geom_line(aes(col=Var))

###### figure for operation
timeseries %>%
  filter(Var %in% c("direct_use",
                    "grid_power",
                    "curtailment",
                    "x_out",
                    "power_fed_in",
                    "pv")) %>%
  ggplot(aes(x=time, y=value)) +
  geom_line(aes(col=Var))



