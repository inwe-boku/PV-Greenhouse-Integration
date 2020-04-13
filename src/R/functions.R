annualize<-function(invest,
                    interest_rate,
                    run_time,
                    timesteps = 8760){

  annuity_factor <- (1 + interest_rate)^run_time * interest_rate / ((1 + interest_rate)^run_time - 1)

  return (invest * annuity_factor /8760 * (timesteps))
}


create_input_data<-function(timesteps,
                            demand_in,
                            pv_gen,
                            controllable_demand_in,
                            pv_invest_annualized,
                            storage_invest_annualized,
                            efficiency_storage,
                            feed_in_tariff,
                            gridcosts,
                            maximum_power_controllable_demand
                            ){

  time_ <- paste0("t", 1:timesteps)

  ############# we assume that load can be flexibly distributed within one day
  ############# so controllable load is distributed within one day.
  days_ <- paste0("d", 1:(timesteps / 24))

  ############# controls which hours is in which day
  t_in_d <- data.frame(t = time_,
                     d = rep(days_, each =24),
                     value = 1)


  ############# this demand is fixed and can't be changed, i.e. no demand response
  demand <- tibble(t = time_,
                 value = demand_in)

  ############# this demand is meant to be shifted within one day
  controllable_demand <- tibble(t = days_,
                              value = controllable_demand_in)


  ############# pv production
  pv_production<-tibble(c = time_,
                      value = pv_gen)

  ############# parameters which defines costs and prices in simulation
  ############# in €/KW (annualized) or in €/kWh
  cost_parameters<-c("investment_costs_PV",
                   "investment_costs_Storage",
                   "grid_buy_costs",
                   "grid_sell_price")




  costs<-tibble(index = cost_parameters,
              value = c(pv_invest_annualized,
                        storage_invest_annualized,
                        gridcosts,
                        feed_in_tariff))




  ############# technical specifications.
  technical_parameters_set <- c("efficiency_storage",
                              "max_power_controllable_load") # in kW

  technical_parameters <- data.frame(index = technical_parameters_set,
                                   value = c(efficiency_storage,
                                             maximum_power_controllable_demand))



  time.df <- data.frame(t = time_)
  costs_parameters.df <- data.frame(c = cost_parameters)
  days.df <- data.frame(d = days_)
  technologies_parameters.df <- data.frame(tech = technical_parameters_set)

  write2.gdx(paste0(input_dir,
                  "input.gdx"),
           list(demand = demand,
                pv_production = pv_production,
                costs = costs,
                controllable_demand = controllable_demand,
                t_in_d = t_in_d,
                technical_parameters = technical_parameters),
           list(t = time.df,
                d = days.df,
                c = costs_parameters.df,
                tech_param = technologies_parameters.df))

  print("File for GAMS run input.gdx written")

}

read_timeseries_from_results<-function(){

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
    mutate(Var = "power_fed_in") %>%
    mutate(value = -1 * value) %>%
    mutate(value = ifelse(value >= 0, -0.00001, value))

  operation_curtailment <- mygdx["x_curtailment"] %>%
    mutate(Var = "curtailment") %>%
    mutate(value = -1 * value) %>%
    mutate(value = ifelse(value >= 0, -0.00001, value))

  operation_x_control_demand <- mygdx["x_control_demand"] %>%
    mutate(Var = "control_demand")

  operation_demand <- mygdx["demand"] %>%
    mutate(Var = "demand") %>%
    mutate(V1 = t)


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
                          operation_pv,
                          operation_x_control_demand,
                          operation_demand) %>%
    mutate(time = as.numeric(str_replace(V1, "t", "")))

  return(timeseries)

}
