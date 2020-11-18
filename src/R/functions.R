annualize<-function(invest,
                    interest_rate,
                    run_time,
                    timesteps = 8760){

  annuity_factor <- (1 + interest_rate)^run_time * interest_rate / ((1 + interest_rate)^run_time - 1)

  return (invest * annuity_factor / 8760 * (timesteps))
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
                            maximum_power_controllable_demand,
                            t_in_d
                            ){

  time_ <- paste0("t", 1:timesteps)

  ############# we assume that load can be flexibly distributed within one day
  ############# so controllable load is distributed within one day.
  days_ <- t_in_d$d %>% unique()



  ############# this demand is fixed and can't be changed, i.e. no demand response
  demand <- tibble(t = time_,
                 value = demand_in)

  ############# this demand is meant to be shifted within one day
  controllable_demand <- controllable_demand_in

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
  technical_parameters_set <- c("efficiency_storage") # in kW

  technical_parameters <- data.frame(index = technical_parameters_set,
                                   value = c(efficiency_storage))


  dt_ <- controllable_demand$dt %>% unique() %>% unlist()

  time.df <- data.frame(t = time_)
  time.df$t <- factor(time.df$t, time.df$t)

  costs_parameters.df <- data.frame(c = cost_parameters)
  costs_parameters.df$c <- factor(costs_parameters.df$c, costs_parameters.df$c)

  days.df <- data.frame(d = days_)
  days.df$d <- factor(days.df$d, days.df$d)

  dt.df <- data.frame(dt = dt_)
  dt.df$dt <- factor(dt.df$dt, dt.df$dt)

  technologies_parameters.df <- data.frame(tech = technical_parameters_set)
  technologies_parameters.df$tech <- factor(technologies_parameters.df$tech, technologies_parameters.df$tech)

  maximum_power_controllable_demand <- data.frame(dt=dt_,
                                                  value=maximum_power_controllable_demand)

  write2.gdx(paste0(input_dir,
                  "input.gdx"),
           list(demand = demand,
                pv_production = pv_production,
                costs = costs,
                controllable_demand = controllable_demand,
                t_in_d = t_in_d,
                technical_parameters = technical_parameters,
                max_power_controllable_load = maximum_power_controllable_demand),
           list(
                t  = time.df,
                d  = days.df,
                dt = dt.df,
                c  = costs_parameters.df,
                tech_param = technologies_parameters.df))

  print("File for GAMS run input.gdx written")

}

read_timeseries_from_results<-function(mygdx){

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
    mutate(Var = "control_demand") %>%
    group_by(V2, Var) %>%
    summarize(value=sum(value)) %>%
    ungroup() %>%
    mutate(V1=V2) %>%
    dplyr::select(V1,value,Var)

  operation_demand <- mygdx["demand"] %>%
    mutate(Var = "demand") %>%
    mutate(V1 = t)


  pv_output <- mygdx["x_pv"][1, 1] * as.numeric(mygdx["pv_production"][,2])

  operation_pv <- mygdx["pv_production"]  %>%
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
    mutate(time = as.numeric(str_replace(V1, "t", "")))  %>%
    dplyr::select(time, Var, value) %>%
    spread(Var, value) %>%
    gather(Var, Value, -time) %>%
    mutate(Value = ifelse(is.na(Value), 0, Value))


  return(timeseries)

}


test_case_full_pv_supply <- function() {


  hours <- 8

  ############# average pv generation for 2006 - 2016 in kw.
  pv <- 1

  timesteps <- length (pv)

  ############# average demand VF in kw.
  demand <- rep(1, hours)

  #####controllable demand

  ### set to number of controllable demands
  periods <- 4
  dt <- 2

  avg_demand1 <- 0
  avg_demand2 <- 0

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

  controllable_demand_full <- controllable_demand_full %>%
    dplyr::select(dt, d, value)

  maximum_power_controllable_demand <- c(500, 500) # how much power the controllable demand can use at most in one instant of time. In kW

  ###8 hours, 4 periods

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


  #########investment costs

  interest_rate <- 0.1
  run_time <- 20

  pv_invest <- 1500 ##some ranodm costs

  pv_invest_annualized <- annualize(pv_invest,
                                    interest_rate,
                                    run_time,
                                    timesteps)
  run_time <- 10

  storage_invest <- 600 # in €/kWh
  storage_invest_annualized <- annualize(storage_invest,
                                         interest_rate,
                                         run_time,
                                         timesteps)
  # run_time <- 30
  # GH_invest <- 1185000
  # GH_invest_annualized <- annualize(GH_invest,
  #                                   interest_rate,
  #                                   run_time,
  #                                   timesteps)


  #Emission cost
  co2.price <- 15.5/10^6  #co2 price Euro/g
  co2.kWh <- 100.27      #co2 g/kWh
  co2 <- co2.price * co2.kWh

  #Grid cost
  gridcosts <- 10000 + co2 # power from grid in €/kWh
  # gridcosts <- gridcosts*mult

  feed_in_tariff <- 0 # subsidy received for feeding power to grid, Euro/kWh

  #technical parameters
  efficiency_storage <- 0

  create_input_data(timesteps = timesteps,
                    demand_in = demand,
                    pv_gen = pv,
                    controllable_demand_in = controllable_demand_full,
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

  if(mygdx["modelstat"] != 1){
    print("Caution: model was not solved properly, results should not be trusted.")
  }
  if(mygdx["solvestat"] != 1){
    print("Caution: model was not solved properly, results should not be trusted.")
  }

  installed_pv_capacity <- mygdx["x_pv"]

  if(installed_pv_capacity == 1){
    print("test case passed")
  }else{
    print("test case failed")

  }



}



test_case_controllable_demand_dispatch_single_hour <- function() {


  hours <- 8

  ############# average pv generation for 2006 - 2016 in kw.
  pv <- c(0, 0, 0, 0, 1, 0, 0, 0)

  timesteps <- length (pv)

  ############# average demand VF in kw.
  demand <- rep(0, hours)

  #####controllable demand

  ### set to number of controllable demands
  periods <- 4
  dt <- 1


  controllable_demand_full <- c(0, 0, 1, 0) %>%
    tibble() %>%
    mutate(dt="p1") %>%
    mutate(d=paste0("d",1:periods))

  names(controllable_demand_full) <- c("value",
                                       "dt",
                                       "d")

  controllable_demand_full <- controllable_demand_full %>%
    dplyr::select(dt, d, value)

  maximum_power_controllable_demand <- c(1) # how much power the controllable demand can use at most in one instant of time. In kW

  ###8 hours, 4 periods

  t_in_d <- tibble(dt=c("p1"),
                      d=c("d1", "d2","d3","d3","d3","d3","d3","d4"),
                      t=c("t1", "t2","t3","t4","t5","t6","t7","t8"),
                      value=c(1, 1, 1, 1, 1, 1, 1, 1))

  #########investment costs

  interest_rate <- 0.1
  run_time <- 20

  pv_invest <- 1500 ##some ranodm costs

  pv_invest_annualized <- annualize(pv_invest,
                                    interest_rate,
                                    run_time,
                                    timesteps)
  run_time <- 10

  storage_invest <- 600 # in €/kWh
  storage_invest_annualized <- annualize(storage_invest,
                                         interest_rate,
                                         run_time,
                                         timesteps)
  # run_time <- 30
  # GH_invest <- 1185000
  # GH_invest_annualized <- annualize(GH_invest,
  #                                   interest_rate,
  #                                   run_time,
  #                                   timesteps)


  #Emission cost
  co2.price <- 15.5/10^6  #co2 price Euro/g
  co2.kWh <- 100.27      #co2 g/kWh
  co2 <- co2.price * co2.kWh

  #Grid cost
  gridcosts <- 10000 + co2 # power from grid in €/kWh
  # gridcosts <- gridcosts*mult

  feed_in_tariff <- 0 # subsidy received for feeding power to grid, Euro/kWh

  #technical parameters
  efficiency_storage <- 0

  create_input_data(timesteps = timesteps,
                    demand_in = demand,
                    pv_gen = pv,
                    controllable_demand_in = controllable_demand_full,
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

  if(mygdx["modelstat"] != 1){
    print("Caution: model was not solved properly, results should not be trusted.")
  }
  if(mygdx["solvestat"] != 1){
    print("Caution: model was not solved properly, results should not be trusted.")
  }

  installed_pv_capacity <- mygdx["x_pv"]
  control_demand <- mygdx["x_control_demand"]

  condition_control <- (control_demand$value[5] == 1) && (sum(control_demand$value) == 1) && (sum(control_demand$value < 0)==0)

  if(installed_pv_capacity == 1 && condition_control){
    print("test case passed")
  }else{
    print("test case failed")

  }



}

test_case_controllable_demand_dispatch_multiple_hour <- function() {


  hours <- 8

  ############# average pv generation for 2006 - 2016 in kw.
  pv <- c(0, 0, 0, 0, 1, 1, 0, 0)

  timesteps <- length (pv)

  ############# average demand VF in kw.
  demand <- rep(0, hours)

  #####controllable demand

  ### set to number of controllable demands
  periods <- 4
  dt <- 1


  controllable_demand_full <- c(0, 0, 1, 0) %>%
    tibble() %>%
    mutate(dt="p1") %>%
    mutate(d=paste0("d",1:periods))

  names(controllable_demand_full) <- c("value",
                                       "dt",
                                       "d")

  controllable_demand_full <- controllable_demand_full %>%
    dplyr::select(dt, d, value)

  maximum_power_controllable_demand <- c(0.5) # how much power the controllable demand can use at most in one instant of time. In kW

  ###8 hours, 4 periods

  t_in_d <- tibble(dt=c("p1"),
                   d=c("d1", "d2","d3","d3","d3","d3","d3","d4"),
                   t=c("t1", "t2","t3","t4","t5","t6","t7","t8"),
                   value=c(1, 1, 1, 1, 1, 1, 1, 1))

  #########investment costs

  interest_rate <- 0.1
  run_time <- 20

  pv_invest <- 1500 ##some ranodm costs

  pv_invest_annualized <- annualize(pv_invest,
                                    interest_rate,
                                    run_time,
                                    timesteps)
  run_time <- 10

  storage_invest <- 600 # in €/kWh
  storage_invest_annualized <- annualize(storage_invest,
                                         interest_rate,
                                         run_time,
                                         timesteps)
  # run_time <- 30
  # GH_invest <- 1185000
  # GH_invest_annualized <- annualize(GH_invest,
  #                                   interest_rate,
  #                                   run_time,
  #                                   timesteps)


  #Emission cost
  co2.price <- 15.5/10^6  #co2 price Euro/g
  co2.kWh <- 100.27      #co2 g/kWh
  co2 <- co2.price * co2.kWh

  #Grid cost
  gridcosts <- 10000 + co2 # power from grid in €/kWh
  # gridcosts <- gridcosts*mult

  feed_in_tariff <- 0 # subsidy received for feeding power to grid, Euro/kWh

  #technical parameters
  efficiency_storage <- 0

  create_input_data(timesteps = timesteps,
                    demand_in = demand,
                    pv_gen = pv,
                    controllable_demand_in = controllable_demand_full,
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

  if(mygdx["modelstat"] != 1){
    print("Caution: model was not solved properly, results should not be trusted.")
  }
  if(mygdx["solvestat"] != 1){
    print("Caution: model was not solved properly, results should not be trusted.")
  }

  installed_pv_capacity <- mygdx["x_pv"]
  control_demand <- mygdx["x_control_demand"]

  condition_control <- (control_demand$value[5] == 0.5 && control_demand$value[6] == 0.5) && (sum(control_demand$value) == 1) && (sum(control_demand$value < 0)==0)

  if(installed_pv_capacity == 0.5 && condition_control){
    print("test case passed")
  }else{
    print("test case failed")

  }



}

test_case_controllable_demand_storage <- function() {


  hours <- 8

  ############# average pv generation for 2006 - 2016 in kw.
  pv <- c(1, 1, 0, 0, 0, 0, 0, 0)

  timesteps <- length (pv)

  ############# average demand VF in kw.
  demand <- rep(0, hours)

  #####controllable demand

  ### set to number of controllable demands
  periods <- 4
  dt <- 1


  controllable_demand_full <- c(0, 0, 1, 0) %>%
    tibble() %>%
    mutate(dt="p1") %>%
    mutate(d=paste0("d",1:periods))

  names(controllable_demand_full) <- c("value",
                                       "dt",
                                       "d")

  controllable_demand_full <- controllable_demand_full %>%
    dplyr::select(dt, d, value)

  maximum_power_controllable_demand <- c(0.5) # how much power the controllable demand can use at most in one instant of time. In kW

  ###8 hours, 4 periods

  t_in_d <- tibble(dt=c("p1"),
                   d=c("d1", "d2","d2","d2","d3","d3","d3","d4"),
                   t=c("t1", "t2","t3","t4","t5","t6","t7","t8"),
                   value=c(1, 1, 1, 1, 1, 1, 1, 1))

  #########investment costs

  interest_rate <- 0.1
  run_time <- 20

  pv_invest <- 1500 ##some ranodm costs

  pv_invest_annualized <- annualize(pv_invest,
                                    interest_rate,
                                    run_time,
                                    timesteps)
  run_time <- 10

  storage_invest <- 600 # in €/kWh
  storage_invest_annualized <- annualize(storage_invest,
                                         interest_rate,
                                         run_time,
                                         timesteps)
  # run_time <- 30
  # GH_invest <- 1185000
  # GH_invest_annualized <- annualize(GH_invest,
  #                                   interest_rate,
  #                                   run_time,
  #                                   timesteps)


  #Emission cost
  co2.price <- 15.5/10^6  #co2 price Euro/g
  co2.kWh <- 100.27      #co2 g/kWh
  co2 <- co2.price * co2.kWh

  #Grid cost
  gridcosts <- 10000 + co2 # power from grid in €/kWh
  # gridcosts <- gridcosts*mult

  feed_in_tariff <- 0 # subsidy received for feeding power to grid, Euro/kWh

  #technical parameters
  efficiency_storage <- 1

  create_input_data(timesteps = timesteps,
                    demand_in = demand,
                    pv_gen = pv,
                    controllable_demand_in = controllable_demand_full,
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

  if(mygdx["modelstat"] != 1){
    print("Caution: model was not solved properly, results should not be trusted.")
  }
  if(mygdx["solvestat"] != 1){
    print("Caution: model was not solved properly, results should not be trusted.")
  }

  installed_pv_capacity <- mygdx["x_pv"]
  control_demand <- mygdx["x_control_demand"]
  x_storage <- mygdx["x_storage"]

  condition_pv <- installed_pv_capacity == 0.5
  condition_control <- (control_demand$value[5] == 0.5 && control_demand$value[6] == 0.5) && (sum(control_demand$value) == 1) && (sum(control_demand$value < 0)==0)
  condition_storage <- x_storage == 1

  if(condition_pv && condition_control && condition_storage){
    print("test case passed")
  }else{
    print("test case failed")

  }



}





