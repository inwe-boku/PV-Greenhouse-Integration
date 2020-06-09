# # run<-FALSE
# 
# if(!run){
# 
#   library(tidyverse)
#   # # # # # #
#   # # # # # #
#   library(gdxtools)
#   # # # # # #
#   # # # # # #
#   igdx(dirname(Sys.which('gams')))
#   # # # # # # #### IF THIS DOES NOT WORK, GAMS DIRECTORY HAS TO BE SET MANUALLY
#   # # # # # # #### E.G: i
#   # # igdx("C:/GAMS/win64/30.2")
# 
#   # # # # # #
#   setwd(paste0(dirname(rstudioapi::getActiveDocumentContext()$path),
#                "/../../"))
# 
#   dir.create("results")
# 
#   run<-TRUE
# }
# ############# CREATING INPUT DATA
# 
# results_dir <- "results/"
# input_dir <- "data/input/"
# output_dir <- "data/output/"
# 
# source("src/R/functions.R")
# 
# 
# #scenarios
# 
# final_results     <- NULL
# 
# scenarios_pv      <- c(0.8, 1, 1.5)
# scenarios_storage <- seq(0.5,1.5, 0.5)
# scenarios_grid    <- seq(0.5,1.5, 0.5)
# 
# 
# for(pv_mult in scenarios_pv){
#   for (storage_mult in scenarios_storage) {
#     for (grid_mult in scenarios_grid) {
# 
#       print("####storage####")
#       print(pv_mult)
#       print(storage_mult)
#       print(grid_mult)


      ############# average pv generation for 2006 - 2016 in kw.
      pvgis_data <- read.csv("data/input/PV_avg-06-16_hr.csv", header=TRUE, sep=";") #PVGis average hourly data 2006-2016

      # pv <- as.vector(pvgis_data$P..W.[1:48])/1000              #Winter: 1:48 -> 1.-2.January
      # pv <- as.vector(pvgis_data$P..W.[2521:2568])/1000         #Spring: 2521:2568 -> 15.-16.April
      # pv <- as.vector(pvgis_data$P..W.[4609:4656])/1000         #Summer: 4609:4656 -> 11.-12.July
      # pv <- as.vector(pvgis_data$P..W.[6577:6624])/1000         #Fall: 6577:6624 -> 1.-2. October

      pv <- as.vector(pvgis_data$P..W.)/1000                 #year avg


      # pv <- as.vector(pvgis_data$P..W.[7500:7667])/1000

      timesteps <- length (pv)
      days <- timesteps / 24



      ############# average demand VF in kw.
      prod_area_VF <- 720 #*mult                                                        #m2 actual production area which has to be illuminated
      energy_demand_VF <- 1009.53                                                #kWh/m2/a, includes illumination, irrigation, humidification, ventilation, ...
      photo_time <- 16                                                           #hours
      dark_time <- 24-photo_time                                                 #hours, during the dark period there is no illumination
      demand_day <- prod_area_VF*energy_demand_VF/365                            #total energy demand in kW/h
      demand_photo <- (demand_day*0.7)/photo_time                                #approx. 70 % of total energy demand accounts for illumination
      demand_dark <- (demand_day*0.3)/24
      demand_tot_VF <- demand_photo+demand_dark
      
      demand_ <- c(rep(demand_dark,dark_time/2), 
                   rep(demand_tot_VF, photo_time), 
                   rep(demand_dark,dark_time/2))                                           #kW in the course of one day
      demand <- c(rep(demand_, days))


      # ########### demand GH in kw/m2
      # GH_demand <- read.csv("data/input/GH-demand.csv", header=TRUE, sep=";")
      #
      # GH_area <- 400           #m2
      # COP.HP <- 3.5                                                     #Coefficient of performance (COP) of the heatpump (HP)


      # GH_lettuce <- as.vector(GH_demand$Coldhouse)
      #
      # # GH_lettuce <- as.vector(GH_demand$Coldhous[1:48])              #Winter: 1:48 -> 1.-2.January
      # # GH_lettuce <- as.vector(GH_demand$Coldhous[2521:2568])         #Spring: 2521:2568 -> 15.-16.April
      # # GH_lettuce <- as.vector(GH_demand$Coldhous[4609:4656])         #Summer: 4609:4656 -> 11.-12.July
      # # GH_lettuce <- as.vector(GH_demand$Coldhous[6577:6624])         #Fall: 6577:6624 -> 1.-2. October
      #
      # # GH_lettuce <- as.vector(GH_demand$Coldhous[7500:7667])
      #
      # GH_d <- GH_lettuce/COP.HP
      #
      #
      # # GH_tomato <- as.vector(GH_demand$Hothouse)
      #
      #
      #
      # GH_demand_ <- GH_d*GH_area
      # demand <- GH_demand_ #* mult



      #####controllable demand
      avg_demand <- 100
      controllable_demand <- runif(days) * avg_demand * 0   #*days statt 0


      #########investment costs

      interest_rate <- 0.04
      run_time <- 20

      #Land cost
      land_cost <- 6.5                                  #Euro/m2 greenland.
      build_land_cost <- 200                            #Euro/m2

      pv_land <- 7.5                                     #1 KW needs more than 1 m2!, so it has to be multiplied by land-use of PV

      pv_invest <- 1000 + land_cost*pv_land # in €/kw
      pv_invest_annualized <- annualize(pv_invest,
                                        interest_rate,
                                        run_time,
                                        timesteps)
      # pv_invest_annualized <- pv_invest_annualized*pv_mult

      run_time <- 10

      storage_invest <- 600 # in €/kWh
      storage_invest_annualized <- annualize(storage_invest,
                                             interest_rate,
                                             run_time,
                                             timesteps)
      # storage_invest_annualized <- storage_invest_annualized*storage_mult

     

      #Emission cost
      co2.price <- 15.5/10^6  #co2 price Euro/g
      co2.kWh <- 125.91      #co2 g/kWh
      co2 <- co2.price * co2.kWh

      #Grid cost
      gridcosts <- 0.199 + co2 # power from grid in €/kWh
      # gridcosts <- gridcosts*grid_mult

      feed_in_tariff <- 0.05 # subsidy received for feeding power to grid, Euro/kWh

      #technical parameters
      efficiency_storage <- 0.95
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
        scale_color_manual(values=c('dark green','orange','dark blue')) +
        labs(title = "Storage Balance", subtitle = " ", x = "day", y = "kWh")

      ###daily aggregation of storage
      timeseries %>%
        filter(Var %in% c("SOC",
                          "x_in",
                          "x_out")) %>%
        group_by(Var) %>%
        mutate(Day = rep(1:days, each = 24)) %>%
        ungroup() %>%
        group_by(Day, Var) %>%
        summarize(Value = sum(Value)) %>%
        ggplot(aes(x = Day, y = Value)) +
        geom_area(aes(fill = Var))+
        scale_fill_manual(values=c('dark green','orange','dark blue')) +
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


      ####PV_area_consumption
      kWp_area <- 7.5                                       #m2
      pv_area <- installed_pv_capacity*kWp_area

      ###co2 emissions
      emissions.t <- (sum_electricity_from_grid*co2.kWh)/10^6 #tons

      ground_area <- 100   #m2


      save.image(file = "Image.RData")


      results <- data.frame(c("Demand",
                              "PV_costs",
                              "PV_capacity",
                              "ES_costs",
                              "ES_capacity",
                              "Grid_costs",
                              "Grid",
                              "Costs",
                              "PV_area",
                              "Emissions"),
                            c(s_demand,
                              pv_invest_annualized,
                              installed_pv_capacity$value,
                              storage_invest_annualized,
                              installed_storage_capacity$value,
                              gridcosts,
                              sum_electricity_from_grid,
                              costs$value,
                              pv_area$value,
                              emissions.t),
                            c("kWh","Euro/kWp","kWp","Euro/kWh", "kWh", "Euro/kWh", "kWh", "Euro", "m2", "tons"),
                            # pv_mult,
                            # storage_mult,
                            # grid_mult)
      names(results) <- c("parameters",
                          "values",
                          "units")
                          # ,
                          # "pv_cost_scenario",
                          # "storage_cost_scenario",
                          # "grid_cost_scenario")

#       final_results <- bind_rows(final_results, results)
# 
#     }
#   }
# }
#   
#   #####economic considerations lettuce####
#   # retail_price <- 2.83                              #Euro/kg Salat
#   # run_time <- 30                                    #years
#   
#   
#   ###economic considerations VF####
#   # VF_productivity <- 79.835                         #kg/m2/a 
#   # VF_invest <- 571.97*prod_area_VF                  #Euro/m2
#   # VF_invest_annualized <- annualize(VF_invest,
#   #                                   interest_rate,
#   #                                   run_time,
#   #                                   timesteps)
#   #revenue <- retail_price*VF_productivity*prod_area_VF #Euro/a
#   
#   ###economic considerations GH
#   # GH_area <- 400                                    #m2 production area
#   # GH_productivity <- 14.14                          #kg/m2/a
#   # GH_invest <- 262.77*GH_area                       #Euro/m2
#   # GH_invest_annualized <- annualize(GH_invest,
#   #                                   interest_rate,
#   #                                   run_time,
#   #                                   timesteps)
#   # GH_tot_costs <- GH_invest_annualized + costs
# 
# 
# final_results
# 
# 
# ####pv scenarios
# final_results %>%
#   filter(parameters %in% c("PV_costs",
#                            "PV_capacity",
#                            "ES_capacity",
#                            "Grid")) %>%
#   filter(storage_cost_scenario == 1) %>%
#   filter(grid_cost_scenario == 1) %>%
#   group_by(parameters) %>%
#   mutate(values_prop=values/max(values)) %>%
#   ggplot(aes(x=pv_cost_scenario, y=values_prop)) +
#   geom_bar(stat="identity", aes(fill=parameters), position="dodge") +
#   labs(title = "Sensitivity analyses", x = "Scenario (pv_costs)", y = "Relation of output to maximum of all scenarios (%)")
# 
# 
# ###storage cost scenario
# final_results %>%
#   filter(parameters %in% c("ES_costs",
#                            "PV_capacity",
#                            "ES_capacity",
#                            "Grid")) %>%
#   filter(pv_cost_scenario == 1) %>%
#   filter(grid_cost_scenario == 1) %>%
#   group_by(parameters) %>%
#   mutate(values_prop=values/max(values)) %>%
#   ggplot(aes(x=storage_cost_scenario, y=values_prop)) +
#   geom_bar(stat="identity", aes(fill=parameters), position="dodge") +
#   labs(title = "Sensitivity analyses", x = "Scenario (ES_costs)", y = "Relation of output to maximum of all scenarios (%)")
# 
# ###grid_cost_scenario
# final_results %>%
#   filter(parameters %in% c("Grid_costs",
#                            "PV_capacity",
#                            "ES_capacity",
#                            "Grid")) %>%
#   filter(pv_cost_scenario == 1) %>%
#   filter(storage_cost_scenario == 1) %>%
#   group_by(parameters) %>%
#   mutate(values_prop=values/max(values)) %>%
#   ggplot(aes(x=grid_cost_scenario, y=values_prop)) +
#   geom_bar(stat="identity", aes(fill=parameters), position="dodge") +
#   labs(title = "Sensitivity analyses", x = "Scenario (grid costs)", y = "Relation of output to maximum of all scenarios (%)")
# 
# 
# 
# final_results
# 
# write.csv(final_results,paste0(results_dir,'Scenario_Grid_costs.csv'), row.names = FALSE)
