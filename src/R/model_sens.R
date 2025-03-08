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


#scenarios

final_results     <- NULL

scenarios_pv      <- c(0.8, 1, 1.2)
scenarios_storage <- c(1) #seq(0.25,1.25, 0.5)
scenarios_grid    <-  c(1) #seq(0.8,2.4, 0.5)
scenarios_interest_r <- c(1) #seq(0.5,1.5,0.5)
scenarios_fit <- c(1) #seq(0.1, 0.5, 0.1)


for(pv_mult in scenarios_pv){
  for (storage_mult in scenarios_storage) {
    for (grid_mult in scenarios_grid) {
      for(ir_mult in scenarios_interest_r) {
        for(fit_mult in scenarios_fit) {

      print("####storage####")
      print(pv_mult)
      print(storage_mult)
      print(grid_mult)
      print(ir_mult)
      print (fit_mult)


      ############# average pv generation for 2006 - 2016 in kw.
      pvgis_data <- read.csv("data/input/PV_avg-06-16_hr.csv", header=TRUE, sep=";") #PVGis average hourly data 2006-2016
      pv <- as.vector(pvgis_data$P..W.)/1000                 #year avg

      pv <- c(pv[1:24],pv) 
      
      timesteps <- length (pv)
      days <- timesteps / 24



      ############# average demand VF in kw.
      prod_area_VF <- 720 #*mult                                                        #m2 actual production area which has to be illuminated
      energy_demand_VF <- 1173.5                                                #kWh/m2/a, includes illumination, irrigation, humidification, ventilation, ...
      photo_time <- 16                                                           #hours
      dark_time <- 24-photo_time                                                 #hours, during the dark period there is no illumination
      demand_day <- prod_area_VF*energy_demand_VF/365                            #total energy demand in kW/h
      demand_photo <- (demand_day*0.7)/photo_time                                #approx. 70 % of total energy demand accounts for illumination
      demand_dark <- (demand_day*0.3)/24
      demand_tot_VF <- demand_photo+demand_dark
      
      demand_ <- c(rep(demand_dark,dark_time/2), 
                   rep(demand_tot_VF, photo_time), 
                   rep(demand_dark,dark_time/2))                                           #kW in the course of one day
      demand <- c(rep(demand_, days-1))
      demand <- c(rep(0,24),demand)
      

      # ########### demand GH in kw/m2
      # GH_demand <- read.csv("data/input/GH-demand.csv", header=TRUE, sep=";")
      #
      # GH_lettuce <- as.vector(GH_demand$Coldhouse)
      # # GH_tomato <- as.vector(GH_demand$Hothouse)
      
      # GH_area <- 400                                                    #m2
      # COP.HP <- 3.5                                                     #Coefficient of performance (COP) of the heatpump (HP)
      #
      # GH_d <- GH_lettuce/COP.HP
      #
      # GH_demand_ <- GH_d*GH_area
      # demand <- c(rep(0,24),GH_demand_) 



      #####controllable demand
      avg_demand <- 100
      controllable_demand <- runif(days) * avg_demand * 0   #*days statt 0


      #########investment costs

      interest_rate <- 0.04
      interest_rate <- interest_rate*ir_mult
      run_time <- 20

      #Land cost
      land_cost <- 6.5                                  #Euro/m2 greenland.
      build_land_cost <- 200                            #Euro/m2

      pv_land <- 15                                     #1 KW needs more than 1 m2!, so it has to be multiplied by land-use of PV

      pv_invest <- 1000 + land_cost*pv_land # in €/kw
      pv_invest_annualized <- annualize(pv_invest,
                                        interest_rate,
                                        run_time,
                                        timesteps)
      pv_invest_annualized <- pv_invest_annualized*pv_mult

      run_time <- 10

      storage_invest <- 600 # in €/kWh
      storage_invest_annualized <- annualize(storage_invest,
                                             interest_rate,
                                             run_time,
                                             timesteps)
      storage_invest_annualized <- storage_invest_annualized*storage_mult

     

      #Emission cost
      co2.kWh <- 136.81      #co2 g/kWh

      #Grid cost
      gridcosts <- 0.17  # power from grid in €/kWh
      gridcosts <- gridcosts*grid_mult

      feed_in_tariff <- 0.05*fit_mult # subsidy received for feeding power to grid, Euro/kWh

      #technical parameters
      efficiency_storage <- 0.95
      maximum_power_controllable_demand <- 0 # how much power the controllable demand can use at most in one instant of time. In kW


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
      sum_pv <- mygdx["x_direct_use"]
      sum_pv_use <- sum(sum_pv$value)
      sum_storage_out <- mygdx["x_out"]
      sum_storage <- sum(sum_storage_out$value)
      sold_pv_energy <- mygdx["x_sell_to_grid"]
      sum_pv_sold <- sum(sold_pv_energy$value)

      timeseries <- read_timeseries_from_results(mygdx)

##RESULTS##
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
                            c("kWh","Euro/kWp","kWp", "kWh", "Euro/kWh", "kWh","kWh", "Euro/kWh", "kWh", "kWh", "Euro", "m2", "tons"),
                            pv_mult,
                            storage_mult,
                            grid_mult,
                            ir_mult,
                            fit_mult)

      names(results) <- c("parameters",
                          "values",
                          "units",
                          "pv_cost_scenario",
                          "storage_cost_scenario",
                          "grid_cost_scenario",
                          "interest_rate_scenario",
                          "feed_in_scenario")

      final_results <- bind_rows(final_results, results)
     }
    }
   }
  }
}



##FIGURES########################################################################
  ####pv scenarios absolute values
  final_results %>%
    filter(parameters %in% c("PV_energy",
                             "ES_out",
                             "Grid",
                             "PV_sold")) %>%
    filter(storage_cost_scenario == 1) %>%
    filter(grid_cost_scenario == 1) %>%
    filter(interest_rate_scenario==1)%>%
    group_by(parameters) %>%
    ggplot(aes(x=pv_cost_scenario, y=values)) +
    geom_line(stat="identity", aes(color=parameters), position="dodge", size=1) +
    geom_point(aes(x = pv_cost_scenario, y=values))+
    scale_y_continuous(labels = scales::comma)+
    labs(title = "Sensitivity analyses", x = "Change in PV Costs", y = "Energy used in kWh")+
    theme(plot.title = element_text(size = 18),
          plot.subtitle=element_text(size=16),
          axis.title.y = element_text(size = 15),
          axis.text.y = element_text(size = 15),
          axis.title.x = element_text(size = 17.5),
          axis.text.x = element_text(size = 15),
          text = element_text(size = 20)) 
  
  
  ####es scenarios absolute values
  final_results %>%
    filter(parameters %in% c("PV_energy",
                             "ES_out",
                             "Grid",
                             "PV_sold")) %>%
    filter(pv_cost_scenario == 1) %>%
    filter(grid_cost_scenario == 1) %>%
    filter(interest_rate_scenario==1)%>%
    group_by(parameters) %>%
    ggplot(aes(x=storage_cost_scenario, y=values)) +
    geom_line(stat="identity", aes(color=parameters), position="dodge", size=1) +
    geom_point(aes(x = storage_cost_scenario, y=values))+
    scale_y_continuous(labels = scales::comma)+
    labs(title = "Sensitivity analyses", x = "Change in ES Costs", y = "Energy used in kWh")+
    theme(plot.title = element_text(size = 18),
          plot.subtitle=element_text(size=16),
          axis.title.y = element_text(size = 15),
          axis.text.y = element_text(size = 15),
          axis.title.x = element_text(size = 17.5),
          axis.text.x = element_text(size = 15),
          text = element_text(size = 20)) 
  
  
  ####grid scenarios absolute values
  final_results %>%
    filter(parameters %in% c("PV_energy",
                             "ES_out",
                             "Grid",
                             "PV_sold")) %>%
    filter(pv_cost_scenario == 1) %>%
    filter(storage_cost_scenario == 1) %>%
    filter(interest_rate_scenario==1)%>%
    group_by(parameters) %>%
    ggplot(aes(x=grid_cost_scenario, y=values)) +
    geom_line(stat="identity", aes(color=parameters), position="dodge", size=1) +
    geom_point(aes(x = grid_cost_scenario, y=values))+
    scale_y_continuous(labels = scales::comma)+
    labs(title = "Sensitivity analyses", x = "Change in Grid Costs", y = "Energy used in kWh")+
    theme(plot.title = element_text(size = 18),
          plot.subtitle=element_text(size=16),
          axis.title.y = element_text(size = 15),
          axis.text.y = element_text(size = 15),
          axis.title.x = element_text(size = 17.5),
          axis.text.x = element_text(size = 15),
          text = element_text(size = 20)) 
  
  
  ####interest rate scenarios absolute values
  final_results %>%
    filter(parameters %in% c("PV_energy",
                             "ES_out",
                             "Grid",
                             "PV_sold")) %>%
    filter(pv_cost_scenario == 1) %>%
    filter(storage_cost_scenario == 1) %>%
    filter(grid_cost_scenario==1)%>%
    group_by(parameters) %>%
    ggplot(aes(x=interest_rate_scenario, y=values)) +
    geom_line(stat="identity", aes(color=parameters), position="dodge", size=1) +
    geom_point(aes(x = interest_rate_scenario, y=values))+
    scale_y_continuous(labels = scales::comma)+
    labs(title = "Sensitivity analyses", x = "Change in interest rate", y = "Energy used in kWh")+
    theme(plot.title = element_text(size = 18),
          plot.subtitle=element_text(size=16),
          axis.title.y = element_text(size = 15),
          axis.text.y = element_text(size = 15),
          axis.title.x = element_text(size = 17.5),
          axis.text.x = element_text(size = 15),
          text = element_text(size = 20)) 
  
  
  ####fit scenarios absolute values
  final_results %>%
    filter(parameters %in% c("PV_energy",
                             "ES_out",
                             "Grid",
                             "PV_sold")) %>%
    group_by(parameters) %>%
    ggplot(aes(x=feed_in_scenario, y=values)) +
    geom_line(stat="identity", aes(color=parameters), position="dodge", size=1) +
    geom_point(aes(x = feed_in_scenario, y=values))+
    scale_y_continuous(labels = scales::comma)+
    labs(title = "Sensitivity analyses", x = "Change in feed-in-tariff", y = "Energy used in kWh")+
    theme(plot.title = element_text(size = 18),
          plot.subtitle=element_text(size=16),
          axis.title.y = element_text(size = 15),
          axis.text.y = element_text(size = 15),
          axis.title.x = element_text(size = 17.5),
          axis.text.x = element_text(size = 15),
          text = element_text(size = 20)) 

##RESULTS## 
    final_results
    
    write.csv(final_results,paste0(results_dir,'results.csv'), row.names = FALSE)






