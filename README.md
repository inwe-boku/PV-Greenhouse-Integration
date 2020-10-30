# PV-Greenhouse-Integration

READ ME
This document gives information about how to use the model in this repository. The repository contains two models: 
-	model_sens.R: calculates sensitivity analysis
-	model_scenarios.R: calculates scenarios
For using the model the first time the first command in the first line

	run <- FALSE

has to be activated, i.e. not commented. This command loads necessary libraries and sets the working directory. It has to be noticed, that this line must be executed only for the
very first run. After the first run, this line must be deactivated (comment out) in order to prevent the model from failures.

	# run <- FALSE

#######################################################################################
Model – scenarios:
The model can be used to calculate the most cost-efficient way to cover the electricity demand of an Urban Agricultural System (UAS), either a Vertical Farm (VF) or a Greenhouse (GH). 
First of all, the user has to choose which UAS to simulate. Depending on the choice, the next step is to activate/deactivate the UAS of interest, by commenting out. In this case the 
VF is chosen:
	
###Choice of UAS simulation
		VF <- FALSE		#simulation VF
		VF.E <- FALSE		#calculation VF per kg of lettuce
		VF.R <- FALSE		#remove variable VF after run to prepare for next run
		# GH <- FALSE		#simulation GH
		# GH.E <- FALSE	#calculation GH per kg of lettuce
		# GH.R <- FALSE	#remove variable GH after run to prepare for next run

The next step is to choose the scenario which should be simulated. There is a selection of three scenarios:
-	Base-Scenario: Benchmark, uses data from literature
-	Autarky Scenario: simulates an isolated operation of the UAS, where no grid is available
-	Mix Scenario: uses a mix of all electricity system elements
The user again can choose which scenario to simulate by commenting out the other scenarios. In this case the base scenario was chosen:
	
###Choice of Scenario
		base <- FALSE
		# autarky <- FALSE
		# mix <- FALSE

Depending on the choice of scenario, different input parameters are handed over to the model. Input parameters are:
-	PV.i: investment costs for the photovoltaic system (PV) in Euro/kWp
-	ES.i: investment costs for the energy storage system (ES) in Euro/kWh
-	G.i: public grid (G) electricity costs in Euro/kWh
-	fit.i: feed-in-tariff to sell surplus PV electricity to the grid in Euro/kWh
The calculation results in information regarding costs, used electricity, PV area and emissions of the simulated UAS. The results are also given per kg of lettuce produced.

Note: Beside input parameters determined in the scenario, the model is based on various input variables, which can be changed at the beginning of the model’s script.  

########################################################################################
Model – sens:
This model calculates the sensitivity of the parameters: PV investment costs, energy storage (ES) system investment costs, grid electricity costs.
The model can be used by running the script all at once. As there are a lot of simulations it takes the model more than 20 minutes to calculate all parameters simultaneously. 
The execution time can be shortened by calculating only one parameter at once, while the others remain unchanged. This is possible by setting all parameters 1, except the one considered.

scenarios_pv      <- c(0.8, 1, 1.2)
scenarios_storage <- c(1) 
scenarios_grid    <- c(1) 
scenarios_interest_r <- c(1)
scenarios_fit <- c(1)

Here for example the model only calculates the sensitivity of the PV investment costs parameter, while all other economic parameters remain as in the base scenario. 
The sensitivity analysis uses the base scenario as a starting point, which provides the initial input parameters, which subsequently are altered. In each of the diagrams, 
one economic parameter is altered, while the other parameters remain as in the base scenario (ceteris paribus). The x-axis displays the change in investment costs in percentage 
and the y-axis shows the amount of electricity used, given in absolute terms. The results of the sensitivity analysis show the influence of increasing or declining costs of 
electricity supply elements to supply the VF in the course of one year.
