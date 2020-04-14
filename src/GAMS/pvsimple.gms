set t;

set c;

set d;

set tech_param;


parameter demand(t);

parameter controllable_demand(d);

parameter pv_production(t);

parameter costs;

parameter t_in_d(t, d);

parameter  technical_parameters(tech_param)

$gdxin data/input/input.gdx
$load t d c tech_param demand pv_production costs controllable_demand t_in_d technical_parameters
$gdxin

positive variable
x_pv,
x_storage,
x_in,
x_out,
x_soc,
x_direct_use,
x_curtailment,
x_buy_from_grid,
x_sell_to_grid,
x_control_demand;

variable x_cost;

equations
obj,
demand_balance,
pv_balance,
stor_max,
stor_balance,
control_dem,
control_dem_max;

obj..x_cost =E= costs("investment_costs_PV") * x_pv +
                costs("investment_costs_storage") * x_storage +
                sum(t, costs("grid_buy_costs") * x_buy_from_grid(t)) -
                sum(t, costs("grid_sell_price") * x_sell_to_grid(t))
                        ;

demand_balance(t).. x_direct_use(t) + x_out(t) + x_buy_from_grid(t)=E=
                            demand(t) + x_control_demand(t);

pv_balance(t)..pv_production(t)*x_pv =E= x_direct_use(t) +
                                         x_in(t) +
                                         x_curtailment(t) +
                                         x_sell_to_grid(t);

stor_max(t).. x_soc(t) =L= x_storage;

stor_balance(t)..x_soc(t) =E= x_soc(t-1) + technical_parameters("efficiency_storage")*x_in(t) - x_out(t);

control_dem(d)..controllable_demand(d) =E= SUM(t$t_in_d(t,d), x_control_demand(t));

control_dem_max(t)..x_control_demand(t) =L= technical_parameters("max_power_controllable_load");

model pv /all/;

solve pv minimizing x_cost using lp;

scalar modelstat, solvestat;
modelstat =  pv.modelstat;
solvestat =  pv.solvestat;

Execute_Unload 'data/output/output.gdx';











