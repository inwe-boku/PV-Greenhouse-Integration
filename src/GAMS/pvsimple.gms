set t;

set c;


parameter demand(t);

parameter pv_production(t);

parameter costs;

$gdxin data/input/input.gdx
$load t c demand pv_production costs
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
x_sell_to_grid;

variable x_cost;

equations
obj,
demand_balance,
pv_balance,
stor_max,
stor_balance;

obj..x_cost =E= costs("investment_costs_PV") * x_pv +
                costs("investment_costs_storage") * x_storage +
                sum(t, costs("grid_buy_costs") * x_buy_from_grid(t)) -
                sum(t, costs("grid_sell_price") * x_sell_to_grid(t))
                        ;

demand_balance(t).. x_direct_use(t) + x_out(t) + x_buy_from_grid(t)=E=
                            demand(t);

pv_balance(t)..pv_production(t)*x_pv =E= x_direct_use(t) +
                                         x_in(t) +
                                         x_curtailment(t) +
                                         x_sell_to_grid(t);

stor_max(t).. x_soc(t) =L= x_storage;

stor_balance(t)..x_soc(t) =E= x_soc(t-1) + 0.9*x_in(t) - x_out(t);

model pv /all/;

solve pv minimizing x_cost using lp;

Execute_Unload 'data/output/output.gdx';











