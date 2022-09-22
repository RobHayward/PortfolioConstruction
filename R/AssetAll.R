#AssetAll.R
# running through the vignette. 
# https://cran.r-project.org/web/packages/AssetAllocation/vignettes/AssetAllocation.html
#Asset allocation vignette
#install.packages('AssetAllocation')
library(AssetAllocation)
library(PerformanceAnalytics)
# Use this to install the latest version of quantmod
#install.packages('quantmod')
library(quantmod)
names(asset_allocations$static)
asset_allocations$static$all_weather
all_weather <- asset_allocations$static$all_weather
bt_all_weather <- backtest_allocation(all_weather, ETFs$Prices, ETFs$Returns, ETFs$risk_free)
charts.PerformanceSummary(bt_all_weather$returns, main = all_weather$strat$name)
bt_all_weather$table_performance
chart.StackedBar(bt_all_weather$rebalance_weights, 
                 date.format = "%Y",
                 main = past0("Allocations, ", all_weather$name))
#==========================================================================
# Now try a custom strategy based on factors
factors_EW <- list(name = "EW Factors",
                   tickers = c("MTUM", "VLUE", "USMV", "QUAL"), 
                   default_weights = c(0.25, 0.25, 0.25, 0.25), 
                   rebalance_frequency = "month", 
                   portfolio_rule_fn = "constant_weights")
Factor_ETFs_data <- get_data_from_tickers(factors_EW$tickers, starting_date = "2013-08-01")
bt_factors_EW <- backtest_allocation(factors_EW, Factor_ETFs_data$P, Factor_ETFs_data$R)
charts.PerformanceSummary(bt_factors_EW$returns, main = bt_factors_EW$strat$name)
bt_factors_EW$table_performance
#======================================================================
ivy <- asset_allocations$tactical$ivy
raa <- asset_allocations$tactical$raa
dual <- asset_allocations$tactical$dual
aaa <- asset_allocations$tactical$aaa
bt_ivy <- backtest_allocation(ivy, ETFs$Prices, ETFs$Returns, ETFs$risk_free)
bt_raa <- backtest_allocation(raa, ETFs$Prices, ETFs$Returns, ETFs$risk_free)
bt_dual <- backtest_allocation(dual, ETFs$Prices, ETFs$Returns, ETFs$risk_free)
bt_aaa <- backtest_allocation(aaa, ETFs$Prices, ETFs$Returns, ETFs$risk_free)
ret_strats <- merge.xts(bt_ivy$returns, bt_raa$returns, bt_dual$returns, bt_aaa$returns)
# find index from which all strats are available
min_ind <- which.max(!is.na(rowSums(ret_strats)))
charts.PerformanceSummary(ret_strats[min_ind:nrow(ret_strats)])
cbind(bt_ivy$table_performance, bt_raa$table_performance, 
      bt_dual$table_performance, bt_aaa$table_performance)
# Visualise allocations
chart.StackedBar(bt_ivy$rebalance_weights, 
                 date.format = "%Y", 
                 main = paste0("Allocations, ", bt_ivy$strat$name))
                 
                 