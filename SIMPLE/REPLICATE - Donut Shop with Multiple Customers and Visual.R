################# SET SEED FOR REPLICABILITY ###################################
set.seed(2025)

################# CREATE CUSTOMER USING SIMMER #################################
# Note : The customer behavior is simple. 
#         Customer spawns, logs entry, seizes shop counter, and gets processed.
#         Processing time is modeled with normal distribution (mean=10, sd =2)
#         Customer releases counter and logs exit
#         This is modeled with an object which simmer pipes into
################################################################################

customer <-
  trajectory("Customer's path") %>%
  seize("counter") %>%
  timeout(function() rnorm(1,10,2)) %>%
  release("counter") 

################# APPLY REPLICATION ON SHOP ####################################
#          Use lapply method on shop created  
################# CREATE SHOP USING SIMMER #####################################
################ CUSTOMERS SPAWN WITH EXPONENTIAL DISTRIBUTION #################
# Note : The shop is made into a simmer object and customers are generated.
#        The customers spawn with exponential distribution
#        There are two counters which serve customers 
############### RUN SHOP UNTIL RUNTIME = 240 ###################################
envs <- lapply(1:100, function(i) {
  simmer("shop") %>% 
    add_resource("counter",2) %>%
    add_generator("Customer", customer, function() rexp(1, 1/5)) %>%
    run(until=240)
}
)

################ OUTPUT ########################################################
#            STORE MONITOR 
################ ###### ########################################################

############## VISUALIZATION ###########

# plot the utilization 
plot(get_mon_resources(envs), metric = "utilization")

# Plot usage in steps 
# Server refers to usage of counters 
plot(get_mon_resources(envs), metric = "usage", items = "server")

# plot waiting time
plot(get_mon_arrivals(envs), metric = "waiting_time")

## geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'
