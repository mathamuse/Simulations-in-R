################# SET SEED FOR REPLICABILITY ###################################
set.seed(2021)

################# CREATE CUSTOMER USING SIMMER #################################
# Note : The customer behavior is simple. 
#         Customer spawns, seizes shop counter, and gets processed.
#         There are two independent shop counters.
#         Customer selects one with shortest queue
#         Processing time is modeled with normal distribution (mean=10, sd =2)
#         Customer releases counter and exits
#         This is modeled with an object which simmer pipes into
################################################################################
customer <-
  trajectory("Customer's path") %>%
  log_("Here I am") %>%
  select(c("counter1", "counter2"), policy = "shortest-queue") %>%
  seize_selected() %>%
  timeout(function() rnorm(1,10,2)) %>%
  release_selected() %>%
  log_("Finished")

################# CREATE SHOP USING SIMMER #####################################
################ CUSTOMERS SPAWN WITH EXPONENTIAL DISTRIBUTION #################
# Note : The shop is made into a simmer object and customers are generated.
#        The customers spawn with exponential distribution
#        There are two independant counters: counter1 and counter2

shopM <-
  simmer("shop") %>%
  add_resource("counter1", 1) %>%
  add_resource("counter2", 1) %>%
  add_generator("Customer", customer, function() rexp(1, 1/5))

############### RUN SHOP UNTIL RUNTIME = 45 ###################################

run(shopM, until = 45)


################ OUTPUT ########################################################
#            STORE MONITOR 
################ ###### ########################################################
#print number of customers who are not served
sum(get_mon_arrivals(shopM)$activity_time == 0)

#print hourly number of customers who are not served
sum(get_mon_arrivals(shopM)$activity_time == 0)/now(shopM)*60

# Plot usage in steps 
# Server refers to usage of counters 
resources <- get_mon_resources(shopM)
plot(resources, metric = "utilization")
plot(resources, metric = "usage",steps=T)

