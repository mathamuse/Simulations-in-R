################# SET SEED FOR REPLICABILITY ###################################
set.seed(2021)

################# CREATE CUSTOMER USING SIMMER #################################
# Note : The customer behavior is basic. 
#         Customer spawns, attempts to seize shop counter
#         If not seized, Customer waits
#         If seized, Customer gets processed.
#         If not seized in a particular time, Customer leaves ie abandons
#         Processing time is modeled with normal distribution (mean=10, sd =2)
#         Customer releases counter and exits
#         This is modeled with an object which simmer pipes into
################################################################################

customer <-
  trajectory("Customer's path") %>%
  log_("Here I am") %>%
  renege_in(function() rnorm(1,5,1),            #leave the queue if this happens
            out = trajectory("Reneging customer") %>%
              log_("I am off")) %>%
  seize("counter") %>%
  renege_abort() %>%
  timeout(function() rnorm(1,10,2)) %>%
  release("counter") %>%
  log_("Finished")


################# CREATE SHOP USING SIMMER #####################################
################ CUSTOMERS SPAWN WITH EXPONENTIAL DISTRIBUTION #################
# Note : The shop is made into a simmer object and customers are generated.
#        The customers spawn with exponential distribution

shopZ <-
  simmer("shop") %>%
  add_resource("counter") %>%
  add_generator("Customer", customer, function() rexp(1, 1/5))

############### RUN SHOP UNTIL RUNTIME = 45 ###################################
run(shopZ, until = 45)

# find all the balking customers in this shop
sum(get_mon_arrivals(shopZ)$activity_time == 0)

# hourly rate of balking in this shop
sum(get_mon_arrivals(shopZ)$activity_time == 0)/now(shopZ)*60

################ OUTPUT ########################################################
#            STORE MONITOR 
################ ###### ########################################################

resources <- get_mon_resources(shopZ)

# Plot usage in steps 
# Server refers to usage of counters 
plot(resources, metric = "utilization")

# Plot usage in steps 
# Server refers to usage of counters
plot(resources, metric = "usage",steps=T)

