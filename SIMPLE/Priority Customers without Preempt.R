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
  log_("Here I am") %>%
  seize("counter") %>%
  timeout(function() rnorm(1,10,2)) %>%
  release("counter") %>%
  log_("Finished")

################# CREATE SHOP USING SIMMER #####################################
################ CUSTOMERS SPAWN WITH EXPONENTIAL DISTRIBUTION #################
# Note : The shop is made into a simmer object and customers are generated.
#        The customers spawn with exponential distribution
#        There are two types of customers: Customers and Priority Customers
#        Priority customers take priority but don't preempt existing queue

shopPw <-
  simmer("shop") %>%
  add_resource("counter",1) %>%
  add_generator("Customer", customer, function() rexp(1, 1/5)) %>%
  add_generator("Priority_Customer", customer, function() rexp(1, 1/15), priority = 1)

############### RUN SHOP UNTIL RUNTIME = 45 ###################################

shopPw %>% run(until = 45)


################ OUTPUT ########################################################
#            STORE MONITOR 
################ ###### ########################################################

resources <- get_mon_resources(shopPw)

# Plot usage in steps 
# Server refers to usage of counters 
plot(resources, metric = "utilization")

# Plot usage in steps 
# Server refers to usage of counters 
plot(resources, metric = "usage",steps=T)
