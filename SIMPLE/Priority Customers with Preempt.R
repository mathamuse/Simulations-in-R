################# SET SEED FOR REPLICABILITY ###################################
set.seed(2021)

################# CREATE CUSTOMER USING SIMMER #################################
# Note : The customer behavior is simple. 
#         Customer spawns, seizes shop counter, and gets processed.
#         Processing time is modeled with normal distribution (mean=10, sd =2)
#         Customer releases counter and exits
#         This is modeled with an object which simmer pipes into
################################################################################
customer <-
  trajectory("Customer's path") %>%
  seize("counter") %>%
  timeout(function() rnorm(1,10,2)) %>%
  release("counter") 

################# CREATE SHOP USING SIMMER #####################################
################ CUSTOMERS SPAWN WITH EXPONENTIAL DISTRIBUTION #################
# Note : The shop is made into a simmer object and customers are generated.
#        The customers spawn with exponential distribution
#        There are two types of customers: Customers and Priority Customers
#        Priority customers take priority and preempt existing queue
#        Pre-empted Customer resumes after Priority customer is done.
shopPp <-
  simmer("shop") %>%
  add_resource("counter", preemptive = TRUE) %>%
  add_generator("Customer", customer, function() rexp(1, 1/5)) %>%
  add_generator("Priority_Customer", customer, function() rexp(1, 1/15), priority = 1)

############### RUN SHOP UNTIL RUNTIME = 45 ###################################

shopPp %>% run(until = 45)

################ OUTPUT ########################################################
#            STORE MONITOR 
################ ###### ########################################################
resources <- get_mon_resources(shopPp)

# Plot usage in steps 
# Server refers to usage of counters 
plot(resources, metric = "utilization")

# Plot usage in steps 
# Server refers to usage of counters 
plot(resources, metric = "usage",steps=T)

