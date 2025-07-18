################# SET SEED FOR REPLICABILITY ###################################
set.seed(2021)

################# CREATE CUSTOMER USING SIMMER #################################
# Note : The customer behavior is basic. 
#         Customer spawns, attempts to seize shop counter
#         If not seized, Customer BALKS i.e waits and logs this behavior
#         If seized, Customer gets processed.
#         Processing time is modeled with normal distribution (mean=10, sd =2)
#         Customer releases counter and exits
#         This is modeled with an object which simmer pipes into
################################################################################
customer <-
  trajectory("Customer's path") %>%
  log_("Here I am") %>%
  seize("counter", continue = FALSE, reject = 
          trajectory("Balked customer") %>% log_("Balking") ) %>%
  timeout(function() rnorm(1,10,2)) %>%
  release("counter") %>%
  log_("Finished")

################# CREATE SHOP USING SIMMER #####################################
################ CUSTOMERS SPAWN WITH EXPONENTIAL DISTRIBUTION #################
# Note : The shop is made into a simmer object and customers are generated.
#        The customers spawn with exponential distribution
#        Counter has a fixed queue size of 3, beyond which BALK occurs
shopB <-
  simmer("shop") %>%
  add_resource("counter", queue_size = 3) %>%
  add_generator("Customer", customer,
                function() rexp(1, 1/5))

############### RUN SHOP UNTIL RUNTIME = 45 ###################################

shopB %>% run(until = 45)

################ OUTPUT ########################################################
#            STORE MONITOR 
################ ###### ########################################################

# find all the balking customers
sum(get_mon_arrivals(shopB)$activity_time == 0)

# hourly rate of balking
sum(get_mon_arrivals(shopB)$activity_time == 0)/now(shopB)*60

resources <- get_mon_resources(shopB)

# Plot usage in steps 
# Server refers to usage of counters 
plot(resources, metric = "utilization")


# Plot usage in steps 
# Server refers to usage of counters 
plot(resources, metric = "usage",steps=T)

