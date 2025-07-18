################# SET SEED FOR REPLICABILITY ###################################
set.seed(2021)

################# CREATE CUSTOMER USING SIMMER #################################
# Note : The customer behavior is very simple. 
#         They spawn, walk around the art gallery for 10 mins and leave
#         This is modeled with an object which simmer pipes into
################################################################################

customer <-
  trajectory("Customer's path") %>%  # trajectory named 
  log_("Here I am") %>%              # Log presence
  timeout(10) %>%                    # Leave
  log_("I must leave")               # Log leaving

################# CREATE SHOP USING SIMMER #####################################
################ CUSTOMERS SPAWN WITH EXPONENTIAL DISTRIBUTION #################
# Note : The shop is made into a simmer object and customers are generated.
#        The customers spawn with exponential distribution

shopA <-
  simmer("shop") %>%
  add_generator("Customer", customer, function()rexp(1, 1/5))

############### RUN SHOP UNTIL RUNTIME = 100 ####################################

shopA %>% run(until = 50)

################ STORE MONITOR #################################################

shopA %>% get_mon_arrivals()

