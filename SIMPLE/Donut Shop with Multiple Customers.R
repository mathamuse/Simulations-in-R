################# SET SEED FOR REPLICABILITY ###################################
set.seed(2029)

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

shopAB <-
  simmer("shop") %>%
  add_resource("counter") %>%
  add_generator("Customer", customer, function() rexp(1, 1/5))

############### RUN SHOP UNTIL RUNTIME = 100 ####################################

shopAB %>% run(until = 100)

################ OUTPUT ########################################################
#            STORE MONITOR 
################ ###### ########################################################
shopAB %>%
  get_mon_arrivals() %>%
  transform(waiting_time = end_time - start_time - activity_time)
