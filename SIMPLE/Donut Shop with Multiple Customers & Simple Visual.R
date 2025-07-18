################# SET SEED FOR REPLICABILITY ###################################
set.seed(2021)

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
#        There are two counters which serve customers 

shopABC <-
  simmer("shop") %>%
  add_resource("counter",2) %>%
  add_generator("Customer", customer, function() rexp(1, 1/5))

############### RUN SHOP UNTIL RUNTIME = 30 ####################################

shopABC %>% run(until = 30)

################ OUTPUT ########################################################
#            STORE MONITOR 
################ ###### ########################################################

# show waiting times of customers
shopABC %>%
  get_mon_arrivals() %>%
  transform(waiting_time = end_time - start_time - activity_time)

# show resource usage
shopABC %>% get_mon_resources()


############## VISUALIZATION ###########
#install.packages("simmer.plot")

resources <- get_mon_resources(shopABC)

# plot the utilization 
plot(resources, metric = "utilization")

# Plot usage in steps 
# Server refers to usage of counters 
# queue refers to customers in the queue
# system is total customers = serving + waiting(queued)

plot(resources, metric = "usage",steps=T)
