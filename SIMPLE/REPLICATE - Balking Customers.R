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

################# APPLY REPLICATION ON SHOP ####################################
#          Use lapply method on shop created  
################# CREATE SHOP USING SIMMER #####################################
################ CUSTOMERS SPAWN WITH EXPONENTIAL DISTRIBUTION #################
# Note : The shop is made into a simmer object and customers are generated.
#        The customers spawn with exponential distribution
#        Counter has a fixed queue size of 3, beyond which BALK occurs
############### RUN SHOP UNTIL RUNTIME = 40 ###################################
replication_no = 100
env <- lapply(1:replication_no, function(i){
  shopB<- simmer("shop") %>%
  add_resource("counter",4, queue_size = 6) %>%
  add_generator("Customer", customer,
                function() rexp(1, 1/5)) %>%
  run(until=40)
  
  
  # find all the balking customers in this shop
  balk_rate <- sum(get_mon_arrivals(shopB)$activity_time == 0)
  
  # hourly rate of balking in this shop
  hourly_balk_rate <- sum(get_mon_arrivals(shopB)$activity_time == 0)/now(shopB)*60
  
  #return as a list
  return(list(
    environ = shopB,
    br = balk_rate,
    hbr = hourly_balk_rate
  ))
  }
)

############### OUTPUT THE Balk Rates and Hourly Balk Rates  ###################

balker<-do.call(rbind,env)
mean_balk_rate <- sum(as.numeric(balker[,2]))/replication_no
paste0("Mean balk rate is : ",mean_balk_rate)
mean_hourly_balk_rate<-sum(as.numeric(balker[,3]))/replication_no
paste0("Mean Hourly balk rate is : ",mean_hourly_balk_rate)

################ OUTPUT ########################################################
#            STORE MONITOR 
################ ###### ########################################################


env_for_plot <- lapply(env, function(x) x$environ)
resource_data <- get_mon_resources(env_for_plot)
arr_data<-get_mon_arrivals(env_for_plot)

# plot the utilization of the counter
plot(resource_data, metric = "utilization")

# Server refers to usage of counters 
plot(resource_data, metric = "usage", items = "server", steps = TRUE)

# Plot cumulative average server usage over time
# Plot server usage (individual lines for each replication)
plot(resource_data, metric = "usage", items = "server")

# Plot cumulative queue evolution over time
# Plot queue usage (individual lines for each replication)
p<-plot(resource_data, metric = "usage", items = "queue", steps=TRUE)
print(p)

#plot waiting time and flow time
plot(arr_data, metric = "waiting_time")
plot(arr_data, metric = "flow_time")


## geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'

