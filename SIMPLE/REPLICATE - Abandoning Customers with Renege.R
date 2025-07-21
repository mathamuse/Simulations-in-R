################# SET SEED FOR REPLICABILITY ###################################
set.seed(2021)

################# CREATE CUSTOMER USING SIMMER #################################
# Note : The customer behavior is basic. 
#         Customer spawns, attempts to seize shop counter
#         If not seized, Customer waits
#         If seized, Customer gets processed.
#         If not seized in a particular time [Normal with mean=5, sd =1 ], Customer leaves ie abandons
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

################# APPLY REPLICATION ON SHOP ####################################
#          Use lapply method on shop created  
################# CREATE SHOP USING SIMMER #####################################
################ CUSTOMERS SPAWN WITH EXPONENTIAL DISTRIBUTION #################
# Note : The shop is made into a simmer object and customers are generated.
#        The customers spawn with exponential distribution
############### RUN SHOP UNTIL RUNTIME = 45 ###################################

replication_no = 100
envs <- lapply(1:replication_no, function(i){
 shopZ <-
  simmer("shop") %>%
  add_resource("counter",2) %>%
  add_generator("Customer", customer, function() rexp(1, 1/5)) %>%
  run(until = 45)

  # find all the abandoning customers in this shop
  leave_rate <- sum(get_mon_arrivals(shopZ)$activity_time == 0)
  
  # hourly rate of balking in this shop
  hourly_leave_rate <- sum(get_mon_arrivals(shopZ)$activity_time == 0)/now(shopZ)*60
  
  #return as a vector
  return(list(
    environ = shopZ,
    lr = leave_rate,
    hlr=hourly_leave_rate
    ))
   }
)

############### OUTPUT THE Balk Rates and Hourly Balk Rates  ###################
leaver<-do.call(rbind,envs)
mean_leave_rate<- sum(unlist(leaver[,2]))/replication_no
paste0("Mean balk rate is : ",mean_leave_rate)
mean_leaver_balk_rate<-sum(unlist(leaver[,3]))/replication_no
paste0("Mean Hourly balk rate is : ",mean_leaver_balk_rate)


################ OUTPUT ########################################################
#            STORE MONITOR 
################ ###### ########################################################

env_for_plot <- lapply(envs, function(x) x$environ)
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
