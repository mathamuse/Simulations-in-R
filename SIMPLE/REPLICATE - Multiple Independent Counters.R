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

################# APPLY REPLICATION ON SHOP ####################################
#          Use lapply method on shop created  
################# CREATE SHOP USING SIMMER #####################################
################ CUSTOMERS SPAWN WITH EXPONENTIAL DISTRIBUTION #################
# Note : The shop is made into a simmer object and customers are generated.
#        The customers spawn with exponential distribution
############### RUN SHOP UNTIL RUNTIME = 45 ###################################
replication_no = 100
envs <- lapply(1:replication_no, function(i){
 shopM <-
  simmer("shop") %>%
  add_resource("counter1", 1) %>%
  add_resource("counter2", 1) %>%
  add_generator("Customer", customer, function() rexp(1, 1/5)) %>%
  run(until = 45)
 
 # find all the abandoning customers in this shop
 leave_rate <- sum(get_mon_arrivals(shopM)$activity_time == 0)
 
 # hourly rate of balking in this shop
 hourly_leave_rate <- sum(get_mon_arrivals(shopM)$activity_time == 0)/now(shopZ)*60
 
 #return as a vector
 return(list(
   environ = shopM,
   lr = leave_rate,
   hlr=hourly_leave_rate
 ))
 }
)


############### OUTPUT THE Leave Rates and Hourly Leaving Rates  ###################
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
