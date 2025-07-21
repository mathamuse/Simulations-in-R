################# SET SEED FOR REPLICABILITY ###################################
set.seed(2021)

################# CREATE PATIENT USING SIMMER #################################
# Note : The patient behavior is simple. 
#         patient spawns, seizes nurse resource, and gets processed.
#         Processing time is modeled with normal distribution (mean=15)
#         Patient releases nurse and seizes doctor resource (two at once) and gets processed
#         Processing time is modeled with normal distribution (mean=5)         
#         Patient releases doctor resource and seizes administration and gets processed
#         Processing time is modeled with normal distribution (mean=5)         
#         This is modeled with an object which simmer pipes into
################################################################################

patient <- trajectory("patients' path") %>%
  ## add an intake activity 
  seize("nurse", 1) %>%
  timeout(function() rnorm(1, 15)) %>%
  release("nurse", 1) %>%
  ## add a consultation activity
  seize("doctor", 2) %>%
  timeout(function() rnorm(1, 20)) %>%
  release("doctor", 2) %>%
  ## add a planning activity
  seize("administration", 1) %>%
  timeout(function() rnorm(1, 5)) %>%
  release("administration", 1)

# plot patient trajectory
plot(patient)

################# APPLY REPLICATION ON HEALTH CENTER ###########################
#          Use lapply method on clinic created  
################# CREATE SHOP USING SIMMER #####################################
################ CUSTOMERS SPAWN WITH EXPONENTIAL DISTRIBUTION #################
# Note : The clinic is made into a simmer object and patients are generated.
#        The patients spawn with exponential distribution
#        There are three nurses, four doctors and 2 administrators.
#        
############### RUN CLINIC UNTIL RUNTIME = 240 #################################

envs <- lapply(1:100, function(i) {
  simmer("health center") %>%
    add_resource("nurse", 3) %>%
    add_resource("doctor", 4) %>%
    add_resource("administration", 2) %>%
    add_generator("patient", patient, function() rexp(1, 1/5)) %>%
    run(until=480)
}
)

################ OUTPUT ########################################################
#            STORE MONITOR 
################ ###### ########################################################

# plot the utilization 
plot(get_mon_resources(envs),metric= "utilization")

# Plot usage in steps 
# Server refers to usage of counters 
plot(get_mon_resources(envs), metric = "usage", items = "server")

# plot waiting time
plot(get_mon_arrivals(envs), metric = "waiting_time")

