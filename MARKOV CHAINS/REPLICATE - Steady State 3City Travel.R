library(magrittr)
library(dplyr)
library(tidyr)
library(simmer)
library(simmer.plot)

################# SET SEED FOR REPLICABILITY ###################################
set.seed(2021)

################### REPLICATE with 10 REPLICATIONS #############################

no_replications <- 20
envs <- lapply(1:no_replications, function(i){

################# CREATE TRAVELLER USING SIMMER #################################
# Note : The traveller behavior is simple. 
#         Traveller spawns, and seizes path between three cities
#         
#         This is modeled with an object which simmer pipes into
################################################################################
paths <- simmer("Single_path") %>% 
  add_resource("City1") %>%
  add_resource("City2") %>%
  add_resource("City3")

traveller <- trajectory("journey") %>%
  log_("ENTER")%>%
  set_attribute("Count1",0)%>%
  set_attribute("Count2",0) %>%
  set_attribute("Count3",0) %>%
  set_attribute("Steps",0) %>%
  set_attribute("Current",function() {sample(c(1,2,3), 1, prob = c(1/3,1/3,1/3))}) %>% #INITIALIZE with current
  log_(tag="looper",function() paste0("Looping : Now ", get_attribute(paths, "Current"))) %>% 
  #timeout(1) %>%
  branch(option= function(){
    Cur<- get_attribute(paths,"Current")
    if (Cur == 1) {1}                   #Reach City1
    else if(Cur == 2)  {2}              #Reach City2
    else if(Cur == 3) {3}               #Reach City 3
    else {4}                            # Error Message   
  },
  continue = c(TRUE,TRUE,TRUE,FALSE),
  
  #BEHAVIOUR IN CITY1
  trajectory("City1")%>%
    #log_("Entering City1") %>%
    seize("City1",1) %>%
    set_attribute("Count1",1,mod = "+") %>%                                        #Increment Count1
    set_attribute("Steps",1,mod="+")    %>%                                        #Increment Steps
    set_attribute("Current", function() {sample(c(1,2,3), 1, prob = c(0.1,0.8,0.1))})  %>% #INITIALIZE with Next step
    #log_("Leaving City1")%>%
    timeout(1) %>%
    release("City1") ,
  
  #BEHAVIOUR IN CITY2
  trajectory("City2")%>%
    #log_("Entering City2") %>%
    seize("City2") %>%
    set_attribute("Count2",1,mod = "+") %>%                                        #Increment Count1
    set_attribute("Steps",1,mod="+")    %>%                                        #Increment Steps
    set_attribute("Current", function() {sample(c(1,2,3), 1, prob = c(0.5,0.1,0.4))})  %>% #INITIALIZE with Next step
    #log_("Leaving City2")%>%
    timeout(1) %>%
    release("City2"),
  
  #B City3
  trajectory("City3")%>%
    #log_("Entering City3") %>%
    seize("City3") %>%
    set_attribute("Count3",1,mod = "+") %>%                                        #Increment Count1
    set_attribute("Steps",1,mod="+")    %>%                                        #Increment Steps
    set_attribute("Current", function() {sample(c(1,2,3), 1, prob = c(0.2,0.1,0.7))})  %>% #INITIALIZE with Next step
    #log_("Leaving City3")%>%
    timeout(1) %>%
    release("City3"),
  
  #ERROR MSG
  trajectory("Error")%>%
    log_("Error Encountered")
  
  )%>% 
  log_(function() paste0("Counts . Count1: ", get_attribute(paths, "Count1")," Count2: ", get_attribute(paths, "Count2")," Count3: ", get_attribute(paths, "Count3")," Steps taken: ", get_attribute(paths, "Steps"))) %>% #log counts
  #timeout(1) %>%      #take one step
  rollback("looper") %>% 
  log_("EXIT")
######################## RUN SIMULATION FOR 1000 STEPS #########################             
step_n0 <-1000
paths %>% add_generator("OVERALL TRAJECTORY",traveller,at(0),mon = 2) %>%
  run(until = step_n0 +1)

# get attributes of simulation for filtering 
attribs <- get_mon_attributes(paths)

# Filter for the Last Update (Using dplyr)
last_attributes <- attribs %>%
  group_by(key, name) %>%  # Group by attribute key and arrival name
  slice_max(time, n = 1) %>% # Select the row with the maximum time (last update) within each group
  ungroup() %>%
  dplyr::select(key, value) # Explicitly use dplyr::select

# Convert to a List
# Convert to a named list where names are attribute keys and values are the attribute values
last_attribute_list <- setNames(last_attributes$value, last_attributes$key)

# Print the result
TotSteps <- last_attribute_list[[5]]
p1 <- last_attribute_list[[1]]/TotSteps
p2 <- last_attribute_list[[2]]/TotSteps
p3 <- last_attribute_list[[3]]/TotSteps


print("Probability of City1 :")
print(p1)
print("Probability of City2 :")
print(p2)
print("Probability of City3 :")
print(p3)

 return( list( environ<- paths,
               probC1 <- p1,
               probC2 <- p2,
               probC3 <- p3
              )
 )

}
)
################ OUTPUT ########################################################
#            STORE MONITOR 
################ ###### ########################################################
envs
env_for_plot <- lapply(envs,'[[',1 )
#env_for_plot
resource_data <- get_mon_resources(env_for_plot)

############### PLOT EVOLUTION BY LOOKING AT RESOURCE USAGE ####################
# plot the utilization of the counter
plot(resource_data, metric = "utilization")

# Plot cumulative average server usage over time
# Plot server usage (individual lines for each replication)
plot(resource_data, metric = "usage", items = "server")

############### SUMMARIZE TO CALCULATE PROBABILITIES ###########################
overall_data <- do.call(rbind,envs)
overall_data
overall_average_pc1 <-sum(unlist(overall_data[,2]))/no_replications
overall_average_pc1
overall_average_pc2 <-sum(unlist(overall_data[,3]))/no_replications
overall_average_pc2
overall_average_pc3 <-sum(unlist(overall_data[,4]))/no_replications
overall_average_pc3

