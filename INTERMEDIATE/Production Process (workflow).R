library(magrittr)
library(dplyr)
library(tidyr)
library(simmer)
library(simmer.plot)
################# SET SEED FOR REPLICABILITY ###################################
set.seed(2025)
manLin <- simmer("ManufacturingLine") %>%
  add_resource("M1",2) %>%
  add_resource("M2",3) %>%
  add_resource("QC",2) 
  
################# CREATE PRODUCT USING SIMMER #################################
# Note : The product behavior is simple. 
#         STAGE 1 : Product spawns, seizes Machine1 resource, and gets processed.
#         Processing time is modeled with normal distribution (mean=15)
#         Process can be sucessful or failure (with prob 90% success)
#         STAGE 2 : Product releases Machine1 and seizes Machine2 and gets processed
#         Processing time is modeled with normal distribution (mean=5)         
#         Process can be sucessful or failure (with prob 70% success)
#         STAGE 3 : Product releases Machine2 resource and seizes QualityCheck and gets processed
#         Processing time is modeled with normal distribution (mean=7)
#         If Stage1 is FAIL, scrap the product.
#         If stage2 is FAIL, retry Stage2 (max attempts 3)
#         If both success, product is PASS
#         This is modeled with an object which simmer pipes into
################################################################################

 product_traj <-
  trajectory("product") %>%
  # Initialize M2_attempt to 0 at the very beginning of the trajectory.
  # This ensures it's set once per product.
  log_("Here I am") %>%
  set_attribute("M2_attempt", 0) %>%
  log_(function() paste0("Product created with M2_attempt initialized to: ", get_attribute(manLin, "M2_attempt"))) %>%
   
  #STAGE 1 begin MACHINE 1
  seize("M1",1) %>%              # seize the means of production Machine1
  set_attribute("M1_output",function() {sample(c(1,0 ), 1, prob = c(0.9, 0.1))}) %>% # pass with a probability of 0.9, else fail.
  timeout(function() rnorm(1,12,1)) %>%
  release("M1") %>%
  log_(function() paste0("Machine1 Completed. M1_output: ", get_attribute(manLin, "M1_output"))) %>%
   
  # #STAGE 2 begin MACHINE 2
  log_(tag = "M2_loop", function() paste0("Machine2 begins. M2_attempt: ", get_attribute(manLin, "M2_attempt"))) %>%
  seize("M2",1) %>%              # seize the means of production Machine2
  set_attribute("M2_attempt",1,mod="+") %>%
  timeout(function() rnorm(1,12,1)) %>%
  release("M2") %>%
  set_attribute("M2_output", function() sample(c(1,0 ), 1, prob = c(0.7, 0.3))) %>%  # pass with a probability of 0.7, else fail.
  log_(function() paste0("Machine2 Completed. M2_output: ", get_attribute(manLin, "M2_output"))) %>%
   
  #STAGE 3 begin QUALITY CHECK
  seize("QC",1) %>%             # seize the means of production Quality Check
  log_("Quality Check Entered") %>%
  timeout(function() rnorm(1,6,1)) %>%
  release("QC") %>%
  branch(
       option = function(){
       m1_out <- get_attribute(manLin , "M1_output")
       m2_out <- get_attribute(manLin , "M2_output")
       m2_att <- get_attribute(manLin , "M2_attempt")
       
       if(m1_out == 0) {1}                            #Option 1 : Failed at M1 so Scrap
       else if (m1_out == 1 & m2_out == 1 ) {2}       #Option 2 : Passed both M1 and M2
       else if (m1_out == 1 & m2_out == 0 & m2_att >=3) {3} 
       else {4}},                                     # option 4 : Retry      
     continue = c(TRUE,TRUE,TRUE,TRUE),
     
     # option 1 FAILED at Stage 1 ... SCRAP
     trajectory("Scrap")%>%     
       set_attribute("Output",-1) %>%
       log_("FAILED AT STAGE 1 , so SCRAP"),
     
     # option 2 PASS
     trajectory("Pass") %>%     
       set_attribute("Output",1) %>%
       log_("PASSED"),
     
     # option 2 FAILED at Stage 2 ... FAIL
     trajectory("Fail")%>% 
       set_attribute("Output",3) %>%
       log_("FAILED AT STAGE 2 , so FINAL FAIL"),
     
     # option 4 FAILED at Stage 2 ... Retry
     trajectory("Retry") %>%
       set_attribute("M2_attempt",1,mod = "+") %>%
       log_(function() paste0("Failed after STAGE 2. Retrying M2. Current M2_attempt: ", get_attribute(manLin, "M2_attempt"))) %>%
       rollback("M2_loop") 
     ) %>%
   log_(function() paste0("Product Finished. Final Output: ", get_attribute(manLin, "Output"))) #final Log

################# RUN MANUFACTURING LINE UNTIL RUNTIME = 100####################
 
manLin %>% 
  add_generator("product arrival",product_traj, at(seq(1,400,by=5)), mon = 2) %>%
  run(until = 1000)

# plot the trajectory 
plot(product_traj)


################ OUTPUT ########################################################
#            STORE MONITOR 
################ ###### ########################################################

# --- Analyze Product Outcomes using mon_attributes_data ---
# Retrieve monitoring data AFTER the simulation run
mon_arrivals_data <- get_mon_arrivals(manLin)
mon_attributes_data <- get_mon_attributes(manLin)


# --- ANALYSIS SECTION ---

# Helper function to extract the final value of a specific attribute for each product
get_final_attribute_value <- function(data, attribute_key) {
  data %>%
    filter(key == attribute_key) %>%
    group_by(name, replication) %>%
    # Get the last recorded value for this attribute for each unique product
    slice_max(time, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(name, replication, value) %>%
    rename(!!attribute_key := value) # Renames the 'value' column to the attribute's key (e.g., M1_output)
}

# Extract final values for each attribute
final_m1_output <- get_final_attribute_value(mon_attributes_data, "M1_output")
final_m2_output <- get_final_attribute_value(mon_attributes_data, "M2_output")
final_product_output <- get_final_attribute_value(mon_attributes_data, "Output")
final_m2_attempt <- get_final_attribute_value(mon_attributes_data, "M2_attempt")


# Join all these final attribute values to the main arrivals data
final_product_summary_fixed <- mon_arrivals_data %>%
  filter(!is.na(end_time)) %>% # Only consider completed arrivals
  left_join(final_m1_output, by = c("name", "replication")) %>%
  left_join(final_m2_output, by = c("name", "replication")) %>%
  left_join(final_product_output, by = c("name", "replication")) %>%
  left_join(final_m2_attempt, by = c("name", "replication")) %>% # Also join M2_attempt
  # Select and reorder columns as desired
  select(name, replication, start_time, end_time,
         M1_output, M2_output, M2_attempt, Output) # Ensure all columns are here


# --- Analyze Final Product Outcomes ---
final_outcomes_summary <- final_product_summary_fixed %>%
  group_by(Output) %>%
  summarise(
    count = n(),
    mean_flow_time = mean(end_time - start_time, na.rm = TRUE), # Add na.rm for safety
    .groups = 'drop'
  )
################# ANALYSIS ###################
print("Final Product Outcomes (derived from mon_attributes):")
print(final_outcomes_summary)

print("\nFull product summary (first few rows):")
print(final_product_summary_fixed)

# And the M2_attempt distribution
m2_attempts_summary <- final_product_summary_fixed %>%
  group_by(M2_attempt) %>%
  summarise(count = n(), .groups = 'drop')

print("\nDistribution of M2_attempt for completed products:")
print(m2_attempts_summary)

#  Calculating  Probabilities

n_gen <- get_n_generated(manLin,"product arrival")
print("\n Total products created : ")
print(n_gen)

print(" Probability of completion : ")
compl <- sum(final_product_summary_fixed$replication)
print(compl/n_gen)

print("Probability of Pass : ")
pas <- final_product_summary_fixed %>% group_by(Output) %>% filter(Output==1) %>% count() %>% pull(n)
print(pas/compl)

print("Probability of Fail : ")
fail <- final_product_summary_fixed %>% group_by(Output) %>% filter(Output==3) %>% count() %>% pull(n)
print(fail/compl)

print("Probability of Scrap : ")
scrap <- final_product_summary_fixed %>% group_by(Output) %>% filter(Output==-1) %>% count() %>% pull(n) 
print(scrap/compl)

working_time <- final_product_summary_fixed$end_time - final_product_summary_fixed$start_time 
print("Average Running time:")
print(sum(working_time)/compl)

############################# PLOTTING #########################################
# plot the utilization 
plot(get_mon_resources(manLin),metric= "utilization")

# Plot usage in steps 
# Server refers to usage of counters 
plot(get_mon_resources(manLin), metric = "usage", items = "server",steps=TRUE)

# plot waiting time
plot(get_mon_arrivals(manLin), metric = "waiting_time")
# plot flow time
plot(get_mon_arrivals(manLin), metric = "flow_time")

