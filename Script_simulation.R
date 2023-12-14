# Importing packages
library(dplyr)
library(tidyr)
library(ggplot2)

##Simulation of the total of the length of the ramifications for 15 plants of Luzelle C

# Initialize an empty list to store Total_Ramification_Length_B_S values
repetition_L_C <- list()

# Repeat the simulation 10000 times
for (j in 1:10000) {
  # Simulating the total length of ramifications for 15 plants of Luzelle C
  
  # Simulating the number of ramifications for 15 plants
  simulated_values_ramifications_L_C <- round(rnorm(15, 6.285714, 2.373321), 0)
  
  # Sum of all ramifications created for the 15 plants
  sum_ramifications_L_C <- sum(simulated_values_ramifications_L_C)
  
  # Initializing the variable Total_Ramification_Length_L_C
  Total_Ramification_Length_L_C <- 0
  
  # Loop for each simulated ramification
  for (i in 1:sum_ramifications_L_C) {
    # Simulating the number of leaves for each ramification following the normal distribution
    simulated_values_NbLeaves_L_C <- round(rnorm(1, 6.918367, 3.145043), 0)
    
    # Calculating the mean of internode length on this ramification
    InLengthMean_L_C <- 0.1 * simulated_values_NbLeaves_L_C + 0.28
    
    # Calculating the length of this ramification
    Ramification_Length_L_C <- InLengthMean_L_C * simulated_values_NbLeaves_L_C
    
    # Adding the length of this ramification to Total_Ramification_Length
    Total_Ramification_Length_L_C <- Total_Ramification_Length_L_C + Ramification_Length_L_C
  }
  
  # Store Total_Ramification_Length_L_C in the list
  repetition_L_C[[j]] <- Total_Ramification_Length_L_C
}

# Calculate the mean and standard deviation of the repetition_L_C list
mean_repetition_L_C <- mean(unlist(repetition_L_C))
stddev_repetition_L_C <- sd(unlist(repetition_L_C))

# Display the mean and standard deviation
cat("Mean of repetition_L_C:", mean_repetition_L_C, "\n")
cat("Standard Deviation of repetition_L_C:", stddev_repetition_L_C, "\n")


##Simulation of the total of the length of the ramifications for 15 plants of Luzelle S

# Initialize an empty list to store Total_Ramification_Length_L_S values
repetition_L_S <- list()

# Repeat the simulation 10000 times
for (j in 1:10000) {
  # Simulation of the total length of ramifications for 15 plants of Luzelle S
  
  # Simulating the number of ramifications for 15 plants
  simulated_values_ramifications_L_S <- round(rnorm(15, 6.444444, 3.303571), 0)
  
  # Sum of all ramifications created for the 15 plants
  sum_ramifications_L_S <- sum(simulated_values_ramifications_L_S)
  
  # Initializing the variable Total_Ramification_Length_L_S
  Total_Ramification_Length_L_S <- 0
  
  # Loop for each simulated ramification
  for (i in 1:sum_ramifications_L_S) {
    # Simulating the number of leaves for each ramification following the normal distribution
    simulated_values_NbLeaves_L_S <- round(rnorm(1, 6.354167, 2.657455), 0)
    
    # Calculating the mean of internode length on this ramification
    InLengthMean_L_S <- 0.24 * simulated_values_NbLeaves_L_S - 0.12
    
    # Calculating the length of this ramification
    Ramification_Length_L_S <- InLengthMean_L_S * simulated_values_NbLeaves_L_S
    
    # Adding the length of this ramification to Total_Ramification_Length
    Total_Ramification_Length_L_S <- Total_Ramification_Length_L_S + Ramification_Length_L_S
  }
  
  # Store Total_Ramification_Length_L_S in the list
  repetition_L_S[[j]] <- Total_Ramification_Length_L_S
}

# Calculate the mean and standard deviation of the repetition_L_S list
mean_repetition_L_S <- mean(unlist(repetition_L_S))
stddev_repetition_L_S <- sd(unlist(repetition_L_S))

# Display the mean and standard deviation
cat("Mean of repetition_L_S:", mean_repetition_L_S, "\n")
cat("Standard Deviation of repetition_L_S:", stddev_repetition_L_S, "\n")


##Simulation of the total of the length of the ramifications for 15 plants of Antane C

# Initialize an empty list to store Total_Ramification_Length_A_C values
repetition_A_C <- list()

# Repeat the simulation 10000 times
for (j in 1:10000) {
  # Simulating the number of ramifications for 15 plants
  simulated_values_ramifications_A_C <- round(rnorm(15, 8.75, 2.727178), 0)
  
  # Sum of all ramifications created for the 15 plants
  sum_ramifications_A_C <- sum(simulated_values_ramifications_A_C)
  
  # Initializing the variable Total_Ramification_Length_A_C
  Total_Ramification_Length_A_C <- 0
  
  # Loop for each simulated ramification
  for (i in 1:sum_ramifications_A_C) {
    # Simulating the number of leaves for each ramification following the normal distribution
    simulated_values_NbLeaves_A_C <- round(rnorm(1, 6.868852, 2.419019), 0)
    
    # Calculating the mean of internode length on this ramification
    InLengthMean_A_C <- 0.2 * simulated_values_NbLeaves_A_C - 0.59
    
    # Calculating the length of this ramification
    Ramification_Length_A_C <- InLengthMean_A_C * simulated_values_NbLeaves_A_C
    
    # Adding the length of this ramification to Total_Ramification_Length
    Total_Ramification_Length_A_C <- Total_Ramification_Length_A_C + Ramification_Length_A_C
  }
  
  # Store Total_Ramification_Length_A_C in the list
  repetition_A_C[[j]] <- Total_Ramification_Length_A_C
}

# Calculate the mean and standard deviation of the repetition_A_C list
mean_repetition_A_C <- mean(unlist(repetition_A_C))
stddev_repetition_A_C <- sd(unlist(repetition_A_C))

# Display the mean and standard deviation
cat("Mean of repetition_A_C:", mean_repetition_A_C, "\n")
cat("Standard Deviation of repetition_A_C:", stddev_repetition_A_C, "\n")


##Simulation of the total of the length of the ramifications for 15 plants of Antane S

# Initialize an empty list to store Total_Ramification_Length_A_S values
repetition_A_S <- list()

# Repeat the simulation 10000 times
for (j in 1:10000) {
  # Simulating the number of ramifications for 15 plants
  simulated_values_ramifications_A_S <- round(rnorm(15, 7.166667, 2.67187), 0)
  
  # Sum of all ramifications created for the 15 plants
  sum_ramifications_A_S <- sum(simulated_values_ramifications_A_S)
  
  # Initializing the variable Total_Ramification_Length_A_S
  Total_Ramification_Length_A_S <- 0
  
  # Loop for each simulated ramification
  for (i in 1:sum_ramifications_A_S) {
    # Simulating the number of leaves for each ramification following the normal distribution
    simulated_values_NbLeaves_A_S <- round(rnorm(1, 5.797980, 2.287338), 0)
    
    # Calculating the mean of internode length on this ramification
    InLengthMean_A_S <- 0.22 * simulated_values_NbLeaves_A_S - 0.49
    
    # Calculating the length of this ramification
    Ramification_Length_A_S <- InLengthMean_A_S * simulated_values_NbLeaves_A_S
    
    # Adding the length of this ramification to Total_Ramification_Length
    Total_Ramification_Length_A_S <- Total_Ramification_Length_A_S + Ramification_Length_A_S
  }
  
  # Store Total_Ramification_Length_A_S in the list
  repetition_A_S[[j]] <- Total_Ramification_Length_A_S
}

# Calculate the mean and standard deviation of the repetition_A_S list
mean_repetition_A_S <- mean(unlist(repetition_A_S))
stddev_repetition_A_S <- sd(unlist(repetition_A_S))

# Display the mean and standard deviation
cat("Mean of repetition_A_S:", mean_repetition_A_S, "\n")
cat("Standard Deviation of repetition_A_S:", stddev_repetition_A_S, "\n")


##Simulation of the total of the length of the ramifications for 15 plants of Marina C

# Initialize an empty list to store Total_Ramification_Length_M_C values
repetition_M_C <- list()

# Repeat the simulation 10000 times
for (j in 1:10000) {
  # Simulating the number of ramifications for 15 plants
  simulated_values_ramifications_M_C <- round(rnorm(15, 7.625, 2.496873), 0)
  
  # Sum of all ramifications created for the 15 plants
  sum_ramifications_M_C <- sum(simulated_values_ramifications_M_C)
  
  # Initializing the variable Total_Ramification_Length_M_C
  Total_Ramification_Length_M_C <- 0
  
  # Loop for each simulated ramification
  for (i in 1:sum_ramifications_M_C) {
    # Simulating the number of leaves for each ramification following the normal distribution
    simulated_values_NbLeaves_M_C <- round(rnorm(1, 6.537037, 2.278831), 0)
    
    # Calculating the mean of internode length on this ramification
    InLengthMean_M_C <- 0.24 * simulated_values_NbLeaves_M_C - 0.46
    
    # Calculating the length of this ramification
    Ramification_Length_M_C <- InLengthMean_M_C * simulated_values_NbLeaves_M_C
    
    # Adding the length of this ramification to Total_Ramification_Length
    Total_Ramification_Length_M_C <- Total_Ramification_Length_M_C + Ramification_Length_M_C
  }
  
  # Store Total_Ramification_Length_M_C in the list
  repetition_M_C[[j]] <- Total_Ramification_Length_M_C
}

# Calculate the mean and standard deviation of the repetition_M_C list
mean_repetition_M_C <- mean(unlist(repetition_M_C))
stddev_repetition_M_C <- sd(unlist(repetition_M_C))

# Display the mean and standard deviation
cat("Mean of repetition_M_C:", mean_repetition_M_C, "\n")
cat("Standard Deviation of repetition_M_C:", stddev_repetition_M_C, "\n")


##Simulation of the total of the length of the ramifications for 15 plants of Marina S

# Initialize an empty list to store Total_Ramification_Length_M_S values
repetition_M_S <- list()

# Repeat the simulation 10000 times
for (j in 1:10000) {
  # Simulating the number of ramifications for 15 plants
  simulated_values_ramifications_M_S <- round(rnorm(15, 5.875, 3.257204), 0)
  
  # Sum of all ramifications created for the 15 plants
  sum_ramifications_M_S <- sum(simulated_values_ramifications_M_S)
  
  # Initializing the variable Total_Ramification_Length_M_S
  Total_Ramification_Length_M_S <- 0
  
  # Loop for each simulated ramification
  for (i in 1:sum_ramifications_M_S) {
    # Simulating the number of leaves for each ramification following the normal distribution
    simulated_values_NbLeaves_M_S <- round(rnorm(1, 5.915789, 2.565615), 0)
    
    # Calculating the mean of internode length on this ramification
    InLengthMean_M_S <- 0.26 * simulated_values_NbLeaves_M_S - 0.24
    
    # Calculating the length of this ramification
    Ramification_Length_M_S <- InLengthMean_M_S * simulated_values_NbLeaves_M_S
    
    # Adding the length of this ramification to Total_Ramification_Length
    Total_Ramification_Length_M_S <- Total_Ramification_Length_M_S + Ramification_Length_M_S
  }
  
  # Store Total_Ramification_Length_M_S in the list
  repetition_M_S[[j]] <- Total_Ramification_Length_M_S
}

# Calculate the mean and standard deviation of the repetition_M_S list
mean_repetition_M_S <- mean(unlist(repetition_M_S))
stddev_repetition_M_S <- sd(unlist(repetition_M_S))

# Display the mean and standard deviation
cat("Mean of repetition_M_S:", mean_repetition_M_S, "\n")
cat("Standard Deviation of repetition_M_S:", stddev_repetition_M_S, "\n")



##Simulation of the total of the length of the ramifications for 15 plants of Barvince C

# Initialize an empty list to store Total_Ramification_Length_B_C values
repetition_B_C <- list()

# Repeat the simulation 10à00 times
for (j in 1:10000) {
  # Simulating the number of ramifications for 15 plants
  simulated_values_ramifications_B_C <- round(rnorm(15, 7.166667, 2.409472), 0)
  
  # Sum of all ramifications created for the 15 plants
  sum_ramifications_B_C <- sum(simulated_values_ramifications_B_C)
  
  # Initializing the variable Total_Ramification_Length_B_C
  Total_Ramification_Length_B_C <- 0
  
  # Loop for each simulated ramification
  for (i in 1:sum_ramifications_B_C) {
    # Simulating the number of leaves for each ramification following the normal distribution
    simulated_values_NbLeaves_B_C <- round(rnorm(1, 7.010309, 3.071300), 0)
    
    # Calculating the mean of internode length on this ramification
    InLengthMean_B_C <- 0.06 * simulated_values_NbLeaves_B_C + 0.74
    
    # Calculating the length of this ramification
    Ramification_Length_B_C <- InLengthMean_B_C * simulated_values_NbLeaves_B_C
    
    # Adding the length of this ramification to Total_Ramification_Length
    Total_Ramification_Length_B_C <- Total_Ramification_Length_B_C + Ramification_Length_B_C
  }
  
  # Store Total_Ramification_Length_B_C in the list
  repetition_B_C[[j]] <- Total_Ramification_Length_B_C
}

# Calculate the mean and standard deviation of the repetition_B_C list
mean_repetition_B_C <- mean(unlist(repetition_B_C))
stddev_repetition_B_C <- sd(unlist(repetition_B_C))

# Display the mean and standard deviation
cat("Mean of repetition_B_C:", mean_repetition_B_C, "\n")
cat("Standard Deviation of repetition_B_C:", stddev_repetition_B_C, "\n")


##Simulation of the total of the length of the ramifications for 15 plants of Barvince S

# Initialize an empty list to store Total_Ramification_Length_B_S values
repetition_B_S <- list()

# Repeat the simulation 10000 times
for (j in 1:10000) {
  # Simulating the number of ramifications for 15 plants
  simulated_values_ramifications_B_S <- round(rnorm(15, 6, 2.380476), 0)
  
  # Sum of all ramifications created for the 15 plants
  sum_ramifications_B_S <- sum(simulated_values_ramifications_B_S)
  
  # Initializing the variable Total_Ramification_Length_B_S
  Total_Ramification_Length_B_S <- 0
  
  # Loop for each simulated ramification
  for (i in 1:sum_ramifications_B_S) {
    # Simulating the number of leaves for each ramification following the normal distribution
    simulated_values_NbLeaves_B_S <- round(rnorm(1, 6.253165, 2.607042), 0)
    
    # Calculating the mean of internode length on this ramification
    InLengthMean_B_S <- 0.3 * simulated_values_NbLeaves_B_S - 0.71
    
    # Calculating the length of this ramification
    Ramification_Length_B_S <- InLengthMean_B_S * simulated_values_NbLeaves_B_S
    
    # Adding the length of this ramification to Total_Ramification_Length
    Total_Ramification_Length_B_S <- Total_Ramification_Length_B_S + Ramification_Length_B_S
  }
  
  # Store Total_Ramification_Length_B_S in the list
  repetition_B_S[[j]] <- Total_Ramification_Length_B_S
}

# Calculate the mean and standard deviation of the repetition_B_S list
mean_repetition_B_S <- mean(unlist(repetition_B_S))
stddev_repetition_B_S <- sd(unlist(repetition_B_S))

# Display the mean and standard deviation
cat("Mean of repetition_B_S:", mean_repetition_B_S, "\n")
cat("Standard Deviation of repetition_B_S:", stddev_repetition_B_S, "\n")

