# Import necessary packages
library(dplyr)
library(ggplot2)

## Loading Data
data_table <- read.table("modelisation.csv", header=TRUE, sep=";", dec=",")

## Filter data for 'LUZELLE' variety and 'C' light treatment
filtered_data_L_C <- subset(data_table, Var == "LUZELLE" & LT == "C")

#initialize total length to 0
Total_Length_Ramifications_L_C<-0

# Calculate the sum of ramification lengths for these plants
Total_Length_Ramifications_L_C <- sum(filtered_data_L_C$RamifLength, na.rm = TRUE)

# Display the total sum of ramification lengths for the selected plants
print(Total_Length_Ramifications_L_C)

## Filter data for 'LUZELLE' variety and 'S' light treatment

## Filter data for 'LUZELLE' variety and 'S' light treatment
filtered_data_L_S <- subset(data_table, Var == "LUZELLE" & LT == "S")

#initialize total length to 0
Total_Length_Ramifications_L_S<-0

# Calculate the sum of ramification lengths for these plants
Total_Length_Ramifications_L_S <- sum(filtered_data_L_S$RamifLength, na.rm = TRUE)

# Display the total sum of ramification lengths for the selected plants
print(Total_Length_Ramifications_L_S)


## Filter data for 'ANTANE' variety and 'C' light treatment
filtered_data_A_C <- subset(data_table, Var == "ANTANE" & LT == "C")

# Calculate the sum of ramification lengths for these plants
Total_Length_Ramifications_A_C <- sum(filtered_data_A_C$RamifLength, na.rm = TRUE)

# Display the total sum of ramification lengths for the selected plants
print(Total_Length_Ramifications_A_C)


## Filter data for 'ANTANE' variety and 'S' light treatment
filtered_data_A_S <- subset(data_table, Var == "ANTANE" & LT == "S")

# Calculate the sum of ramification lengths for these plants
Total_Length_Ramifications_A_S <- sum(filtered_data_A_S$RamifLength, na.rm = TRUE)

# Display the total sum of ramification lengths for the selected plants
print(Total_Length_Ramifications_A_S)

## Filter data for 'Marina' variety and 'C' light treatment
filtered_data_M_C <- subset(data_table, Var == "MARINA" & LT == "C")

# Calculate the sum of ramification lengths for these plants
Total_Length_Ramifications_M_C <- sum(filtered_data_M_C$RamifLength, na.rm = TRUE)

# Display the total sum of ramification lengths for the selected plants
print(Total_Length_Ramifications_M_C)

## Filter data for 'Marina' variety and 'S' light treatment
filtered_data_M_S <- subset(data_table, Var == "MARINA" & LT == "S")

# Calculate the sum of ramification lengths for these plants
Total_Length_Ramifications_M_S <- sum(filtered_data_M_S$RamifLength, na.rm = TRUE)

# Display the total sum of ramification lengths for the selected plants
print(Total_Length_Ramifications_M_S)

## Filter data for 'Barvince' variety and 'C' light treatment
filtered_data_B_C <- subset(data_table, Var == "BARVINCE" & LT == "C")

# Calculate the sum of ramification lengths for these plants
Total_Length_Ramifications_B_C <- sum(filtered_data_B_C$RamifLength, na.rm = TRUE)

# Display the total sum of ramification lengths for the selected plants
print(Total_Length_Ramifications_B_C)


## Filter data for 'Barvince' variety and 'S' light treatment
filtered_data_B_S <- subset(data_table, Var == "BARVINCE" & LT == "S")

# Calculate the sum of ramification lengths for these plants
Total_Length_Ramifications_B_S <- sum(filtered_data_B_S$RamifLength, na.rm = TRUE)

# Display the total sum of ramification lengths for the selected plants
print(Total_Length_Ramifications_B_S)
