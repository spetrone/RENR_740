#FBP.R
library(readr)
library(cffdrs)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)

weather_df <- read_excel("LWF-161-2015-20250309T213257Z-001/LWF-161-2015/Weather/WX-LWF161-2015.xlsx")
View(weather_df)


#make FBP table for each fuel type
#FuelType, LAT, LONG, ELV, FFMC, BUI, WS, WD
# GS (Ground Slope), Dj (Julian day), hr, PC (percent conifer)
conditions <- c("M-1", 48.127944, -91.3020208, 500, 93, 50, 5, 0, 280, 16, 75)
conditions2 <- c("M-1", 48.127944, -91.3020208, 500, 93, 50, 25, 0, 280, 16, 75)

latitude <- 55.476183
longitude <- -112.027267
elevation <- 500
slope <- 0
fuel_type <- "C2"
aspect <- 0

burned_df <- read_csv("LWF161_BurnedFuel.csv")
agg_burned <- burned_df %>% 
  group_by(FUEL_TYPE) %>%
  summarise(fuel_count = sum(COUNT)) 



input_FBP_df <- weather_df %>%
  rename(WS = WIND_SPEED_KMH,
         WD = WIND_DIRECTION,
         FFMC = FINE_FUEL_MOISTURE_CODE,
         BUI = BUILD_UP_INDEX) %>%
  mutate(Dj =  yday(WEATHER_DATE),
         hr = 16,
         FuelType = NA,
         GS = slope,
         ELV = elevation,
         LAT = latitude,
         LONG = longitude,
         Aspect = aspect) %>%
  select(FuelType, WEATHER_DATE, WS, WD, FFMC, BUI, Dj, hr, GS, ELV, LAT, LONG, Aspect)




# Define the 5 fuel types
#selectin most dominant fuel codes
fuel_types <- c( "C1", "C2", "C3", "C4", "C5", "D1", "M2")


# Create an empty list to store the dataframes for each fuel type
df_list <- lapply(fuel_types, function(fuel_type) {
  # For each fuel type, modify the dataframe and set the FuelType column
  df <- input_FBP_df
  df$FuelType <- fuel_type
  return(df)
})

# Combine all the dataframes into one
input_FBP_df <- bind_rows(df_list)




#---------Run FBP tool -----------------------------

# Convert the last 11 columns to numeric
input_FBP_df[, (ncol(input_FBP_df)-10):ncol(input_FBP_df)] <- lapply(input_FBP_df[, (ncol(input_FBP_df)-10):ncol(input_FBP_df)], as.numeric)


input_FBP_df

fbp(input_FBP_df[3,])

FBP_df <- fbp(input_FBP_df)
FBP_df

#re-order by ID - for some reason it is out of order
FBP_df$ID <- as.numeric(FBP_df$ID)
FBP_df <- FBP_df[order(FBP_df$ID), ]


FBP_df2 <- cbind(FBP_df, input_FBP_df)
FBP_df2 <- FBP_df2 %>% select(FuelType,WEATHER_DATE, Dj,CFB, CFC, HFI, RAZ, ROS, SFC, TFC)


#SELECT TIME INTERVAL
ignition_date <- as.POSIXct("2015-06-23", format="%Y-%m-%d")
held_date <- as.POSIXct("2015-07-19", format="%Y-%m-%d")
under_control_date <- as.POSIXct("2015-07-26", format="%Y-%m-%d")
ext_date <- as.POSIXct("2016-02-09")
pic_date <- as.POSIXct("2015-06-27")
date24 <- as.POSIXct("2015-06-24")
date25 <- as.POSIXct("2015-06-25")
date26 <- as.POSIXct("2015-06-26")


# Example: held_dates and corresponding labels
held_dates <- (c( held_date, under_control_date ))  # Example dates
labels <- c("Held", "Under Control")  # Corresponding labels for each date

# Create a data frame for held_dates and labels
line_labels <- data.frame(
  held_date = held_dates,
  label = labels
)


FBP_df2 <- FBP_df2 %>% filter(WEATHER_DATE >= ignition_date & WEATHER_DATE <= ext_date)

FBP_C2 <- FBP_df2 %>% filter(FuelType == "C2")

FBP_short_all <- FBP_df2 %>% filter(WEATHER_DATE >= ignition_date & WEATHER_DATE <= under_control_date +5)

FBP_xshort_all <- FBP_df2 %>% filter(WEATHER_DATE >= ignition_date & WEATHER_DATE <= pic_date)

FBP_C22 <- FBP_short_all %>% filter(FuelType == "C2")
#--------- for C2 only ----------------------
# Reshape the data to long format (wide to long)
df_long_C2 <- FBP_C2 %>%
  pivot_longer(cols = -c(WEATHER_DATE, FuelType),  # Exclude WEATHER_DATE and FuelType columns
               names_to = "variable",  # Create a new column for the original column names
               values_to = "value")    # Create a new column for the values

# Create and store each plot in a list (one plot per variable)
plots_list_C2 <- lapply(unique(df_long_C2$variable), function(var) {
  
  subset_data <- subset(df_long_C2, variable == var)
  max_y <- (0.9 * (max(subset_data$value, na.rm = TRUE)))
  
  ggplot(subset(df_long_C2, variable == var), aes(x = WEATHER_DATE, y = value)) +
    geom_vline(xintercept = ignition_date, linetype="dashed",color = "red", size=1) +
    geom_vline(xintercept = held_date, linetype="dashed",color = "orangered3", size=1) +
    geom_vline(xintercept = under_control_date, linetype="dashed",color = "orangered3", size=1) +
    geom_text(aes(x=ignition_date, y=max_y, label="\nIgnition"), colour="gray30", size = 6,  angle=90) +
    geom_text(aes(x=held_date, y=max_y, label="\nBeing Held"), colour="gray30", size = 6,  angle=90) +
    geom_text(aes(x=under_control_date, y=max_y, label="\nUnder Control"), size = 6, colour="gray30", angle=90) +
    geom_line(color = "blue") +  # Line color (you can adjust as needed)
    geom_point(color = "blue") + # Add points to the lines
    labs(x = "Date", y = var, title = paste(var, "for C2 Fuel Type")) +  # Add title
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),  
      axis.title = element_text(size = 16),                        
      axis.text = element_text(size = 14),
      legend.position =  element_blank()
    )
})


# Name each plot in the list with the variable names for easy access
names(plots_list_C2) <- unique(df_long_C2$variable)

plots_list_C2[["CFB"]]  <- plots_list_C2[["CFB"]]  + geom_hline(yintercept = 0.1, linetype="dashed", color="gray40", size=0.8)
plots_list_C2[["CFB"]]  <- plots_list_C2[["CFB"]]  + geom_hline(yintercept = 0.9, linetype="dashed", color="gray40", size=0.8)

plots_list_C2[["CFB"]]



# Save each plot to a file, for example, as PNG files
lapply(names(plots_list_C2), function(var) {
  ggsave(paste0(var, "_C2_plot.png"), plot = plots_list_C2[[var]], width = 10, height = 6)
})



#------- For All Fuel Types -----------------------

df_long <- FBP_df2 %>%
  pivot_longer(cols = CFB:TFC,  # Reshape only the CFB and CFC columns into long format
               names_to = "variable",  # Name for the column that will hold the variable names
               values_to = "value")    # Name for the column that will hold the values


plots_list <- lapply(unique(df_long$variable), function(var) {
  
  subset_data <- subset(df_long, variable == var)
  
  # Find the maximum value for the y-axis (for the current variable)
  max_y <- (0.9 * (max(subset_data$value, na.rm = TRUE)))
  
  ggplot(subset(df_long, variable == var), aes(x = WEATHER_DATE, y = value, color = FuelType, group = FuelType)) +
    geom_line(size = 0.8) +  # Set line thickness for C1 to 1.5, others to 1
    geom_point() +    # Add points to the lines
    geom_vline(xintercept = ignition_date, linetype="dashed",color = "red", size=1) +
    geom_vline(xintercept = held_date, linetype="dashed",color = "orangered3", size=1) +
    geom_vline(xintercept = under_control_date, linetype="dashed",color = "orangered3", size=1) +
    geom_text(aes(x=ignition_date, y=max_y, label="\nIgnition"), colour="gray30", size = 6,  angle=90) +
    geom_text(aes(x=held_date, y=max_y, label="\nBeing Held"), colour="gray30", size = 6,  angle=90) +
    geom_text(aes(x=under_control_date, y=max_y, label="\nUnder Control"), size = 6, colour="gray30", angle=90) +
    labs(x = "Date", y = var, title = paste(var, "by Fuel Type"), color = "Fuel Type") +  # Add legend title
    theme_minimal() + 
    theme(
      plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),  
      axis.title = element_text(size = 16),                        
      axis.text = element_text(size = 14),
      legend.title = element_text(size = 16, face = "bold"),   # Increase legend title size
      legend.text = element_text(size = 14),                    # Increase legend text size
      legend.key.size = unit(1.5, "cm")     
    #  legend.position = c(0.9, 0.85)  # Position the legend inside the plot
    ) +
    scale_size_continuous(range = c(0.8, 1.5)) +  # Control the size scale for lines
    scale_color_manual(values = c("C1" = "brown", "C2" = "blue", "C3" = "deepskyblue",
                                  "C4" = "darkorchid1", "C5" = "slateblue", "D1" = "darkolivegreen3", "M2" = "aquamarine4"))  # Manually select colors
})




# Name each plot in the list with the variable names for easy access
names(plots_list) <- unique(df_long$variable)

# Example of how to access a plot, for instance for "CFB"


plots_list[["CFB"]]  <- plots_list[["CFB"]]  + geom_hline(yintercept = 0.1, linetype="dashed", color="gray40", size=0.8)
plots_list[["CFB"]]  <- plots_list[["CFB"]] + geom_hline(yintercept = 0.9, linetype="dashed", color="gray40", size=0.8)

plots_list[["CFB"]]


# Save each plot to a file, for example, as PNG files
lapply(names(plots_list), function(var) {
  ggsave(paste0(var, "_plot.png"), plot = plots_list[[var]], width = 10, height = 6)
})



#--------------------- For shorter time interval -------------------------------
#--------- for C2 only ----------------------
# Reshape the data to long format (wide to long)
df_long_C22 <- FBP_C22 %>%
  pivot_longer(cols = -c(WEATHER_DATE, FuelType),  # Exclude WEATHER_DATE and FuelType columns
               names_to = "variable",  # Create a new column for the original column names
               values_to = "value")    # Create a new column for the values

# Create and store each plot in a list (one plot per variable)
plots_list_C22 <- lapply(unique(df_long_C22$variable), function(var) {
  
  subset_data <- subset(df_long_C22, variable == var)
  max_y <- (0.9 * (max(subset_data$value, na.rm = TRUE)))
  
  ggplot(subset(df_long_C22, variable == var), aes(x = WEATHER_DATE, y = value)) +
    geom_vline(xintercept = ignition_date, linetype="dashed",color = "red", size=1) +
    geom_vline(xintercept = held_date, linetype="dashed",color = "orangered3", size=1) +
    geom_vline(xintercept = under_control_date, linetype="dashed",color = "orangered3", size=1) +
    geom_text(aes(x=ignition_date, y=max_y, label="\nIgnition"), colour="gray30", size = 6,  angle=90) +
    geom_text(aes(x=held_date, y=max_y, label="\nBeing Held"), colour="gray30", size = 6,  angle=90) +
    geom_text(aes(x=under_control_date, y=max_y, label="\nUnder Control"), size = 6, colour="gray30", angle=90) +
    geom_line(color = "blue") +  # Line color (you can adjust as needed)
    geom_point(color = "blue") + # Add points to the lines
    labs(x = "Date", y = var, title = paste(var, "for C2 Fuel Type")) +  # Add title
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),  
      axis.title = element_text(size = 16),                        
      axis.text = element_text(size = 14),
      legend.position =  element_blank()
    )
})


# Name each plot in the list with the variable names for easy access
names(plots_list_C22) <- unique(df_long_C22$variable)

plots_list_C22[["CFB"]]  <- plots_list_C22[["CFB"]]  + geom_hline(yintercept = 0.1, linetype="dashed", color="gray40", size=0.8)
plots_list_C22[["CFB"]]  <- plots_list_C22[["CFB"]]  + geom_hline(yintercept = 0.9, linetype="dashed", color="gray40", size=0.8)

plots_list_C22[["CFB"]]



# Save each plot to a file, for example, as PNG files
lapply(names(plots_list_C22), function(var) {
  ggsave(paste0("./short/", var, "_C2_plot.png"), plot = plots_list_C22[[var]], width = 10, height = 6)
})



#------- For All Fuel Types -----------------------

df_long2 <- FBP_short_all %>%
  pivot_longer(cols = CFB:TFC,  # Reshape only the CFB and CFC columns into long format
               names_to = "variable",  # Name for the column that will hold the variable names
               values_to = "value")    # Name for the column that will hold the values


plots_list2 <- lapply(unique(df_long2$variable), function(var) {
  
  subset_data <- subset(df_long2, variable == var)
  
  # Find the maximum value for the y-axis (for the current variable)
  max_y <- (0.9 * (max(subset_data$value, na.rm = TRUE)))
  
  ggplot(subset(df_long2, variable == var), aes(x = WEATHER_DATE, y = value, color = FuelType, group = FuelType)) +
    geom_line(size = 0.8) +  # Set line thickness for C1 to 1.5, others to 1
    geom_point() +    # Add points to the lines
    geom_vline(xintercept = ignition_date, linetype="dashed",color = "red", size=1) +
    geom_vline(xintercept = held_date, linetype="dashed",color = "orangered3", size=1) +
    geom_vline(xintercept = under_control_date, linetype="dashed",color = "orangered3", size=1) +
    geom_text(aes(x=ignition_date, y=max_y, label="\nIgnition"), colour="gray30", size = 6,  angle=90) +
    geom_text(aes(x=held_date, y=max_y, label="\nBeing Held"), colour="gray30", size = 6,  angle=90) +
    geom_text(aes(x=under_control_date, y=max_y, label="\nUnder Control"), size = 6, colour="gray30", angle=90) +
    labs(x = "Date", y = var, title = paste(var, "by Fuel Type"), color = "Fuel Type") +  # Add legend title
    theme_minimal() + 
    theme(
      plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),  
      axis.title = element_text(size = 16),                        
      axis.text = element_text(size = 14),
      legend.title = element_text(size = 16, face = "bold"),   # Increase legend title size
      legend.text = element_text(size = 14),                    # Increase legend text size
      legend.key.size = unit(1.5, "cm")     
      #  legend.position = c(0.9, 0.85)  # Position the legend inside the plot
    ) +
    scale_size_continuous(range = c(0.8, 1.5)) +  # Control the size scale for lines
    scale_color_manual(values = c("C1" = "brown", "C2" = "blue", "C3" = "deepskyblue",
                                  "C4" = "darkorchid1", "C5" = "slateblue", "D1" = "darkolivegreen3", "M2" = "aquamarine4"))  # Manually select colors
})




# Name each plot in the list with the variable names for easy access
names(plots_list2) <- unique(df_long2$variable)

# Example of how to access a plot, for instance for "CFB"


plots_list2[["CFB"]]  <- plots_list2[["CFB"]]  + geom_hline(yintercept = 0.1, linetype="dashed", color="gray40", size=0.8)
plots_list2[["CFB"]]  <- plots_list2[["CFB"]] + geom_hline(yintercept = 0.9, linetype="dashed", color="gray40", size=0.8)

plots_list2[["CFB"]]


# Save each plot to a file, for example, as PNG files
lapply(names(plots_list2), function(var) {
  ggsave(paste0("./short/", var, "_plot.png"), plot = plots_list2[[var]], width = 10, height = 6)
})


#-------------- Extra short - for pictures --------------------------------
#------- For All Fuel Types -----------------------

df_long3 <- FBP_xshort_all %>%
  pivot_longer(cols = CFB:TFC,  # Reshape only the CFB and CFC columns into long format
               names_to = "variable",  # Name for the column that will hold the variable names
               values_to = "value")    # Name for the column that will hold the values


plots_list3 <- lapply(unique(df_long3$variable), function(var) {
  
  subset_data <- subset(df_long2, variable == var)
  
  # Find the maximum value for the y-axis (for the current variable)
  max_y <- (0.9 * (max(subset_data$value, na.rm = TRUE)))
  
  ggplot(subset(df_long3, variable == var), aes(x = WEATHER_DATE, y = value, color = FuelType, group = FuelType)) +
    geom_line(size = 0.8) +  # Set line thickness for C1 to 1.5, others to 1
    geom_point() +    # Add points to the lines
    geom_vline(xintercept = date24, linetype="dashed",color = "gray30", size=1) +
    geom_vline(xintercept = date25, linetype="dashed",color = "gray30", size=1) +
    geom_vline(xintercept = date26, linetype="dashed",color = "gray30", size=1) +
    #geom_text(aes(x=date24, y=max_y, label="\nJune 24"), colour="gray30", size = 4,  angle=90) +
    #geom_text(aes(x=date25, y=max_y, label="\nJune 25"), colour="gray30", size = 4,  angle=90) +
    #geom_text(aes(x=date26, y=max_y, label="\nJune 26"), size = 4, colour="gray30", angle=90) +
    geom_vline(xintercept = ignition_date, linetype="dashed",color = "red", size=1) +
    geom_text(aes(x=ignition_date, y=max_y, label="\nIgnition"), colour="gray30", size = 6,  angle=90) +
    labs(x = "Date", y = var, title = paste(var, "by Fuel Type"), color = "Fuel Type") +  # Add legend title
    theme_minimal() + 
    theme(
      plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),  
      axis.title = element_text(size = 16),                        
      axis.text = element_text(size = 14),
      legend.title = element_text(size = 16, face = "bold"),   # Increase legend title size
      legend.text = element_text(size = 14),                    # Increase legend text size
      legend.key.size = unit(1.5, "cm")     
      #  legend.position = c(0.9, 0.85)  # Position the legend inside the plot
    ) +
    scale_size_continuous(range = c(0.8, 1.5)) +  # Control the size scale for lines
    scale_color_manual(values = c("C1" = "brown", "C2" = "blue", "C3" = "deepskyblue",
                                  "C4" = "darkorchid1", "C5" = "slateblue", "D1" = "darkolivegreen3", "M2" = "aquamarine4"))  # Manually select colors
})




# Name each plot in the list with the variable names for easy access
names(plots_list3) <- unique(df_long3$variable)

# Example of how to access a plot, for instance for "CFB"


plots_list3[["CFB"]]  <- plots_list3[["CFB"]]  + geom_hline(yintercept = 0.1, linetype="dashed", color="gray40", size=0.8)
plots_list3[["CFB"]]  <- plots_list3[["CFB"]] + geom_hline(yintercept = 0.9, linetype="dashed", color="gray40", size=0.8)

plots_list3[["CFB"]]


# Save each plot to a file, for example, as PNG files
lapply(names(plots_list3), function(var) {
  ggsave(paste0("./xshort/", var, "_plot.png"), plot = plots_list3[[var]], width = 10, height = 6)
})
