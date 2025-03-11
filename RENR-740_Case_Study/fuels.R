library(readr)
library(dplyr)
library(stringr)
library(ggplot2)
library(RColorBrewer)
library(ggrepel)



unburned_df <- read_csv("LWF161_UnburnedFuel.csv")
burned_df <- read_csv("LWF161_BurnedFuel.csv")


#aggregate M1
burned_df <- burned_df %>%
  mutate(major_type= str_extract(FUEL_TYPE, "^[^ ]+"))

unburned_df <- unburned_df %>%
  mutate(major_type= str_extract(FUEL_TYPE, "^[^ ]+"))

agg_burned <- burned_df %>% 
  group_by(major_type) %>%
  summarise(fuel_count = sum(COUNT)) 

agg_unburned <- unburned_df %>% 
  group_by(major_type) %>%
  summarise(fuel_count = sum(COUNT)) 



#make pie charts
colors <- brewer.pal(11, "Paired")

burned_chart <- agg_burned %>% 
  mutate(perc = round((fuel_count/ sum(fuel_count)), 4)) %>% 
  mutate(labels = scales::percent(perc)) %>% 
  arrange(desc(major_type)) %>% ## arrange in the order of the legend
  mutate(text_y = cumsum(fuel_count) - fuel_count/2) ### calculate where to place the text labels

burned_chart <- burned_chart %>%
  ggplot(aes(x = "", y = fuel_count, fill = major_type)) + 
  geom_col() +
  theme_void() +
  coord_polar(theta = "y") +
  geom_bar(stat = "identity", width = 1, color = "gray98", linewidth = 0.2) +
  geom_label_repel(aes(label = labels, y = text_y), 
                   nudge_x = 0.6, nudge_y = 0.6,
                   size = 2, show.legend = F) +
  guides(fill = guide_legend(title = "Fuel Types")) +
  scale_fill_manual(values = c("brown", "chocolate", "darkorange",
                               "darkgoldenrod1", "yellow",
                               "darkolivegreen3", "aquamarine4",
                               "orchid3", "slateblue3",
                               "violet", "deepskyblue")) +
  labs(title = "Burned Fuel Types") + 
  theme(
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold")
  ) 


burned_chart
ggsave("burned_fuel_pie.png", plot = burned_chart, dpi = 300, width = 4, height = 3)


unburned_chart <- agg_unburned %>% 
  mutate(perc = round((fuel_count/ sum(fuel_count)), 4)) %>% 
  mutate(labels = scales::percent(perc)) %>% 
  arrange(desc(major_type)) %>% ## arrange in the order of the legend
  mutate(text_y = cumsum(fuel_count) - fuel_count/2) ### calculate where to place the text labels

unburned_chart <- unburned_chart %>%
  ggplot(aes(x = "", y = fuel_count, fill = major_type)) + 
  geom_col() +
  theme_void() +
  geom_bar(stat = "identity", width = 1, color = "grey98", size = 0.2) +
  coord_polar(theta = "y") +
  geom_label_repel(aes(label = labels, y = text_y), 
                   nudge_x = 0.6, nudge_y = 0.6,
                   size = 2, show.legend = F) +
  guides(fill = guide_legend(title = "Fuel Types")) +
  scale_fill_manual(values = c("brown", "chocolate", "darkorange",
                               "darkgoldenrod1", "yellow",
                               "darkolivegreen3", "aquamarine4",
                               "orchid3", "slateblue3",
                               "violet", "deepskyblue")) +
  labs(title = "Unburned Fuel Types") + 
  theme(
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold")
  ) 

unburned_chart
ggsave("unburned_fuel_pie.png", plot = unburned_chart, dpi = 300, width = 4, height = 3)

burned_chart <- ggplot(agg_burned, aes(x = "", y = fuel_count, fill = major_type)) +
  geom_bar(stat = "identity", color = "white") +
  geom_label_repel(aes(x = 1.6, label = scales::percent(fuel_count, accuracy = .1)), position = position_stack(vjust = .5)) +
  coord_polar("y") +
  theme_void()

# Create pie chart with ColorBrewer Set3 palette
burned_chart  <- ggplot(agg_burned, aes(x = "", y = fuel_count, fill = major_type)) +
  geom_bar(stat = "identity", width = 1, color = "white", size = 1) +  # Add white borders between slices
  coord_polar(theta = "y") +
  scale_fill_manual(values = colors) +
  labs(title = "Improved Pie Chart with ColorBrewer Set3 Palette") +
  theme_void() +
  theme(
    legend.title = element_blank(),
    legend.position = "top",
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold")
  ) +
  geom_text_repel(aes(x = 1.6, label = paste0(major_type, "\n", round(fuel_count/sum(fuel_count)*100, 1), "%")), 
            position = position_stack(vjust = 0.5), color = "black", size = 5, fontface = "bold")

burned_chart

#----------------- M1/M2 Fuels -------------------------------------

#burned
burned_M <- burned_df %>%
  filter(major_type == "M-1/M-2") %>%
  mutate(percent_C = as.numeric(str_extract(FUEL_TYPE_, "\\d+(?=%)")))

total_M_burned <- sum(burned_M$COUNT)

burned_M <- burned_M %>%
  mutate(FREQ = COUNT / total_M_burned)

burned_M_plot <- ggplot(burned_M, aes(x = percent_C, y =FREQ)) +
  geom_bar(stat = "identity", fill = "aquamarine4", color = "black", alpha = 0.7) +
  labs(x = "Conifer Percentage(%)", y = "Proportion of M1/M2 Fuels", title = "Conifer Percentage in M1/M2 Burned Fuels") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 24, face = "bold"),
    axis.title = element_text(size = 20),                         # Axis labels size
    axis.text = element_text(size = 16)                          # Axis text size
  ) 

burned_M_plot
#unburned
unburned_M <- unburned_df %>%
  filter(major_type == "M-1/M-2") %>%
  mutate(percent_C = as.numeric(str_extract(FUEL_TYPE_, "\\d+(?=%)")))

total_M_unburned <- sum(unburned_M$COUNT)

unburned_M <- unburned_M %>%
  mutate(FREQ = COUNT / total_M_unburned)

unburned_M_plot <- ggplot(unburned_M, aes(x = percent_C, y = FREQ)) +
  geom_bar(stat = "identity", fill = "aquamarine4", color = "black", alpha = 0.7) +
  labs(x = "Conifer Percentage (%)", y = "Proportion of M1/M2 Fuels", title = "Conifer Percentage in M1/M2 Unburned Fuels") +
  theme_minimal() +
  theme(
  plot.title = element_text(hjust = 0.5, size = 24, face = "bold"),
  axis.title = element_text(size = 20),                         # Axis labels size
  axis.text = element_text(size = 16)                          # Axis text size
  ) 

unburned_M_plot

ggsave("unburned_M1M2.png", plot = unburned_M_plot, dpi = 300, width = 8, height = 6)
ggsave("burned_M1M2.png", plot = burned_M_plot, dpi = 300, width = 8, height = 6)

