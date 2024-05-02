library(tidyverse)
library(knitr)
library(patchwork)
library(cowplot)
library(kableExtra)

setwd("C:/Users/emory/Documents/Intro to Data and Statistics/Datasets")
df = read.csv("UNHCR Asylum Data.csv")


World_data <- read.csv("UNHCR Asylum Data.csv")%>%
  filter(Country.of.origin == "Ukraine"| Country.of.origin == "Cuba"| Country.of.origin
         == "Afghanistan"| Country.of.origin == "Haiti"| Country.of.origin ==
           "Venezuela (Bolivarian Republic of)")%>%
  filter(Cases...Persons == "P")%>%
  filter(Year>= 2021)%>%
  group_by(Country.of.asylum, Country.of.origin, Year)%>%
  summarize(Recognized.decisions = sum(Recognized.decisions), 
            Complementary.protection = sum(Complementary.protection),
            Rejected.decisions = sum(Rejected.decisions),
            Otherwise.closed = sum(Otherwise.closed), 
            Total.decisions = sum(Total.decisions))%>%
  mutate(
    Rejected.percent.overall = ((Rejected.decisions + Otherwise.closed) / Total.decisions) * 100,
    Rejected.case.percent = (Rejected.decisions / Total.decisions) * 100,
    Rejected.admin.percent = (Otherwise.closed / Total.decisions) * 100)


#Creation of Bargraph Visualization showcasing differences in asylum application and rejection rates
#by country of origin by asylum applicants. 

asylum_table <- World_data %>%
  group_by(Country.of.origin, Year) %>%
  summarize(Total_Asylum_Seekers = sum(Total.decisions))



asylum_table_rejections <- World_data%>%
  group_by(Country.of.origin, Year) %>%
  summarise(Total_asylum_case_rejections = sum(Rejected.decisions))



country_colors <- c(
  "Afghanistan" = "#1f77b4", 
  "Ukraine" = "#ff7f0e", 
  "Haiti" = "#2ca02c", 
  "Venezuela (Bolivarian Republic of)" = "#d62728",
  "Cuba" = "#9467bd"
)

asylum_table$Country_Year <- paste(asylum_table$Country.of.origin, asylum_table$Year, sep = "_")


asylum_table_rejections$Country_Year <- paste(asylum_table_rejections$Country.of.origin, asylum_table_rejections$Year, sep = "_")


desired_order <- c(
  "Afghanistan_2021", "Afghanistan_2022", "Afghanistan_2023",
  "Cuba_2021", "Cuba_2022", "Cuba_2023",
  "Haiti_2021", "Haiti_2022", "Haiti_2023",
  "Ukraine_2021", "Ukraine_2022", "Ukraine_2023",
  "Venezuela (Bolivarian Republic of)_2021", "Venezuela (Bolivarian Republic of)_2022", "Venezuela (Bolivarian Republic of)_2023"
)

asylum_table$Country_Year <- factor(asylum_table$Country_Year, levels = desired_order)
asylum_table_rejections$Country_Year <- factor(asylum_table_rejections$Country_Year, levels = desired_order)


plot_total_asylum <- ggplot(asylum_table, aes(x = Country_Year, y = Total_Asylum_Seekers, fill = Country.of.origin)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black") +
  geom_text(aes(label = Year), position = position_dodge(width = 1), vjust = 1.5, size = 3) +  
  labs(x = "Year", 
       y = "Asylum Applications",
       fill = "Country of Origin") +
  scale_fill_manual(values = country_colors) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),  
        axis.title.x = element_text(size = 12),  
        axis.text.y = element_text(size = 8),
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        plot.title = element_text(size = 12)) +  
  coord_cartesian(clip = "off") +  
  scale_x_discrete(expand = c(0.1, 0))  

plot_rejections <- ggplot(asylum_table_rejections, aes(x = Country_Year, y = Total_asylum_case_rejections, fill = Country.of.origin)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black") +
  geom_text(aes(label = Year), position = position_dodge(width = 1), vjust = 1.5, size = 3) +  
  labs(x = "Year",  
       y = "Asylum Case Rejections") +
  scale_fill_manual(values = country_colors) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),  
        axis.title.x = element_text(size = 12),  
        axis.text.y = element_text(size = 8),
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        plot.title = element_text(size = 12)) +  
  theme(legend.position = "none") +
  coord_cartesian(clip = "off") +  
  scale_x_discrete(expand = c(0.1, 0))  

combined_plots <- plot_total_asylum / plot_rejections


combined_plots_with_title <- ggdraw() +
  draw_label("Comparison of Total Asylum Applications and Case Rejections", 
             x = 0.5, y = 0.98, 
             hjust = 0.5, vjust = 1, 
             size = 14,  
             fontface = "bold")


final_plot <- cowplot::plot_grid(
  combined_plots_with_title,
  combined_plots,
  ncol = 1,
  rel_heights = c(0.05, 1)  
)




#Creation of Boxplot visualizing regression results


Broad_Regression<-lm(data = World_data, formula = Rejected.percent.overall ~ Country.of.origin+as.factor(Year))

predictions <- data.frame(World_data$Country.of.origin, predict(Broad_Regression))


colnames(predictions) <- c("Country.of.origin", "Predicted_Rejection")


country_colors <- c("Afghanistan" = "#1f77b4", 
                    "Ukraine" = "#ff7f0e", 
                    "Haiti" = "#2ca02c", 
                    "Venezuela (Bolivarian Republic of)" = "#d62728",
                    "Cuba" = "#9467bd")


boxplot_plot <- ggplot(predictions, aes(x = Country.of.origin, y = Predicted_Rejection, fill = Country.of.origin)) +
  geom_boxplot() +
  labs(x = NULL, y = "Predicted Rejection Percent", title = "Visualization of Regression Results: 
       Impact of Country of Origin on 
       Asylum Case Rejection Percentages")+
  scale_fill_manual(values = country_colors, name = "Country of Origin") + 
  theme_minimal()+
  theme(axis.text.x = element_blank(),  
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        plot.title = element_text(face = "bold"))


boxplot_plot
print(final_plot)