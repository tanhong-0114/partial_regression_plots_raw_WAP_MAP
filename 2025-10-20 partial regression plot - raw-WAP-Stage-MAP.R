# load needed library
library(tidyverse)
library(viridis)
library(readxl)#read excel
library(rcompanion)#check variance
library(car)#anova
library(emmeans)#multiple comparison
library(multcomp)#multiple comparison
library(multcompView)#multiple comparison
library(dplyr)#getting the means and standard error
library(ggthemes) #畫圖
library(ggplot2) #畫圖
library(scales) #畫圖
library(ggbreak)
library(cowplot)#排圖
library(ggpubr) #存圖
library(dplyr)
library(FSA)
library(ggplot2)
library(rstatix)
library(zoo)

library(scales)  # for alpha()
# Load needed raw data
aphid_soil_microclimate_data_clean_for_model <- read_excel("C:/Users/hp/Desktop/aphid soil paper/pest_microclimate_data_2024_spring_and_fall.xlsx")

enemy <- read_excel("C:/Users/hp/Desktop/enemy_df.xlsx", sheet = "final")

enemy_reduced <- enemy%>%
  select(Date, `growing season`, block, plot, enemy_abundance)


df_new <- aphid_soil_microclimate_data_clean_for_model %>%
  left_join(enemy_reduced, by = c("Date", "growing season", "block", "plot"))

# Residuals - soybean growing stage
df_new$new_soybean_stage <- factor(df_new$new_soybean_stage, ordered = FALSE)
df_new$week <- factor(df_new$week, ordered = FALSE)
df_new$MAP <- factor(df_new$MAP, ordered = FALSE)


# Model aphid abundance by growth stage
# y-axis residual
# Residuals - WAP
df_new$week <- factor(df_new$week, ordered = FALSE)
resid_WAP <- lm(aphid_abundance_log ~ week, data = df_new )
# Extract residuals
df_new$resid_WAP<- residuals(resid_WAP)

# Model aphid abundance by growth stage
resid_stage <- lm(aphid_abundance_log ~ new_soybean_stage, data = df_new)
# Extract residuals
df_new$resid_stage<- residuals(resid_stage)

# Model aphid abundance by growth stage
resid_MAP <- lm(aphid_abundance_log ~ MAP, data = df_new)
# Extract residuals
df_new$resid_MAP <- residuals(resid_MAP)

# Residuals - WAP
df_new$week <- factor(df_new$week, ordered = FALSE)
resid_WAP_daily_mois <- lm(daily_mean_soil_moisture ~ week, data = df_new )
resid_WAP_1_day_mois <- lm(soil_moisture_lag ~ week, data = df_new )
resid_WAP_3_day_mois <- lm(three_day_mean_soil_moisture ~ week, data = df_new )
resid_WAP_5_day_mois <- lm(five_day_mean_soil_moisture ~ week, data = df_new )
resid_WAP_7_day_mois <- lm(seven_day_mean_soil_moisture ~ week, data = df_new )

resid_WAP_daily_ec <- lm(daily_mean_soil_conductivity ~ week, data = df_new )
resid_WAP_1_day_ec <- lm(soil_EC_lag ~ week, data = df_new )
resid_WAP_3_day_ec <- lm(three_day_mean_soil_conductivity ~ week, data = df_new )
resid_WAP_5_day_ec <- lm(five_day_mean_soil_conductivity ~ week, data = df_new )
resid_WAP_7_day_ec <- lm(seven_day_mean_soil_conductivity ~ week, data = df_new )
# Extract residuals
df_new$resid_WAP_daily_mois<- residuals(resid_WAP_daily_mois)
df_new$resid_WAP_1_day_mois<- residuals(resid_WAP_1_day_mois)
df_new$resid_WAP_3_day_mois<- residuals(resid_WAP_3_day_mois)
df_new$resid_WAP_5_day_mois<- residuals(resid_WAP_5_day_mois)
df_new$resid_WAP_7_day_mois<- residuals(resid_WAP_7_day_mois)

df_new$resid_WAP_daily_ec<- residuals(resid_WAP_daily_ec)
df_new$resid_WAP_1_day_ec<- residuals(resid_WAP_1_day_ec)
df_new$resid_WAP_3_day_ec<- residuals(resid_WAP_3_day_ec)
df_new$resid_WAP_5_day_ec<- residuals(resid_WAP_5_day_ec)
df_new$resid_WAP_7_day_ec<- residuals(resid_WAP_7_day_ec)

# Model aphid abundance by growth stage
resid_stage <- lm(aphid_abundance_log ~ new_soybean_stage, data = df_new)
resid_stage_daily_mois <- lm(daily_mean_soil_moisture ~ new_soybean_stage, data = df_new )
resid_stage_1_day_mois <- lm(soil_moisture_lag ~ new_soybean_stage, data = df_new )
resid_stage_3_day_mois <- lm(three_day_mean_soil_moisture ~ new_soybean_stage, data = df_new )
resid_stage_5_day_mois <- lm(five_day_mean_soil_moisture ~ new_soybean_stage, data = df_new )
resid_stage_7_day_mois <- lm(seven_day_mean_soil_moisture ~ new_soybean_stage, data = df_new )

resid_stage_daily_ec <- lm(daily_mean_soil_conductivity ~ new_soybean_stage, data = df_new )
resid_stage_1_day_ec <- lm(soil_EC_lag ~ new_soybean_stage, data = df_new )
resid_stage_3_day_ec <- lm(three_day_mean_soil_conductivity ~ new_soybean_stage, data = df_new )
resid_stage_5_day_ec <- lm(five_day_mean_soil_conductivity ~ new_soybean_stage, data = df_new )
resid_stage_7_day_ec <- lm(seven_day_mean_soil_conductivity ~ new_soybean_stage, data = df_new )

# Extract residuals
df_new$resid_stage_daily_mois<- residuals(resid_stage_daily_mois)
df_new$resid_stage_1_day_mois<- residuals(resid_stage_1_day_mois)
df_new$resid_stage_3_day_mois<- residuals(resid_stage_3_day_mois)
df_new$resid_stage_5_day_mois<- residuals(resid_stage_5_day_mois)
df_new$resid_stage_7_day_mois<- residuals(resid_stage_7_day_mois)

df_new$resid_stage_daily_ec<- residuals(resid_stage_daily_ec)
df_new$resid_stage_1_day_ec<- residuals(resid_stage_1_day_ec)
df_new$resid_stage_3_day_ec<- residuals(resid_stage_3_day_ec)
df_new$resid_stage_5_day_ec<- residuals(resid_stage_5_day_ec)
df_new$resid_stage_7_day_ec<- residuals(resid_stage_7_day_ec)

# Model aphid abundance by MAP

resid_MAP_daily_mois <- lm(daily_mean_soil_moisture ~ MAP, data = df_new )
resid_MAP_1_day_mois <- lm(soil_moisture_lag ~ MAP, data = df_new )
resid_MAP_3_day_mois <- lm(three_day_mean_soil_moisture ~ MAP, data = df_new )
resid_MAP_5_day_mois <- lm(five_day_mean_soil_moisture ~ MAP, data = df_new )
resid_MAP_7_day_mois <- lm(seven_day_mean_soil_moisture ~ MAP, data = df_new )

resid_MAP_daily_ec <- lm(daily_mean_soil_conductivity ~ MAP, data = df_new )
resid_MAP_1_day_ec <- lm(soil_EC_lag ~ MAP, data = df_new )
resid_MAP_3_day_ec <- lm(three_day_mean_soil_conductivity ~ MAP, data = df_new )
resid_MAP_5_day_ec <- lm(five_day_mean_soil_conductivity ~ MAP, data = df_new )
resid_MAP_7_day_ec <- lm(seven_day_mean_soil_conductivity ~ MAP, data = df_new )
# Extract residuals
df_new$resid_MAP_daily_mois<- residuals(resid_MAP_daily_mois)
df_new$resid_MAP_1_day_mois<- residuals(resid_MAP_1_day_mois)
df_new$resid_MAP_3_day_mois<- residuals(resid_MAP_3_day_mois)
df_new$resid_MAP_5_day_mois<- residuals(resid_MAP_5_day_mois)
df_new$resid_MAP_7_day_mois<- residuals(resid_MAP_7_day_mois)

df_new$resid_MAP_daily_ec<- residuals(resid_MAP_daily_ec)
df_new$resid_MAP_1_day_ec<- residuals(resid_MAP_1_day_ec)
df_new$resid_MAP_3_day_ec<- residuals(resid_MAP_3_day_ec)
df_new$resid_MAP_5_day_ec<- residuals(resid_MAP_5_day_ec)
df_new$resid_MAP_7_day_ec<- residuals(resid_MAP_7_day_ec)

spring <- c("2024 spring")
fall <- c("2024 fall")

df_new_spring<- df_new %>%
  filter (`growing season` %in% spring)

df_new_fall <- df_new %>%
  filter (`growing season` %in% fall)


#-----------------------------------------------------------------------------------------------------------------------------------

# filtered the data first, than residualized with stage
# now get started from here

#2025-09-24
# points: stroke = 0.25, shape = 21, size = 0.8
#1
#geom_point(aes(fill = after_scale(scales::alpha(color, 0.30))),stroke = 0.25, shape = 21, size = 0.8) + 
#2
#geom_smooth(method = "lm", se = TRUE,aes(color = `growing season`, fill = `growing season`),alpha = 0.4, size = 0.5) +   # 信賴區間透明度
#3 spring removed, size = 2.5
#4 

#daily mean soil moisture
all_raw_aphid_abundance_daily_mean_soil_moisture_correlation_test <- cor.test(df_new$aphid_abundance_log,
                                                                              df_new$daily_mean_soil_moisture,
                                                                              use = "complete.obs")
# Extract the correlation coefficient and p-value
all_raw_aphid_abundance_daily_mean_soil_moisture_correlation_coefficient <- all_raw_aphid_abundance_daily_mean_soil_moisture_correlation_test$estimate
all_raw_aphid_abundance_daily_mean_soil_moisture_p_value <- all_raw_aphid_abundance_daily_mean_soil_moisture_correlation_test$p.value

plot_all_raw_daily_mean_soil_moisture_aphid_abundance <- 
  ggplot(df_new,
         aes(x = daily_mean_soil_moisture,
             y = aphid_abundance_log,
             color = `growing season`)) + 
  geom_point(aes(fill = after_scale(scales::alpha(color, 0.30))),
             stroke = 0.2, shape = 21, size = 0.75) +
  geom_smooth(method = "lm", se = TRUE,col = "blue", aes(group = 1),
              alpha = 0.3, size = 0.5) +   # 信賴區間透明度
  # 線條顏色（深色）
  scale_color_manual(values = c("2024 spring" = "#006600",  # 深綠
                                "2024 fall"   = "#867052")) +  # 深咖啡
  
  labs(title = "",
       x = "Daily Mois.",
       y = expression( ~ log[10](Aphids + 1)/m^2 )) +
  theme_classic(base_size = 8, base_family = "sans") +
  theme(plot.title=element_text(size=8, face="bold",hjust = 0.5),
        axis.title=element_text(size=8),
        axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x = element_text(size =6),
        axis.text.y = element_text(size = 6),
        legend.position= "none",        # 顯示圖例
        legend.key.size = unit(12,"points"),
        legend.title = element_blank()) +
  coord_cartesian(clip = "off") +
  
  annotate("text", x = 10, y = -1.6,
           label = paste0("r = ",
                          round(all_raw_aphid_abundance_daily_mean_soil_moisture_correlation_coefficient, 3),
                          ", p < 0.001 "),
           hjust = 0, vjust = 0, size = 2.5, color = "red", fontface = "bold")  +
  scale_x_continuous(limits = c(6, 45)) +
  scale_y_continuous(limits = c(-2.0, 4.0))

ggsave("plot_all_raw_daily_mean_soil_moisture_aphid_abundance.png", plot = plot_all_raw_daily_mean_soil_moisture_aphid_abundance, 
       width = 4, height = 5, units = "cm", dpi = 300)

#daily mean soil moisture
all_raw_aphid_abundance_daily_mean_soil_moisture_spring_correlation_test <- cor.test(df_new_spring$aphid_abundance_log,
                                                                                     df_new_spring$daily_mean_soil_moisture,
                                                                                     use = "complete.obs")
# Extract the correlation coefficient and p-value
all_raw_aphid_abundance_daily_mean_soil_moisture_spring_correlation_coefficient <- all_raw_aphid_abundance_daily_mean_soil_moisture_spring_correlation_test$estimate
all_raw_aphid_abundance_daily_mean_soil_moisture_spring_p_value <- all_raw_aphid_abundance_daily_mean_soil_moisture_spring_correlation_test$p.value


#daily mean soil moisture
all_raw_aphid_abundance_daily_mean_soil_moisture_fall_correlation_test <- cor.test(df_new_fall$aphid_abundance_log,
                                                                                   df_new_fall$daily_mean_soil_moisture,
                                                                                   use = "complete.obs")
# Extract the correlation coefficient and p-value
all_raw_aphid_abundance_daily_mean_soil_moisture_fall_correlation_coefficient <- all_raw_aphid_abundance_daily_mean_soil_moisture_fall_correlation_test$estimate
all_raw_aphid_abundance_daily_mean_soil_moisture_fall_p_value <- all_raw_aphid_abundance_daily_mean_soil_moisture_fall_correlation_test$p.value



# scale_color_manual(values = c("2024 spring" = "#006600", "2024 fall" = "#867052"))
# Scatter plot with a linear trend line
plot_all_raw_daily_mean_soil_moisture_aphid_abundance_seasons <- 
  ggplot(df_new,
         aes(x = daily_mean_soil_moisture,
             y = aphid_abundance_log,
             color = `growing season`)) + 
  geom_point(aes(fill = after_scale(scales::alpha(color, 0.30))),
             stroke = 0.2, shape = 21, size = 0.75) + 
  # 設定回歸線顏色，根據不同的 'growing season' 來顯示不同顏色
  geom_smooth(data = df_new %>% filter(`growing season` == "2024 spring"), method = "lm", se = TRUE,
              color = "#006600",  # 深綠色回歸線
              fill = "#66CC66",   # 淺綠色信賴區間
              alpha = 0.4, size = 0.5) +   # 信賴區間透明度
  geom_smooth(data = df_new %>% filter(`growing season` == "2024 fall"), method = "lm", se = TRUE,
              color = "#8B4513",  # 紅棕色回歸線
              fill = "#C4B7A6",   # 淺咖啡信賴區間
              alpha = 0.4, size = 0.5) +   # 信賴區間透明度
  labs(title = "",
       x = "Daily Mois.",
       y = expression( ~ log[10](Aphids + 1)/m^2 )) +
  theme_classic(base_size = 8, base_family = "sans") +
  theme(plot.title = element_text(size = 8, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 8),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6),
        legend.position = "none",        # 顯示圖例
        legend.key.size = unit(12, "points"),
        legend.title = element_blank()) +
  coord_cartesian(clip = "off") +
  # 資料點顏色設置，確保資料點顏色正確
  scale_color_manual(values = c("2024 spring" = "#006600",  # 深綠
                                "2024 fall"   = "#867052")) +  # 深咖啡
  # 區間顏色設置，確保資料的顏色不會被覆蓋
  scale_fill_manual(values = c("2024 spring" = "#66CC66",  # 淺綠
                               "2024 fall"   = "#C4B7A6")) +  # 淺咖啡
  annotate("text", x = 10, y = -1.4,
           label = paste0("r = ",
                          round(all_raw_aphid_abundance_daily_mean_soil_moisture_spring_correlation_coefficient, 3),
                          ", p = ",
                          signif(all_raw_aphid_abundance_daily_mean_soil_moisture_spring_p_value, 3)),
           hjust = 0, vjust = 0, size = 2.5, color = "#006600") +
  annotate("text", x = 10, y = -1.8,
           label = paste0("r = ",
                          round(all_raw_aphid_abundance_daily_mean_soil_moisture_fall_correlation_coefficient, 3),
                          ", p < 0.001 " ),
           hjust = 0, vjust = 0, size = 2.5, color = "#8B4513", fontface = "bold") +
  scale_x_continuous(limits = c(6, 45)) +
  scale_y_continuous(limits = c(-2.0, 4.0))




#terminal for now 2025-09-22

#---------------------------------------------------------------------------
# restart from here

#daily mean soil moisture
#daily mean soil moisture
all_raw_aphid_abundance_1_day_mean_soil_moisture_correlation_test <- cor.test(df_new$aphid_abundance_log,
                                                                              df_new$soil_moisture_lag,
                                                                              use = "complete.obs")
# Extract the correlation coefficient and p-value
all_raw_aphid_abundance_1_day_mean_soil_moisture_correlation_coefficient <- all_raw_aphid_abundance_1_day_mean_soil_moisture_correlation_test$estimate
all_raw_aphid_abundance_1_day_mean_soil_moisture_p_value <- all_raw_aphid_abundance_1_day_mean_soil_moisture_correlation_test$p.value

plot_all_raw_1_day_mean_soil_moisture_aphid_abundance <- 
  ggplot(df_new,
         aes(x = soil_moisture_lag,
             y = aphid_abundance_log,
             color = `growing season`)) + 
  geom_point(aes(fill = after_scale(scales::alpha(color, 0.30))),
             stroke = 0.2, shape = 21, size = 0.75) +
  geom_smooth(method = "lm", se = TRUE,col = "blue", aes(group = 1),
              alpha = 0.3, size = 0.5) +   # 信賴區間透明度
  # 線條顏色（深色）
  scale_color_manual(values = c("2024 spring" = "#006600",  # 深綠
                                "2024 fall"   = "#867052")) +  # 深咖啡
  
  labs(title = "",
       x = "Prev. 1-D Mois.",
       y = expression( ~ log[10](Aphids + 1)/m^2 )) +
  theme_classic(base_size = 8, base_family = "sans") +
  theme(plot.title=element_text(size=8, face="bold",hjust = 0.5),
        axis.title=element_text(size=8),
        axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x = element_text(size =6),
        axis.text.y = element_text(size = 6),
        legend.position= "none",        # 顯示圖例
        legend.key.size = unit(12,"points"),
        legend.title = element_blank()) +
  coord_cartesian(clip = "off") +
  
  annotate("text", x = 10, y = -1.6,
           label = paste0("r = ",
                          round(all_raw_aphid_abundance_1_day_mean_soil_moisture_correlation_coefficient, 3),
                          ", p < 0.001 "),
           hjust = 0, vjust = 0, size = 2.5, color = "red", fontface = "bold")  +
  scale_x_continuous(limits = c(6, 45)) +
  scale_y_continuous(limits = c(-2.0, 4.0))



#daily mean soil moisture
all_raw_aphid_abundance_1_day_mean_soil_moisture_spring_correlation_test <- cor.test(df_new_spring$aphid_abundance_log,
                                                                                     df_new_spring$soil_moisture_lag,
                                                                                     use = "complete.obs")
# Extract the correlation coefficient and p-value
all_raw_aphid_abundance_1_day_mean_soil_moisture_spring_correlation_coefficient <- all_raw_aphid_abundance_1_day_mean_soil_moisture_spring_correlation_test$estimate
all_raw_aphid_abundance_1_day_mean_soil_moisture_spring_p_value <- all_raw_aphid_abundance_1_day_mean_soil_moisture_spring_correlation_test$p.value


#daily mean soil moisture
all_raw_aphid_abundance_1_day_mean_soil_moisture_fall_correlation_test <- cor.test(df_new_fall$aphid_abundance_log,
                                                                                   df_new_fall$soil_moisture_lag,
                                                                                   use = "complete.obs")
# Extract the correlation coefficient and p-value
all_raw_aphid_abundance_1_day_mean_soil_moisture_fall_correlation_coefficient <- all_raw_aphid_abundance_1_day_mean_soil_moisture_fall_correlation_test$estimate
all_raw_aphid_abundance_1_day_mean_soil_moisture_fall_p_value <- all_raw_aphid_abundance_1_day_mean_soil_moisture_fall_correlation_test$p.value


library(scales)  # for alpha()
# scale_color_manual(values = c("2024 spring" = "#006600", "2024 fall" = "#867052"))
# Scatter plot with a linear trend line
plot_all_raw_1_day_mean_soil_moisture_aphid_abundance_seasons <- 
  ggplot(df_new,
         aes(x = soil_moisture_lag,
             y = aphid_abundance_log,
             color = `growing season`)) + 
  geom_point(aes(fill = after_scale(scales::alpha(color, 0.30))),
             stroke = 0.2, shape = 21, size = 0.75) + 
  # 設定回歸線顏色，根據不同的 'growing season' 來顯示不同顏色
  geom_smooth(data = df_new %>% filter(`growing season` == "2024 spring"), method = "lm", se = TRUE,
              color = "#006600",  # 深綠色回歸線
              fill = "#66CC66",   # 淺綠色信賴區間
              alpha = 0.4, size = 0.5) +   # 信賴區間透明度
  geom_smooth(data = df_new %>% filter(`growing season` == "2024 fall"), method = "lm", se = TRUE,
              color = "#8B4513",  # 紅棕色回歸線
              fill = "#C4B7A6",   # 淺咖啡信賴區間
              alpha = 0.4, size = 0.5) +   # 信賴區間透明度
  labs(title = "",
       x = "Prev. 1-D Mois.",
       y = expression( ~ log[10](Aphids + 1)/m^2 )) +
  theme_classic(base_size = 8, base_family = "sans") +
  theme(plot.title=element_text(size=8, face="bold",hjust = 0.5),
        axis.title=element_text(size=8),
        axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x = element_text(size =6),
        axis.text.y = element_text(size = 6),
        legend.position= "none",        # 顯示圖例
        legend.key.size = unit(12,"points"),
        legend.title = element_blank()) +
  coord_cartesian(clip = "off") +
  # 資料點顏色設置，確保資料點顏色正確
  scale_color_manual(values = c("2024 spring" = "#006600",  # 深綠
                                "2024 fall"   = "#867052")) +  # 深咖啡
  # 區間顏色設置，確保資料的顏色不會被覆蓋
  scale_fill_manual(values = c("2024 spring" = "#66CC66",  # 淺綠
                               "2024 fall"   = "#C4B7A6")) +  # 淺咖啡
  annotate("text", x = 10, y = -1.4,
           label = paste0("r = ",
                          round(all_raw_aphid_abundance_1_day_mean_soil_moisture_spring_correlation_coefficient, 3),
                          ", p < 0.001 "),
           hjust = 0, vjust = 0, size = 2.5, color = "#006600", fontface = "bold") +
  annotate("text", x = 10, y = -1.8,
           label = paste0("r = ",
                          round(all_raw_aphid_abundance_1_day_mean_soil_moisture_fall_correlation_coefficient, 3),
                          ", p < 0.001 "),
           hjust = 0, vjust = 0, size = 2.5, color = "#8B4513", fontface = "bold") +
  scale_x_continuous(limits = c(6, 45)) +
  scale_y_continuous(limits = c(-2.0, 4.0))


#3-day mean soil moisture

all_raw_aphid_abundance_3_day_mean_soil_moisture_correlation_test <- cor.test(df_new$aphid_abundance_log,
                                                                              df_new$three_day_mean_soil_moisture,
                                                                              use = "complete.obs")
# Extract the correlation coefficient and p-value
all_raw_aphid_abundance_3_day_mean_soil_moisture_correlation_coefficient <- all_raw_aphid_abundance_3_day_mean_soil_moisture_correlation_test$estimate
all_raw_aphid_abundance_3_day_mean_soil_moisture_p_value <- all_raw_aphid_abundance_3_day_mean_soil_moisture_correlation_test$p.value

plot_all_raw_3_day_mean_soil_moisture_aphid_abundance <- 
  ggplot(df_new,
         aes(x = three_day_mean_soil_moisture,
             y = aphid_abundance_log,
             color = `growing season`)) + 
  geom_point(aes(fill = after_scale(scales::alpha(color, 0.30))),
             stroke = 0.2, shape = 21, size = 0.75) +
  geom_smooth(method = "lm", se = TRUE,col = "blue", aes(group = 1),
              alpha = 0.3, size = 0.5) +   # 信賴區間透明度
  # 線條顏色（深色）
  scale_color_manual(values = c("2024 spring" = "#006600",  # 深綠
                                "2024 fall"   = "#867052")) +  # 深咖啡
  
  labs(title = "",
       x = "Prev. 3-D Mois.",
       y = expression( ~ log[10](Aphids + 1)/m^2 )) +
  theme_classic(base_size = 8, base_family = "sans") +
  theme(plot.title=element_text(size=8, face="bold",hjust = 0.5),
        axis.title=element_text(size=8),
        axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x = element_text(size =6),
        axis.text.y = element_text(size = 6),
        legend.position= "none",        # 顯示圖例
        legend.key.size = unit(12,"points"),
        legend.title = element_blank()) +
  coord_cartesian(clip = "off") +
  
  annotate("text", x = 10, y = -1.6,
           label = paste0("r = ",
                          round(all_raw_aphid_abundance_3_day_mean_soil_moisture_correlation_coefficient, 3),
                          ", p < 0.001 "),
           hjust = 0, vjust = 0, size = 2.5, color = "red", fontface = "bold")  +
  scale_x_continuous(limits = c(6, 45)) +
  scale_y_continuous(limits = c(-2.0, 4.0))



#daily mean soil moisture
all_raw_aphid_abundance_3_day_mean_soil_moisture_spring_correlation_test <- cor.test(df_new_spring$aphid_abundance_log,
                                                                                     df_new_spring$three_day_mean_soil_moisture,
                                                                                     use = "complete.obs")
# Extract the correlation coefficient and p-value
all_raw_aphid_abundance_3_day_mean_soil_moisture_spring_correlation_coefficient <- all_raw_aphid_abundance_3_day_mean_soil_moisture_spring_correlation_test$estimate
all_raw_aphid_abundance_3_day_mean_soil_moisture_spring_p_value <- all_raw_aphid_abundance_3_day_mean_soil_moisture_spring_correlation_test$p.value


#daily mean soil moisture
all_raw_aphid_abundance_3_day_mean_soil_moisture_fall_correlation_test <- cor.test(df_new_fall$aphid_abundance_log,
                                                                                   df_new_fall$three_day_mean_soil_moisture,
                                                                                   use = "complete.obs")
# Extract the correlation coefficient and p-value
all_raw_aphid_abundance_3_day_mean_soil_moisture_fall_correlation_coefficient <- all_raw_aphid_abundance_3_day_mean_soil_moisture_fall_correlation_test$estimate
all_raw_aphid_abundance_3_day_mean_soil_moisture_fall_p_value <- all_raw_aphid_abundance_3_day_mean_soil_moisture_fall_correlation_test$p.value


library(scales)  # for alpha()
# scale_color_manual(values = c("2024 spring" = "#006600", "2024 fall" = "#867052"))
# Scatter plot with a linear trend line
plot_all_raw_3_day_mean_soil_moisture_aphid_abundance_seasons <- 
  ggplot(df_new,
         aes(x = three_day_mean_soil_moisture,
             y = aphid_abundance_log,
             color = `growing season`)) + 
  geom_point(aes(fill = after_scale(scales::alpha(color, 0.30))),
             stroke = 0.2, shape = 21, size = 0.75) + 
  # 設定回歸線顏色，根據不同的 'growing season' 來顯示不同顏色
  geom_smooth(data = df_new %>% filter(`growing season` == "2024 spring"), method = "lm", se = TRUE,
              color = "#006600",  # 深綠色回歸線
              fill = "#66CC66",   # 淺綠色信賴區間
              alpha = 0.4, size = 0.5) +   # 信賴區間透明度
  geom_smooth(data = df_new %>% filter(`growing season` == "2024 fall"), method = "lm", se = TRUE,
              color = "#8B4513",  # 紅棕色回歸線
              fill = "#C4B7A6",   # 淺咖啡信賴區間
              alpha = 0.4, size = 0.5) +   # 信賴區間透明度
  labs(title = "",
       x = "Prev. 3-D Mois.",
       y = expression( ~ log[10](Aphids + 1)/m^2 )) +
  theme_classic(base_size = 8, base_family = "sans") +
  theme(plot.title=element_text(size=8, face="bold",hjust = 0.5),
        axis.title=element_text(size=8),
        axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x = element_text(size =6),
        axis.text.y = element_text(size = 6),
        legend.position= "none",        # 顯示圖例
        legend.key.size = unit(12,"points"),
        legend.title = element_blank()) +
  coord_cartesian(clip = "off") +
  # 線條顏色（深色）
  # 資料點顏色設置，確保資料點顏色正確
  scale_color_manual(values = c("2024 spring" = "#006600",  # 深綠
                                "2024 fall"   = "#867052")) +  # 深咖啡
  # 區間顏色設置，確保資料的顏色不會被覆蓋
  scale_fill_manual(values = c("2024 spring" = "#66CC66",  # 淺綠
                               "2024 fall"   = "#C4B7A6")) +  # 淺咖啡
  annotate("text", x = 10, y = -1.4,
           label = paste0("r = ",
                          round(all_raw_aphid_abundance_3_day_mean_soil_moisture_spring_correlation_coefficient, 3),
                          ", p = ",
                          signif(all_raw_aphid_abundance_3_day_mean_soil_moisture_spring_p_value, 3)),
           hjust = 0, vjust = 0, size = 2.5, color = "#006600") +
  annotate("text", x = 10, y = -1.8,
           label = paste0("r = ",
                          round(all_raw_aphid_abundance_3_day_mean_soil_moisture_fall_correlation_coefficient, 3),
                          ", p < 0.001 "),
           hjust = 0, vjust = 0, size = 2.5, color = "#8B4513", fontface = "bold") +
  scale_x_continuous(limits = c(6, 45)) +
  scale_y_continuous(limits = c(-2.0, 4.0))


#5-day mean soil moisture

all_raw_aphid_abundance_5_day_mean_soil_moisture_correlation_test <- cor.test(df_new$aphid_abundance_log,
                                                                              df_new$five_day_mean_soil_moisture,
                                                                              use = "complete.obs")
# Extract the correlation coefficient and p-value
all_raw_aphid_abundance_5_day_mean_soil_moisture_correlation_coefficient <- all_raw_aphid_abundance_5_day_mean_soil_moisture_correlation_test$estimate
all_raw_aphid_abundance_5_day_mean_soil_moisture_p_value <- all_raw_aphid_abundance_5_day_mean_soil_moisture_correlation_test$p.value

plot_all_raw_5_day_mean_soil_moisture_aphid_abundance <- 
  ggplot(df_new,
         aes(x = five_day_mean_soil_moisture,
             y = aphid_abundance_log,
             color = `growing season`)) + 
  geom_point(aes(fill = after_scale(scales::alpha(color, 0.30))),
             stroke = 0.2, shape = 21, size = 0.75) +
  geom_smooth(method = "lm", se = TRUE,col = "blue", aes(group = 1),
              alpha = 0.3, size = 0.5) +   # 信賴區間透明度
  # 線條顏色（深色）
  scale_color_manual(values = c("2024 spring" = "#006600",  # 深綠
                                "2024 fall"   = "#867052")) +  # 深咖啡
  
  labs(title = "",
       x = "Prev. 5-D Mois.",
       y = expression( ~ log[10](Aphids + 1)/m^2 )) +
  theme_classic(base_size = 8, base_family = "sans") +
  theme(plot.title=element_text(size=8, face="bold",hjust = 0.5),
        axis.title=element_text(size=8),
        axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x = element_text(size =6),
        axis.text.y = element_text(size = 6),
        legend.position= "none",        # 顯示圖例
        legend.key.size = unit(12,"points"),
        legend.title = element_blank()) +
  coord_cartesian(clip = "off") +
  
  annotate("text", x = 10, y = -1.6,
           label = paste0("r = ",
                          round(all_raw_aphid_abundance_5_day_mean_soil_moisture_correlation_coefficient, 3),
                          ", p < 0.05 "),
           hjust = 0, vjust = 0, size = 2.5, color = "red", fontface = "bold")  +
  scale_x_continuous(limits = c(6, 45)) +
  scale_y_continuous(limits = c(-2.0, 4.0))



#daily mean soil moisture
all_raw_aphid_abundance_5_day_mean_soil_moisture_spring_correlation_test <- cor.test(df_new_spring$aphid_abundance_log,
                                                                                     df_new_spring$five_day_mean_soil_moisture,
                                                                                     use = "complete.obs")
# Extract the correlation coefficient and p-value
all_raw_aphid_abundance_5_day_mean_soil_moisture_spring_correlation_coefficient <- all_raw_aphid_abundance_5_day_mean_soil_moisture_spring_correlation_test$estimate
all_raw_aphid_abundance_5_day_mean_soil_moisture_spring_p_value <- all_raw_aphid_abundance_5_day_mean_soil_moisture_spring_correlation_test$p.value


#daily mean soil moisture
all_raw_aphid_abundance_5_day_mean_soil_moisture_fall_correlation_test <- cor.test(df_new_fall$aphid_abundance_log,
                                                                                   df_new_fall$five_day_mean_soil_moisture,
                                                                                   use = "complete.obs")
# Extract the correlation coefficient and p-value
all_raw_aphid_abundance_5_day_mean_soil_moisture_fall_correlation_coefficient <- all_raw_aphid_abundance_5_day_mean_soil_moisture_fall_correlation_test$estimate
all_raw_aphid_abundance_5_day_mean_soil_moisture_fall_p_value <- all_raw_aphid_abundance_5_day_mean_soil_moisture_fall_correlation_test$p.value


library(scales)  # for alpha()
# scale_color_manual(values = c("2024 spring" = "#006600", "2024 fall" = "#867052"))
# Scatter plot with a linear trend line
plot_all_raw_5_day_mean_soil_moisture_aphid_abundance_seasons <- 
  ggplot(df_new,
         aes(x = five_day_mean_soil_moisture,
             y = aphid_abundance_log,
             color = `growing season`)) + 
  geom_point(aes(fill = after_scale(scales::alpha(color, 0.30))),
             stroke = 0.2, shape = 21, size = 0.75) + 
  # 設定回歸線顏色，根據不同的 'growing season' 來顯示不同顏色
  geom_smooth(data = df_new %>% filter(`growing season` == "2024 spring"), method = "lm", se = TRUE,
              color = "#006600",  # 深綠色回歸線
              fill = "#66CC66",   # 淺綠色信賴區間
              alpha = 0.4, size = 0.5) +   # 信賴區間透明度
  geom_smooth(data = df_new %>% filter(`growing season` == "2024 fall"), method = "lm", se = TRUE,
              color = "#8B4513",  # 紅棕色回歸線
              fill = "#C4B7A6",   # 淺咖啡信賴區間
              alpha = 0.4, size = 0.5) +   # 信賴區間透明度
  labs(title = "",
       x = "Prev. 5-D Mois.",
       y = expression( ~ log[10](Aphids + 1)/m^2 )) +
  theme_classic(base_size = 8, base_family = "sans") +
  theme(plot.title=element_text(size=8, face="bold",hjust = 0.5),
        axis.title=element_text(size=8),
        axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x = element_text(size =6),
        axis.text.y = element_text(size = 6),
        legend.position= "none",        # 顯示圖例
        legend.key.size = unit(12,"points"),
        legend.title = element_blank()) +
  coord_cartesian(clip = "off") +
  # 線條顏色（深色）
  # 資料點顏色設置，確保資料點顏色正確
  scale_color_manual(values = c("2024 spring" = "#006600",  # 深綠
                                "2024 fall"   = "#867052")) +  # 深咖啡
  # 區間顏色設置，確保資料的顏色不會被覆蓋
  scale_fill_manual(values = c("2024 spring" = "#66CC66",  # 淺綠
                               "2024 fall"   = "#C4B7A6")) +  # 淺咖啡
  annotate("text", x = 10, y = -1.4,
           label = paste0("r = ",
                          round(all_raw_aphid_abundance_5_day_mean_soil_moisture_spring_correlation_coefficient, 3),
                          ", p = ",
                          signif(all_raw_aphid_abundance_5_day_mean_soil_moisture_spring_p_value, 3)),
           hjust = 0, vjust = 0, size = 2.5, color = "#006600") +
  annotate("text", x = 10, y = -1.8,
           label = paste0("r = ",
                          round(all_raw_aphid_abundance_5_day_mean_soil_moisture_fall_correlation_coefficient, 3),
                          ", p < 0.001 "),
           hjust = 0, vjust = 0, size = 2.5, color = "#8B4513", fontface = "bold") +
  scale_x_continuous(limits = c(6, 45)) +
  scale_y_continuous(limits = c(-2.0, 4.0))

# 7-day mean soil moisture

all_raw_aphid_abundance_7_day_mean_soil_moisture_correlation_test <- cor.test(df_new$aphid_abundance_log,
                                                                              df_new$seven_day_mean_soil_moisture,
                                                                              use = "complete.obs")
# Extract the correlation coefficient and p-value
all_raw_aphid_abundance_7_day_mean_soil_moisture_correlation_coefficient <- all_raw_aphid_abundance_7_day_mean_soil_moisture_correlation_test$estimate
all_raw_aphid_abundance_7_day_mean_soil_moisture_p_value <- all_raw_aphid_abundance_7_day_mean_soil_moisture_correlation_test$p.value

plot_all_raw_7_day_mean_soil_moisture_aphid_abundance <- 
  ggplot(df_new,
         aes(x = seven_day_mean_soil_moisture,
             y = aphid_abundance_log,
             color = `growing season`)) + 
  geom_point(aes(fill = after_scale(scales::alpha(color, 0.30))),
             stroke = 0.2, shape = 21, size = 0.75) +
  geom_smooth(method = "lm", se = TRUE,col = "blue", aes(group = 1),
              alpha = 0.3, size = 0.5) +   # 信賴區間透明度
  # 線條顏色（深色）
  scale_color_manual(values = c("2024 spring" = "#006600",  # 深綠
                                "2024 fall"   = "#867052")) +  # 深咖啡
  
  labs(title = "",
       x = "Prev. 7-D Mois.",
       y = expression( ~ log[10](Aphids + 1)/m^2 )) +
  theme_classic(base_size = 8, base_family = "sans") +
  theme(plot.title=element_text(size=8, face="bold",hjust = 0.5),
        axis.title=element_text(size=8),
        axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x = element_text(size =6),
        axis.text.y = element_text(size = 6),
        legend.position= "none",        # 顯示圖例
        legend.key.size = unit(12,"points"),
        legend.title = element_blank()) +
  coord_cartesian(clip = "off") +
  
  annotate("text", x = 10, y = -1.6,
           label = paste0("r = ",
                          round(all_raw_aphid_abundance_7_day_mean_soil_moisture_correlation_coefficient, 3),
                          ", p < 0.05 "),
           hjust = 0, vjust = 0, size = 2.5, color = "red", fontface = "bold")  +
  scale_x_continuous(limits = c(6, 45)) +
  scale_y_continuous(limits = c(-2.0, 4.0))



#daily mean soil moisture
all_raw_aphid_abundance_7_day_mean_soil_moisture_spring_correlation_test <- cor.test(df_new_spring$aphid_abundance_log,
                                                                                     df_new_spring$seven_day_mean_soil_moisture,
                                                                                     use = "complete.obs")
# Extract the correlation coefficient and p-value
all_raw_aphid_abundance_7_day_mean_soil_moisture_spring_correlation_coefficient <- all_raw_aphid_abundance_7_day_mean_soil_moisture_spring_correlation_test$estimate
all_raw_aphid_abundance_7_day_mean_soil_moisture_spring_p_value <- all_raw_aphid_abundance_7_day_mean_soil_moisture_spring_correlation_test$p.value


#daily mean soil moisture
all_raw_aphid_abundance_7_day_mean_soil_moisture_fall_correlation_test <- cor.test(df_new_fall$aphid_abundance_log,
                                                                                   df_new_fall$seven_day_mean_soil_moisture,
                                                                                   use = "complete.obs")
# Extract the correlation coefficient and p-value
all_raw_aphid_abundance_7_day_mean_soil_moisture_fall_correlation_coefficient <- all_raw_aphid_abundance_7_day_mean_soil_moisture_fall_correlation_test$estimate
all_raw_aphid_abundance_7_day_mean_soil_moisture_fall_p_value <- all_raw_aphid_abundance_7_day_mean_soil_moisture_fall_correlation_test$p.value


library(scales)  # for alpha()
# scale_color_manual(values = c("2024 spring" = "#006600", "2024 fall" = "#867052"))
# Scatter plot with a linear trend line
plot_all_raw_7_day_mean_soil_moisture_aphid_abundance_seasons <- 
  ggplot(df_new,
         aes(x = seven_day_mean_soil_moisture,
             y = aphid_abundance_log,
             color = `growing season`)) + 
  geom_point(aes(fill = after_scale(scales::alpha(color, 0.30))),
             stroke = 0.2, shape = 21, size = 0.75) + 
  # 設定回歸線顏色，根據不同的 'growing season' 來顯示不同顏色
  geom_smooth(data = df_new %>% filter(`growing season` == "2024 spring"), method = "lm", se = TRUE,
              color = "#006600",  # 深綠色回歸線
              fill = "#66CC66",   # 淺綠色信賴區間
              alpha = 0.4, size = 0.5) +   # 信賴區間透明度
  geom_smooth(data = df_new %>% filter(`growing season` == "2024 fall"), method = "lm", se = TRUE,
              color = "#8B4513",  # 紅棕色回歸線
              fill = "#C4B7A6",   # 淺咖啡信賴區間
              alpha = 0.4, size = 0.5) +   # 信賴區間透明度
  labs(title = "",
       x = "Prev. 7-D Mois.",
       y = expression( ~ log[10](Aphids + 1)/m^2 )) +
  theme_classic(base_size = 8, base_family = "sans") +
  theme(plot.title=element_text(size=8, face="bold",hjust = 0.5),
        axis.title=element_text(size=8),
        axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x = element_text(size =6),
        axis.text.y = element_text(size = 6),
        legend.position= "none",        # 顯示圖例
        legend.key.size = unit(12,"points"),
        legend.title = element_blank()) +
  coord_cartesian(clip = "off") +
  # 線條顏色（深色）
  # 資料點顏色設置，確保資料點顏色正確
  scale_color_manual(values = c("2024 spring" = "#006600",  # 深綠
                                "2024 fall"   = "#867052")) +  # 深咖啡
  # 區間顏色設置，確保資料的顏色不會被覆蓋
  scale_fill_manual(values = c("2024 spring" = "#66CC66",  # 淺綠
                               "2024 fall"   = "#C4B7A6")) +  # 淺咖啡
  annotate("text", x = 10, y = -1.4,
           label = paste0("r = ",
                          round(all_raw_aphid_abundance_7_day_mean_soil_moisture_spring_correlation_coefficient, 3),
                          ", p = ",
                          signif(all_raw_aphid_abundance_7_day_mean_soil_moisture_spring_p_value, 3)),
           hjust = 0, vjust = 0, size = 2.5, color = "#006600") +
  annotate("text", x = 10, y = -1.8,
           label = paste0("r = ",
                          round(all_raw_aphid_abundance_7_day_mean_soil_moisture_fall_correlation_coefficient, 3),
                          ", p < 0.001 "),
           hjust = 0, vjust = 0, size = 2.5, color = "#8B4513", fontface = "bold") +
  scale_x_continuous(limits = c(6, 45)) +
  scale_y_continuous(limits = c(-2.0, 4.0))





#----------------------------------------------------------------------------------------------------------------------------------------------

#daily mean soil EC
all_raw_aphid_abundance_daily_mean_soil_conductivity_correlation_test <- cor.test(df_new$aphid_abundance_log,
                                                                                  df_new$daily_mean_soil_conductivity,
                                                                                  use = "complete.obs")
# Extract the correlation coefficient and p-value
all_raw_aphid_abundance_daily_mean_soil_conductivity_correlation_coefficient <- all_raw_aphid_abundance_daily_mean_soil_conductivity_correlation_test$estimate
all_raw_aphid_abundance_daily_mean_soil_conductivity_p_value <- all_raw_aphid_abundance_daily_mean_soil_conductivity_correlation_test$p.value

plot_all_raw_daily_mean_soil_conductivity_aphid_abundance <- 
  ggplot(df_new,
         aes(x = daily_mean_soil_conductivity,
             y = aphid_abundance_log,
             color = `growing season`)) + 
  geom_point(aes(fill = after_scale(scales::alpha(color, 0.30))),
             stroke = 0.2, shape = 21, size = 0.75) +
  geom_smooth(method = "lm", se = TRUE,col = "blue", aes(group = 1),
              alpha = 0.3, size = 0.5) +   # 信賴區間透明度
  
  labs(title = "",
       x = "Daily EC",
       y = expression( ~ log[10](Aphids + 1)/m^2 )) +
  theme_classic(base_size = 8, base_family = "sans") +
  theme(plot.title=element_text(size=8, face="bold",hjust = 0.5),
        axis.title=element_text(size=8),
        axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x = element_text(size =6),
        axis.text.y = element_text(size = 6),
        legend.position= "none",        # 顯示圖例
        legend.key.size = unit(12,"points"),
        legend.title = element_blank()) +
  coord_cartesian(clip = "off") +
  scale_color_manual(values = c("2024 spring" = "#006600",  # 深綠
                                "2024 fall"   = "#867052")) +  # 深咖啡
  
  annotate("text", x = 0.02, y = -1.6,
           label = paste0("r = ",
                          round(all_raw_aphid_abundance_daily_mean_soil_conductivity_correlation_coefficient, 3),
                          ", p = ",
                          signif(all_raw_aphid_abundance_daily_mean_soil_conductivity_p_value, 3)),
           hjust = 0, vjust = 0, size = 2.5, color = "black")  +
  scale_x_continuous(limits = c(0, 1.1)) +
  scale_y_continuous(limits = c(-2.0, 4.0))

max(df_new$daily_mean_soil_conductivity)

ggsave("plot_all_raw_daily_mean_soil_moisture_aphid_abundance.png", plot = plot_all_raw_daily_mean_soil_moisture_aphid_abundance, 
       width = 4, height = 5, units = "cm", dpi = 300)


# spring

#daily mean soil EC
all_raw_aphid_abundance_daily_mean_soil_conductivity_spring_correlation_test <- cor.test(df_new_spring$aphid_abundance_log,
                                                                                         df_new_spring$daily_mean_soil_conductivity,
                                                                                         use = "complete.obs")
# Extract the correlation coefficient and p-value
all_raw_aphid_abundance_daily_mean_soil_conductivity_spring_correlation_coefficient <- all_raw_aphid_abundance_daily_mean_soil_conductivity_spring_correlation_test$estimate
all_raw_aphid_abundance_daily_mean_soil_conductivity_spring_p_value <- all_raw_aphid_abundance_daily_mean_soil_conductivity_spring_correlation_test$p.value


#daily mean soil moisture
all_raw_aphid_abundance_daily_mean_soil_conductivity_fall_correlation_test <- cor.test(df_new_fall$aphid_abundance_log,
                                                                                       df_new_fall$daily_mean_soil_conductivity,
                                                                                       use = "complete.obs")
# Extract the correlation coefficient and p-value
all_raw_aphid_abundance_daily_mean_soil_conductivity_fall_correlation_coefficient <- all_raw_aphid_abundance_daily_mean_soil_conductivity_fall_correlation_test$estimate
all_raw_aphid_abundance_daily_mean_soil_conductivity_fall_p_value <- all_raw_aphid_abundance_daily_mean_soil_conductivity_fall_correlation_test$p.value



# Scatter plot with a linear trend line
plot_all_raw_daily_mean_soil_conductivity_aphid_abundance_season <- 
  ggplot(df_new,
         aes(x = daily_mean_soil_conductivity,
             y = aphid_abundance_log,
             color = `growing season`)) + 
  geom_point(aes(fill = after_scale(scales::alpha(color, 0.30))),
             stroke = 0.2, shape = 21, size = 0.75) + 
  # 設定回歸線顏色，根據不同的 'growing season' 來顯示不同顏色
  geom_smooth(data = df_new %>% filter(`growing season` == "2024 spring"), method = "lm", se = TRUE,
              color = "#006600",  # 深綠色回歸線
              fill = "#66CC66",   # 淺綠色信賴區間
              alpha = 0.4, size = 0.5) +   # 信賴區間透明度
  geom_smooth(data = df_new %>% filter(`growing season` == "2024 fall"), method = "lm", se = TRUE,
              color = "#8B4513",  # 紅棕色回歸線
              fill = "#C4B7A6",   # 淺咖啡信賴區間
              alpha = 0.4, size = 0.5) +   # 信賴區間透明度
  labs(title = "",
       x = "Daily EC",
       y = expression( ~ log[10](Aphids + 1)/m^2 )) +
  theme_classic(base_size = 8, base_family = "sans") +
  theme(plot.title=element_text(size=8, face="bold",hjust = 0.5),
        axis.title=element_text(size=8),
        axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x = element_text(size =6),
        axis.text.y = element_text(size = 6),
        legend.position= "none",        # 顯示圖例
        legend.key.size = unit(12,"points"),
        legend.title = element_blank()) +
  coord_cartesian(clip = "off") + # 淺咖啡
  # 資料點顏色設置，確保資料點顏色正確
  scale_color_manual(values = c("2024 spring" = "#006600",  # 深綠
                                "2024 fall"   = "#867052")) +  # 深咖啡
  # 區間顏色設置，確保資料的顏色不會被覆蓋
  scale_fill_manual(values = c("2024 spring" = "#66CC66",  # 淺綠
                               "2024 fall"   = "#C4B7A6")) +  # 淺咖啡
  annotate("text", x = 0.02, y = -1.4,
           label = paste0("r = ",
                          round(all_raw_aphid_abundance_daily_mean_soil_conductivity_spring_correlation_coefficient, 3),
                          ", p = ",
                          signif(all_raw_aphid_abundance_daily_mean_soil_conductivity_spring_p_value, 3)),
           hjust = 0, vjust = 0, size = 2.5, color = "#006600") +
  annotate("text", x = 0.02, y = -1.8,
           label = paste0("r = ",
                          round(all_raw_aphid_abundance_daily_mean_soil_conductivity_fall_correlation_coefficient, 3),
                          ", p < 0.05 "),
           hjust = 0, vjust = 0, size = 2.5, color = "#8B4513", fontface = "bold") +
  scale_x_continuous(limits = c(0, 1.1)) +
  scale_y_continuous(limits = c(-2.0, 4.0))

ggsave("plot_stage_daily_mean_soil_conductivity_aphid_abundance.png", plot = plot_stage_daily_mean_soil_conductivity_aphid_abundance, 
       width = 4, height = 5, units = "cm", dpi = 300)

# 1-day mean soil conductivity
#daily mean soil moisture
all_raw_aphid_abundance_1_day_mean_soil_conductivity_correlation_test <- cor.test(df_new$aphid_abundance_log,
                                                                                  df_new$soil_EC_lag,
                                                                                  use = "complete.obs")
# Extract the correlation coefficient and p-value
all_raw_aphid_abundance_1_day_mean_soil_conductivity_correlation_coefficient <- all_raw_aphid_abundance_1_day_mean_soil_conductivity_correlation_test$estimate
all_raw_aphid_abundance_1_day_mean_soil_conductivity_p_value <- all_raw_aphid_abundance_1_day_mean_soil_conductivity_correlation_test$p.value

plot_all_raw_1_day_mean_soil_conductivity_aphid_abundance <- 
  ggplot(df_new,
         aes(x = soil_EC_lag,
             y = aphid_abundance_log,
             color = `growing season`)) + 
  geom_point(aes(fill = after_scale(scales::alpha(color, 0.30))),
             stroke = 0.2, shape = 21, size = 0.75) +
  geom_smooth(method = "lm", se = TRUE,col = "blue", aes(group = 1),
              alpha = 0.3, size = 0.5) +   # 信賴區間透明度
  
  labs(title = "",
       x = "Prev. 1-D EC",
       y = expression( ~ log[10](Aphids + 1)/m^2 )) +
  theme_classic(base_size = 8, base_family = "sans") +
  theme(plot.title=element_text(size=8, face="bold",hjust = 0.5),
        axis.title=element_text(size=8),
        axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x = element_text(size =6),
        axis.text.y = element_text(size = 6),
        legend.position= "none",        # 顯示圖例
        legend.key.size = unit(12,"points"),
        legend.title = element_blank()) +
  coord_cartesian(clip = "off") +
  scale_color_manual(values = c("2024 spring" = "#006600",  # 深綠
                                "2024 fall"   = "#867052")) +  # 深咖啡
  
  annotate("text", x = 0.02, y = -1.6,
           label = paste0("r = ",
                          round(all_raw_aphid_abundance_1_day_mean_soil_conductivity_correlation_coefficient, 3),
                          ", p < 0.05 "),
           hjust = 0, vjust = 0, size = 2.5, color = "red", fontface = "bold")  +
  scale_x_continuous(limits = c(0, 0.6)) +
  scale_y_continuous(limits = c(-2.0, 4.0))

max(df_new$daily_mean_soil_conductivity)




# spring

#daily mean soil EC
all_raw_aphid_abundance_1_day_mean_soil_conductivity_spring_correlation_test <- cor.test(df_new_spring$aphid_abundance_log,
                                                                                         df_new_spring$soil_EC_lag,
                                                                                         use = "complete.obs")
# Extract the correlation coefficient and p-value
all_raw_aphid_abundance_1_day_mean_soil_conductivity_spring_correlation_coefficient <- all_raw_aphid_abundance_1_day_mean_soil_conductivity_spring_correlation_test$estimate
all_raw_aphid_abundance_1_day_mean_soil_conductivity_spring_p_value <- all_raw_aphid_abundance_1_day_mean_soil_conductivity_spring_correlation_test$p.value


#daily mean soil moisture
all_raw_aphid_abundance_1_day_mean_soil_conductivity_fall_correlation_test <- cor.test(df_new_fall$aphid_abundance_log,
                                                                                       df_new_fall$soil_EC_lag,
                                                                                       use = "complete.obs")
# Extract the correlation coefficient and p-value
all_raw_aphid_abundance_1_day_mean_soil_conductivity_fall_correlation_coefficient <- all_raw_aphid_abundance_1_day_mean_soil_conductivity_fall_correlation_test$estimate
all_raw_aphid_abundance_1_day_mean_soil_conductivity_fall_p_value <- all_raw_aphid_abundance_1_day_mean_soil_conductivity_fall_correlation_test$p.value



# Scatter plot with a linear trend line
plot_all_raw_1_day_mean_soil_conductivity_aphid_abundance_season <- 
  ggplot(df_new,
         aes(x = soil_EC_lag,
             y = aphid_abundance_log,
             color = `growing season`)) + 
  geom_point(aes(fill = after_scale(scales::alpha(color, 0.30))),
             stroke = 0.2, shape = 21, size = 0.75) + 
  # 設定回歸線顏色，根據不同的 'growing season' 來顯示不同顏色
  geom_smooth(data = df_new %>% filter(`growing season` == "2024 spring"), method = "lm", se = TRUE,
              color = "#006600",  # 深綠色回歸線
              fill = "#66CC66",   # 淺綠色信賴區間
              alpha = 0.4, size = 0.5) +   # 信賴區間透明度
  geom_smooth(data = df_new %>% filter(`growing season` == "2024 fall"), method = "lm", se = TRUE,
              color = "#8B4513",  # 紅棕色回歸線
              fill = "#C4B7A6",   # 淺咖啡信賴區間
              alpha = 0.4, size = 0.5) +   # 信賴區間透明度
  labs(title = "",
       x = "Prev. 1-D EC",
       y = expression( ~ log[10](Aphids + 1)/m^2 )) +
  theme_classic(base_size = 8, base_family = "sans") +
  theme(plot.title=element_text(size=8, face="bold",hjust = 0.5),
        axis.title=element_text(size=8),
        axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x = element_text(size =6),
        axis.text.y = element_text(size = 6),
        legend.position= "none",        # 顯示圖例
        legend.key.size = unit(12,"points"),
        legend.title = element_blank()) +
  coord_cartesian(clip = "off") + # 淺咖啡
  # 資料點顏色設置，確保資料點顏色正確
  scale_color_manual(values = c("2024 spring" = "#006600",  # 深綠
                                "2024 fall"   = "#867052")) +  # 深咖啡
  # 區間顏色設置，確保資料的顏色不會被覆蓋
  scale_fill_manual(values = c("2024 spring" = "#66CC66",  # 淺綠
                               "2024 fall"   = "#C4B7A6")) +  # 淺咖啡
  annotate("text", x = 0.02, y = -1.4,
           label = paste0("r = ",
                          round(all_raw_aphid_abundance_1_day_mean_soil_conductivity_spring_correlation_coefficient, 3),
                          ", p = ",
                          signif(all_raw_aphid_abundance_1_day_mean_soil_conductivity_spring_p_value, 3)),
           hjust = 0, vjust = 0, size = 2.5, color = "#006600") +
  annotate("text", x = 0.02, y = -1.8,
           label = paste0("r = ",
                          round(all_raw_aphid_abundance_1_day_mean_soil_conductivity_fall_correlation_coefficient, 3),
                          ", p < 0.001 "),
           hjust = 0, vjust = 0, size = 2.5, color = "#8B4513", fontface = "bold") +
  scale_x_continuous(limits = c(0, 0.6)) +
  scale_y_continuous(limits = c(-2.0, 4.0))

# 3-day mean soil conductivity
all_raw_aphid_abundance_3_day_mean_soil_conductivity_correlation_test <- cor.test(df_new$aphid_abundance_log,
                                                                                  df_new$three_day_mean_soil_conductivity,
                                                                                  use = "complete.obs")
# Extract the correlation coefficient and p-value
all_raw_aphid_abundance_3_day_mean_soil_conductivity_correlation_coefficient <- all_raw_aphid_abundance_3_day_mean_soil_conductivity_correlation_test$estimate
all_raw_aphid_abundance_3_day_mean_soil_conductivity_p_value <- all_raw_aphid_abundance_3_day_mean_soil_conductivity_correlation_test$p.value

plot_all_raw_3_day_mean_soil_conductivity_aphid_abundance <- 
  ggplot(df_new,
         aes(x = three_day_mean_soil_conductivity,
             y = aphid_abundance_log,
             color = `growing season`)) + 
  geom_point(aes(fill = after_scale(scales::alpha(color, 0.30))),
             stroke = 0.2, shape = 21, size = 0.75) +
  geom_smooth(method = "lm", se = TRUE,col = "blue", aes(group = 1),
              alpha = 0.3, size = 0.5) +   # 信賴區間透明度
  
  labs(title = "",
       x = "Prev. 3-D EC",
       y = expression( ~ log[10](Aphids + 1)/m^2 )) +
  theme_classic(base_size = 8, base_family = "sans") +
  theme(plot.title=element_text(size=8, face="bold",hjust = 0.5),
        axis.title=element_text(size=8),
        axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x = element_text(size =6),
        axis.text.y = element_text(size = 6),
        legend.position= "none",        # 顯示圖例
        legend.key.size = unit(12,"points"),
        legend.title = element_blank()) +
  coord_cartesian(clip = "off") +
  scale_color_manual(values = c("2024 spring" = "#006600",  # 深綠
                                "2024 fall"   = "#867052")) +  # 深咖啡
  
  annotate("text", x = 0.02, y = -1.6,
           label = paste0("r = ",
                          round(all_raw_aphid_abundance_3_day_mean_soil_conductivity_correlation_coefficient, 3),
                          ", p = ",
                          signif(all_raw_aphid_abundance_3_day_mean_soil_conductivity_p_value, 3)),
           hjust = 0, vjust = 0, size = 2.5, color = "black")  +
  scale_x_continuous(limits = c(0, 0.75)) +
  scale_y_continuous(limits = c(-2.0, 4.0))



# spring

#daily mean soil EC
all_raw_aphid_abundance_3_day_mean_soil_conductivity_spring_correlation_test <- cor.test(df_new_spring$aphid_abundance_log,
                                                                                         df_new_spring$three_day_mean_soil_conductivity,
                                                                                         use = "complete.obs")
# Extract the correlation coefficient and p-value
all_raw_aphid_abundance_3_day_mean_soil_conductivity_spring_correlation_coefficient <- all_raw_aphid_abundance_3_day_mean_soil_conductivity_spring_correlation_test$estimate
all_raw_aphid_abundance_3_day_mean_soil_conductivity_spring_p_value <- all_raw_aphid_abundance_3_day_mean_soil_conductivity_spring_correlation_test$p.value


#daily mean soil moisture
all_raw_aphid_abundance_3_day_mean_soil_conductivity_fall_correlation_test <- cor.test(df_new_fall$aphid_abundance_log,
                                                                                       df_new_fall$three_day_mean_soil_conductivity,
                                                                                       use = "complete.obs")
# Extract the correlation coefficient and p-value
all_raw_aphid_abundance_3_day_mean_soil_conductivity_fall_correlation_coefficient <- all_raw_aphid_abundance_3_day_mean_soil_conductivity_fall_correlation_test$estimate
all_raw_aphid_abundance_3_day_mean_soil_conductivity_fall_p_value <- all_raw_aphid_abundance_3_day_mean_soil_conductivity_fall_correlation_test$p.value



# Scatter plot with a linear trend line
plot_all_raw_3_day_mean_soil_conductivity_aphid_abundance_season <- 
  ggplot(df_new,
         aes(x = three_day_mean_soil_conductivity,
             y = aphid_abundance_log,
             color = `growing season`)) + 
  geom_point(aes(fill = after_scale(scales::alpha(color, 0.30))),
             stroke = 0.2, shape = 21, size = 0.75) + 
  # 設定回歸線顏色，根據不同的 'growing season' 來顯示不同顏色
  geom_smooth(data = df_new %>% filter(`growing season` == "2024 spring"), method = "lm", se = TRUE,
              color = "#006600",  # 深綠色回歸線
              fill = "#66CC66",   # 淺綠色信賴區間
              alpha = 0.4, size = 0.5) +   # 信賴區間透明度
  geom_smooth(data = df_new %>% filter(`growing season` == "2024 fall"), method = "lm", se = TRUE,
              color = "#8B4513",  # 紅棕色回歸線
              fill = "#C4B7A6",   # 淺咖啡信賴區間
              alpha = 0.4, size = 0.5) +   # 信賴區間透明度
  labs(title = "",
       x = "Prev. 3-D EC",
       y = expression( ~ log[10](Aphids + 1)/m^2 )) +
  theme_classic(base_size = 8, base_family = "sans") +
  theme(plot.title=element_text(size=8, face="bold",hjust = 0.5),
        axis.title=element_text(size=8),
        axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x = element_text(size =6),
        axis.text.y = element_text(size = 6),
        legend.position= "none",        # 顯示圖例
        legend.key.size = unit(12,"points"),
        legend.title = element_blank()) +
  coord_cartesian(clip = "off") + # 淺咖啡
  # 資料點顏色設置，確保資料點顏色正確
  scale_color_manual(values = c("2024 spring" = "#006600",  # 深綠
                                "2024 fall"   = "#867052")) +  # 深咖啡
  # 區間顏色設置，確保資料的顏色不會被覆蓋
  scale_fill_manual(values = c("2024 spring" = "#66CC66",  # 淺綠
                               "2024 fall"   = "#C4B7A6")) +  # 淺咖啡
  annotate("text", x = 0.02, y = -1.4,
           label = paste0("r = ",
                          round(all_raw_aphid_abundance_3_day_mean_soil_conductivity_spring_correlation_coefficient, 3),
                          ", p = ",
                          signif(all_raw_aphid_abundance_3_day_mean_soil_conductivity_spring_p_value, 3)),
           hjust = 0, vjust = 0, size = 2.5, color = "#006600") +
  annotate("text", x = 0.02, y = -1.8,
           label = paste0("r = ",
                          round(all_raw_aphid_abundance_3_day_mean_soil_conductivity_fall_correlation_coefficient, 3),
                          ", p < 0.001 "),
           hjust = 0, vjust = 0, size = 2.5, color = "#8B4513", fontface = "bold") +
  scale_x_continuous(limits = c(0, 0.75)) +
  scale_y_continuous(limits = c(-2.0, 4.0))


# 5-day mean soil conductivity

all_raw_aphid_abundance_5_day_mean_soil_conductivity_correlation_test <- cor.test(df_new$aphid_abundance_log,
                                                                                  df_new$five_day_mean_soil_conductivity,
                                                                                  use = "complete.obs")
# Extract the correlation coefficient and p-value
all_raw_aphid_abundance_5_day_mean_soil_conductivity_correlation_coefficient <- all_raw_aphid_abundance_5_day_mean_soil_conductivity_correlation_test$estimate
all_raw_aphid_abundance_5_day_mean_soil_conductivity_p_value <- all_raw_aphid_abundance_5_day_mean_soil_conductivity_correlation_test$p.value

plot_all_raw_5_day_mean_soil_conductivity_aphid_abundance <- 
  ggplot(df_new,
         aes(x = five_day_mean_soil_conductivity,
             y = aphid_abundance_log,
             color = `growing season`)) + 
  geom_point(aes(fill = after_scale(scales::alpha(color, 0.30))),
             stroke = 0.2, shape = 21, size = 0.75) +
  geom_smooth(method = "lm", se = TRUE,col = "blue", aes(group = 1),
              alpha = 0.3, size = 0.5) +   # 信賴區間透明度
  
  labs(title = "",
       x = "Prev. 5-D EC",
       y = expression( ~ log[10](Aphids + 1)/m^2 )) +
  theme_classic(base_size = 8, base_family = "sans") +
  theme(plot.title=element_text(size=8, face="bold",hjust = 0.5),
        axis.title=element_text(size=8),
        axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x = element_text(size =6),
        axis.text.y = element_text(size = 6),
        legend.position= "none",        # 顯示圖例
        legend.key.size = unit(12,"points"),
        legend.title = element_blank()) +
  coord_cartesian(clip = "off") +
  scale_color_manual(values = c("2024 spring" = "#006600",  # 深綠
                                "2024 fall"   = "#867052")) +  # 深咖啡
  
  annotate("text", x = 0.02, y = -1.6,
           label = paste0("r = ",
                          round(all_raw_aphid_abundance_5_day_mean_soil_conductivity_correlation_coefficient, 3),
                          ", p = ",
                          signif(all_raw_aphid_abundance_5_day_mean_soil_conductivity_p_value, 3)),
           hjust = 0, vjust = 0, size = 2.5, color = "black")  +
  scale_x_continuous(limits = c(0, 1.5)) +
  scale_y_continuous(limits = c(-2.0, 4.0))


max(df_new$five_day_mean_soil_conductivity)
# spring

#daily mean soil EC
all_raw_aphid_abundance_5_day_mean_soil_conductivity_spring_correlation_test <- cor.test(df_new_spring$aphid_abundance_log,
                                                                                         df_new_spring$five_day_mean_soil_conductivity,
                                                                                         use = "complete.obs")
# Extract the correlation coefficient and p-value
all_raw_aphid_abundance_5_day_mean_soil_conductivity_spring_correlation_coefficient <- all_raw_aphid_abundance_5_day_mean_soil_conductivity_spring_correlation_test$estimate
all_raw_aphid_abundance_5_day_mean_soil_conductivity_spring_p_value <- all_raw_aphid_abundance_5_day_mean_soil_conductivity_spring_correlation_test$p.value


#daily mean soil moisture
all_raw_aphid_abundance_5_day_mean_soil_conductivity_fall_correlation_test <- cor.test(df_new_fall$aphid_abundance_log,
                                                                                       df_new_fall$five_day_mean_soil_conductivity,
                                                                                       use = "complete.obs")
# Extract the correlation coefficient and p-value
all_raw_aphid_abundance_5_day_mean_soil_conductivity_fall_correlation_coefficient <- all_raw_aphid_abundance_5_day_mean_soil_conductivity_fall_correlation_test$estimate
all_raw_aphid_abundance_5_day_mean_soil_conductivity_fall_p_value <- all_raw_aphid_abundance_5_day_mean_soil_conductivity_fall_correlation_test$p.value



# Scatter plot with a linear trend line
plot_all_raw_5_day_mean_soil_conductivity_aphid_abundance_season <- 
  ggplot(df_new,
         aes(x = five_day_mean_soil_conductivity,
             y = aphid_abundance_log,
             color = `growing season`)) + 
  geom_point(aes(fill = after_scale(scales::alpha(color, 0.30))),
             stroke = 0.2, shape = 21, size = 0.75) + 
  # 設定回歸線顏色，根據不同的 'growing season' 來顯示不同顏色
  geom_smooth(data = df_new %>% filter(`growing season` == "2024 spring"), method = "lm", se = TRUE,
              color = "#006600",  # 深綠色回歸線
              fill = "#66CC66",   # 淺綠色信賴區間
              alpha = 0.4, size = 0.5) +   # 信賴區間透明度
  geom_smooth(data = df_new %>% filter(`growing season` == "2024 fall"), method = "lm", se = TRUE,
              color = "#8B4513",  # 紅棕色回歸線
              fill = "#C4B7A6",   # 淺咖啡信賴區間
              alpha = 0.4, size = 0.5) +   # 信賴區間透明度
  labs(title = "",
       x = "Prev. 5-D EC",
       y = expression( ~ log[10](Aphids + 1)/m^2 )) +
  theme_classic(base_size = 8, base_family = "sans") +
  theme(plot.title=element_text(size=8, face="bold",hjust = 0.5),
        axis.title=element_text(size=8),
        axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x = element_text(size =6),
        axis.text.y = element_text(size = 6),
        legend.position= "none",        # 顯示圖例
        legend.key.size = unit(12,"points"),
        legend.title = element_blank()) +
  coord_cartesian(clip = "off") + # 淺咖啡
  # 資料點顏色設置，確保資料點顏色正確
  scale_color_manual(values = c("2024 spring" = "#006600",  # 深綠
                                "2024 fall"   = "#867052")) +  # 深咖啡
  # 區間顏色設置，確保資料的顏色不會被覆蓋
  scale_fill_manual(values = c("2024 spring" = "#66CC66",  # 淺綠
                               "2024 fall"   = "#C4B7A6")) +  # 淺咖啡
  annotate("text", x = 0.02, y = -1.4,
           label = paste0("r = ",
                          round(all_raw_aphid_abundance_5_day_mean_soil_conductivity_spring_correlation_coefficient, 3),
                          ", p = ",
                          signif(all_raw_aphid_abundance_5_day_mean_soil_conductivity_spring_p_value, 3)),
           hjust = 0, vjust = 0, size = 2.5, color = "#006600") +
  annotate("text", x = 0.02, y = -1.8,
           label = paste0("r = ",
                          round(all_raw_aphid_abundance_5_day_mean_soil_conductivity_fall_correlation_coefficient, 3),
                          ", p = ",
                          signif(all_raw_aphid_abundance_5_day_mean_soil_conductivity_fall_p_value, 3)),
           hjust = 0, vjust = 0, size = 2.5, color = "#8B4513") +
  scale_x_continuous(limits = c(0, 1.5)) +
  scale_y_continuous(limits = c(-2.0, 4.0))

# 7-day mean soil conductivity
all_raw_aphid_abundance_7_day_mean_soil_conductivity_correlation_test <- cor.test(df_new$aphid_abundance_log,
                                                                                  df_new$seven_day_mean_soil_conductivity,
                                                                                  use = "complete.obs")
# Extract the correlation coefficient and p-value
all_raw_aphid_abundance_7_day_mean_soil_conductivity_correlation_coefficient <- all_raw_aphid_abundance_7_day_mean_soil_conductivity_correlation_test$estimate
all_raw_aphid_abundance_7_day_mean_soil_conductivity_p_value <- all_raw_aphid_abundance_7_day_mean_soil_conductivity_correlation_test$p.value

plot_all_raw_7_day_mean_soil_conductivity_aphid_abundance <- 
  ggplot(df_new,
         aes(x = seven_day_mean_soil_conductivity,
             y = aphid_abundance_log,
             color = `growing season`)) + 
  geom_point(aes(fill = after_scale(scales::alpha(color, 0.30))),
             stroke = 0.2, shape = 21, size = 0.75) +
  geom_smooth(method = "lm", se = TRUE,col = "blue", aes(group = 1),
              alpha = 0.3, size = 0.5) +   # 信賴區間透明度
  
  labs(title = "",
       x = "Prev. 7-D EC",
       y = expression( ~ log[10](Aphids + 1)/m^2 )) +
  theme_classic(base_size = 8, base_family = "sans") +
  theme(plot.title=element_text(size=8, face="bold",hjust = 0.5),
        axis.title=element_text(size=8),
        axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x = element_text(size =6),
        axis.text.y = element_text(size = 6),
        legend.position= "none",        # 顯示圖例
        legend.key.size = unit(12,"points"),
        legend.title = element_blank()) +
  coord_cartesian(clip = "off") +
  scale_color_manual(values = c("2024 spring" = "#006600",  # 深綠
                                "2024 fall"   = "#867052")) +  # 深咖啡
  
  annotate("text", x = 0.02, y = -1.6,
           label = paste0("r = ",
                          round(all_raw_aphid_abundance_7_day_mean_soil_conductivity_correlation_coefficient, 3),
                          ", p = ",
                          signif(all_raw_aphid_abundance_7_day_mean_soil_conductivity_p_value, 3)),
           hjust = 0, vjust = 0, size = 2.5, color = "black")  +
  scale_x_continuous(limits = c(0, 1.2)) +
  scale_y_continuous(limits = c(-2.0, 4.0))


max(df_new$five_day_mean_soil_conductivity)
# spring

#daily mean soil EC
all_raw_aphid_abundance_7_day_mean_soil_conductivity_spring_correlation_test <- cor.test(df_new_spring$aphid_abundance_log,
                                                                                         df_new_spring$seven_day_mean_soil_conductivity,
                                                                                         use = "complete.obs")
# Extract the correlation coefficient and p-value
all_raw_aphid_abundance_7_day_mean_soil_conductivity_spring_correlation_coefficient <- all_raw_aphid_abundance_7_day_mean_soil_conductivity_spring_correlation_test$estimate
all_raw_aphid_abundance_7_day_mean_soil_conductivity_spring_p_value <- all_raw_aphid_abundance_7_day_mean_soil_conductivity_spring_correlation_test$p.value


#daily mean soil moisture
all_raw_aphid_abundance_7_day_mean_soil_conductivity_fall_correlation_test <- cor.test(df_new_fall$aphid_abundance_log,
                                                                                       df_new_fall$seven_day_mean_soil_conductivity,
                                                                                       use = "complete.obs")
# Extract the correlation coefficient and p-value
all_raw_aphid_abundance_7_day_mean_soil_conductivity_fall_correlation_coefficient <- all_raw_aphid_abundance_7_day_mean_soil_conductivity_fall_correlation_test$estimate
all_raw_aphid_abundance_7_day_mean_soil_conductivity_fall_p_value <- all_raw_aphid_abundance_7_day_mean_soil_conductivity_fall_correlation_test$p.value



# Scatter plot with a linear trend line
plot_all_raw_7_day_mean_soil_conductivity_aphid_abundance_season <- 
  ggplot(df_new,
         aes(x = seven_day_mean_soil_conductivity,
             y = aphid_abundance_log,
             color = `growing season`)) + 
  geom_point(aes(fill = after_scale(scales::alpha(color, 0.30))),
             stroke = 0.2, shape = 21, size = 0.75) + 
  # 設定回歸線顏色，根據不同的 'growing season' 來顯示不同顏色
  geom_smooth(data = df_new %>% filter(`growing season` == "2024 spring"), method = "lm", se = TRUE,
              color = "#006600",  # 深綠色回歸線
              fill = "#66CC66",   # 淺綠色信賴區間
              alpha = 0.4, size = 0.5) +   # 信賴區間透明度
  geom_smooth(data = df_new %>% filter(`growing season` == "2024 fall"), method = "lm", se = TRUE,
              color = "#8B4513",  # 紅棕色回歸線
              fill = "#C4B7A6",   # 淺咖啡信賴區間
              alpha = 0.4, size = 0.5) +   # 信賴區間透明度
  labs(title = "",
       x = "Prev. 7-D EC",
       y = expression( ~ log[10](Aphids + 1)/m^2 )) +
  theme_classic(base_size = 8, base_family = "sans") +
  theme(plot.title=element_text(size=8, face="bold",hjust = 0.5),
        axis.title=element_text(size=8),
        axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x = element_text(size =6),
        axis.text.y = element_text(size = 6),
        legend.position= "none",        # 顯示圖例
        legend.key.size = unit(12,"points"),
        legend.title = element_blank()) +
  coord_cartesian(clip = "off") + # 淺咖啡
  # 資料點顏色設置，確保資料點顏色正確
  scale_color_manual(values = c("2024 spring" = "#006600",  # 深綠
                                "2024 fall"   = "#867052")) +  # 深咖啡
  # 區間顏色設置，確保資料的顏色不會被覆蓋
  scale_fill_manual(values = c("2024 spring" = "#66CC66",  # 淺綠
                               "2024 fall"   = "#C4B7A6")) +  # 淺咖啡
  annotate("text", x = 0.02, y = -1.4,
           label = paste0("r = ",
                          round(all_raw_aphid_abundance_7_day_mean_soil_conductivity_spring_correlation_coefficient, 3),
                          ", p = ",
                          signif(all_raw_aphid_abundance_7_day_mean_soil_conductivity_spring_p_value, 3)),
           hjust = 0, vjust = 0, size = 2.5, color = "#006600") +
  annotate("text", x = 0.02, y = -1.8,
           label = paste0("r = ",
                          round(all_raw_aphid_abundance_7_day_mean_soil_conductivity_fall_correlation_coefficient, 3),
                          ", p < 0.05 "),
           hjust = 0, vjust = 0, size = 2.5, color = "#8B4513", fontface = "bold") +
  scale_x_continuous(limits = c(0, 1.2)) +
  scale_y_continuous(limits = c(-2.0, 4.0))

#-----------------------------------------------------------------------------------------------------------------------
# WAP
#daily mean soil moisture
all_WAP_aphid_abundance_daily_mean_soil_moisture_correlation_test <- cor.test(df_new$resid_WAP,
                                                                              df_new$resid_WAP_daily_mois,
                                                                              use = "complete.obs")
# Extract the correlation coefficient and p-value
all_WAP_aphid_abundance_daily_mean_soil_moisture_correlation_coefficient <- all_WAP_aphid_abundance_daily_mean_soil_moisture_correlation_test$estimate
all_WAP_aphid_abundance_daily_mean_soil_moisture_p_value <- all_WAP_aphid_abundance_daily_mean_soil_moisture_correlation_test$p.value

max(df_new$resid_WAP_daily_mois)
min(df_new$resid_WAP_daily_mois)

plot_all_WAP_daily_mean_soil_moisture_aphid_abundance <- 
  ggplot(df_new,
         aes(x = resid_WAP_daily_mois,
             y = resid_WAP,
             color = `growing season`)) + 
  geom_point(aes(fill = after_scale(scales::alpha(color, 0.30))),
             stroke = 0.2, shape = 21, size = 0.75) +
  geom_smooth(method = "lm", se = TRUE,col = "blue", aes(group = 1),
              alpha = 0.3, size = 0.5) +   # 信賴區間透明度
  
  labs(title = "",
       x = "Daily Mois. ~ WAP",
       y = expression( ~ log[10](Aphids + 1)/m^2 ~ "~ WAP")) +
  theme_classic(base_size = 8, base_family = "sans") +
  theme(plot.title=element_text(size=8, face="bold",hjust = 0.5),
        axis.title=element_text(size=8),
        axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x = element_text(size =6),
        axis.text.y = element_text(size = 6),
        legend.position= "none",        # 顯示圖例
        legend.key.size = unit(12,"points"),
        legend.title = element_blank()) +
  coord_cartesian(clip = "off") +
  scale_color_manual(values = c("2024 spring" = "#006600",  # 深綠
                                "2024 fall"   = "#867052")) +  # 深咖啡
  
  annotate("text", x = -15, y = -2.6,
           label = paste0("r = ",
                          round(all_WAP_aphid_abundance_daily_mean_soil_moisture_correlation_coefficient, 3),
                          ", p = ",
                          signif(all_WAP_aphid_abundance_daily_mean_soil_moisture_p_value, 3)),
           hjust = 0, vjust = 0, size = 2.5, color = "black")  +
  scale_x_continuous(limits = c(-20, 20)) +
  scale_y_continuous(limits = c(-3.0, 3.0))


#daily mean soil moisture
all_WAP_aphid_abundance_daily_mean_soil_moisture_spring_correlation_test <- cor.test(df_new_spring$resid_WAP,
                                                                                     df_new_spring$resid_WAP_daily_mois,
                                                                                     use = "complete.obs")
# Extract the correlation coefficient and p-value
all_WAP_aphid_abundance_daily_mean_soil_moisture_spring_correlation_coefficient <- all_WAP_aphid_abundance_daily_mean_soil_moisture_spring_correlation_test$estimate
all_WAP_aphid_abundance_daily_mean_soil_moisture_spring_p_value <- all_WAP_aphid_abundance_daily_mean_soil_moisture_spring_correlation_test$p.value


#daily mean soil moisture
all_WAP_aphid_abundance_daily_mean_soil_moisture_fall_correlation_test <- cor.test(df_new_fall$resid_WAP,
                                                                                   df_new_fall$resid_WAP_daily_mois,
                                                                                   use = "complete.obs")
# Extract the correlation coefficient and p-value
all_WAP_aphid_abundance_daily_mean_soil_moisture_fall_correlation_coefficient <- all_WAP_aphid_abundance_daily_mean_soil_moisture_fall_correlation_test$estimate
all_WAP_aphid_abundance_daily_mean_soil_moisture_fall_p_value <- all_WAP_aphid_abundance_daily_mean_soil_moisture_fall_correlation_test$p.value


library(scales)  # for alpha()
# scale_color_manual(values = c("2024 spring" = "#006600", "2024 fall" = "#867052"))
# Scatter plot with a linear trend line
plot_all_WAP_daily_mean_soil_moisture_aphid_abundance_season <- 
  ggplot(df_new,
         aes(x = resid_WAP_daily_mois,
             y = resid_WAP,
             color = `growing season`)) + 
  geom_point(aes(fill = after_scale(scales::alpha(color, 0.30))),
             stroke = 0.2, shape = 21, size = 0.75) + 
  # 設定回歸線顏色，根據不同的 'growing season' 來顯示不同顏色
  geom_smooth(data = df_new %>% filter(`growing season` == "2024 spring"), method = "lm", se = TRUE,
              color = "#006600",  # 深綠色回歸線
              fill = "#66CC66",   # 淺綠色信賴區間
              alpha = 0.4, size = 0.5) +   # 信賴區間透明度
  geom_smooth(data = df_new %>% filter(`growing season` == "2024 fall"), method = "lm", se = TRUE,
              color = "#8B4513",  # 紅棕色回歸線
              fill = "#C4B7A6",   # 淺咖啡信賴區間
              alpha = 0.4, size = 0.5) +   # 信賴區間透明度
  labs(title = "",
       x = "Daily Mois. ~ WAP",
       y = expression( ~ log[10](Aphids + 1)/m^2 ~ "~ WAP")) +
  theme_classic(base_size = 8, base_family = "sans") +
  theme(plot.title=element_text(size=8, face="bold",hjust = 0.5),
        axis.title=element_text(size=8),
        axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x = element_text(size =6),
        axis.text.y = element_text(size = 6),
        legend.position= "none",        # 顯示圖例
        legend.key.size = unit(12,"points"),
        legend.title = element_blank()) +
  coord_cartesian(clip = "off") +
  # 資料點顏色設置，確保資料點顏色正確
  scale_color_manual(values = c("2024 spring" = "#006600",  # 深綠
                                "2024 fall"   = "#867052")) +  # 深咖啡
  # 區間顏色設置，確保資料的顏色不會被覆蓋
  scale_fill_manual(values = c("2024 spring" = "#66CC66",  # 淺綠
                               "2024 fall"   = "#C4B7A6")) +  # 淺咖啡
  annotate("text", x = -15, y = -2.4,
           label = paste0("r = ",
                          round(all_WAP_aphid_abundance_daily_mean_soil_moisture_spring_correlation_coefficient, 3),
                          ", p = ",
                          signif(all_WAP_aphid_abundance_daily_mean_soil_moisture_spring_p_value, 3)),
           hjust = 0, vjust = 0, size = 2.5, color = "#006600") +
  annotate("text", x = -15, y = -2.8,
           label = paste0("r = ",
                          round(all_WAP_aphid_abundance_daily_mean_soil_moisture_fall_correlation_coefficient, 3),
                          ", p = ",
                          signif(all_WAP_aphid_abundance_daily_mean_soil_moisture_fall_p_value, 3)),
           hjust = 0, vjust = 0, size = 2.5, color = "#8B4513") +
  scale_x_continuous(limits = c(-20, 20)) +
  scale_y_continuous(limits = c(-3.0, 3.0))



# 1-day mean soil moisture
all_WAP_aphid_abundance_1_day_mean_soil_moisture_correlation_test <- cor.test(df_new$resid_WAP,
                                                                              df_new$resid_WAP_1_day_mois,
                                                                              use = "complete.obs")
# Extract the correlation coefficient and p-value
all_WAP_aphid_abundance_1_day_mean_soil_moisture_correlation_coefficient <- all_WAP_aphid_abundance_1_day_mean_soil_moisture_correlation_test$estimate
all_WAP_aphid_abundance_1_day_mean_soil_moisture_p_value <- all_WAP_aphid_abundance_1_day_mean_soil_moisture_correlation_test$p.value

max(df_new$resid_WAP_1_day_mois)
min(df_new$resid_WAP_1_day_mois)

plot_all_WAP_1_day_mean_soil_moisture_aphid_abundance <- 
  ggplot(df_new,
         aes(x = resid_WAP_1_day_mois,
             y = resid_WAP,
             color = `growing season`)) + 
  geom_point(aes(fill = after_scale(scales::alpha(color, 0.30))),
             stroke = 0.2, shape = 21, size = 0.75) +
  geom_smooth(method = "lm", se = TRUE,col = "blue", aes(group = 1),
              alpha = 0.3, size = 0.5) +   # 信賴區間透明度
  
  labs(title = "",
       x = "Prev. 1-D Mois. ~ WAP",
       y = expression( ~ log[10](Aphids + 1)/m^2 ~ "~ WAP" )) +
  theme_classic(base_size = 8, base_family = "sans") +
  theme(plot.title=element_text(size=8, face="bold",hjust = 0.5),
        axis.title=element_text(size=8),
        axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x = element_text(size =6),
        axis.text.y = element_text(size = 6),
        legend.position= "none",        # 顯示圖例
        legend.key.size = unit(12,"points"),
        legend.title = element_blank()) +
  coord_cartesian(clip = "off") +
  scale_color_manual(values = c("2024 spring" = "#006600",  # 深綠
                                "2024 fall"   = "#867052")) +  # 深咖啡
  
  annotate("text", x = -15, y = -2.6,
           label = paste0("r = ",
                          round(all_WAP_aphid_abundance_1_day_mean_soil_moisture_correlation_coefficient, 3),
                          ", p = ",
                          signif(all_WAP_aphid_abundance_1_day_mean_soil_moisture_p_value, 3)),
           hjust = 0, vjust = 0, size = 2.5, color = "black")  +
  scale_x_continuous(limits = c(-20, 20)) +
  scale_y_continuous(limits = c(-3.0, 3.0))


#daily mean soil moisture
all_WAP_aphid_abundance_1_day_mean_soil_moisture_spring_correlation_test <- cor.test(df_new_spring$resid_WAP,
                                                                                     df_new_spring$resid_WAP_1_day_mois,
                                                                                     use = "complete.obs")
# Extract the correlation coefficient and p-value
all_WAP_aphid_abundance_1_day_mean_soil_moisture_spring_correlation_coefficient <- all_WAP_aphid_abundance_1_day_mean_soil_moisture_spring_correlation_test$estimate
all_WAP_aphid_abundance_1_day_mean_soil_moisture_spring_p_value <- all_WAP_aphid_abundance_1_day_mean_soil_moisture_spring_correlation_test$p.value


#daily mean soil moisture
all_WAP_aphid_abundance_1_day_mean_soil_moisture_fall_correlation_test <- cor.test(df_new_fall$resid_WAP,
                                                                                   df_new_fall$resid_WAP_1_day_mois,
                                                                                   use = "complete.obs")
# Extract the correlation coefficient and p-value
all_WAP_aphid_abundance_1_day_mean_soil_moisture_fall_correlation_coefficient <- all_WAP_aphid_abundance_1_day_mean_soil_moisture_fall_correlation_test$estimate
all_WAP_aphid_abundance_1_day_mean_soil_moisture_fall_p_value <- all_WAP_aphid_abundance_1_day_mean_soil_moisture_fall_correlation_test$p.value


library(scales)  # for alpha()
# scale_color_manual(values = c("2024 spring" = "#006600", "2024 fall" = "#867052"))
# Scatter plot with a linear trend line
plot_all_WAP_1_day_mean_soil_moisture_aphid_abundance_season <- 
  ggplot(df_new,
         aes(x = resid_MAP_1_day_mois,
             y = resid_WAP,
             color = `growing season`)) + 
  geom_point(aes(fill = after_scale(scales::alpha(color, 0.30))),
             stroke = 0.2, shape = 21, size = 0.75) + 
  # 設定回歸線顏色，根據不同的 'growing season' 來顯示不同顏色
  geom_smooth(data = df_new %>% filter(`growing season` == "2024 spring"), method = "lm", se = TRUE,
              color = "#006600",  # 深綠色回歸線
              fill = "#66CC66",   # 淺綠色信賴區間
              alpha = 0.4, size = 0.5) +   # 信賴區間透明度
  geom_smooth(data = df_new %>% filter(`growing season` == "2024 fall"), method = "lm", se = TRUE,
              color = "#8B4513",  # 紅棕色回歸線
              fill = "#C4B7A6",   # 淺咖啡信賴區間
              alpha = 0.4, size = 0.5) +   # 信賴區間透明度
  labs(title = "",
       x = "Prev. 1-D Mois. ~ WAP",
       y = expression( ~ log[10](Aphids + 1)/m^2 ~ "~ WAP")) +
  theme_classic(base_size = 8, base_family = "sans") +
  theme(plot.title=element_text(size=8, face="bold",hjust = 0.5),
        axis.title=element_text(size=8),
        axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x = element_text(size =6),
        axis.text.y = element_text(size = 6),
        legend.position= "none",        # 顯示圖例
        legend.key.size = unit(12,"points"),
        legend.title = element_blank()) +
  coord_cartesian(clip = "off") +
  # 資料點顏色設置，確保資料點顏色正確
  scale_color_manual(values = c("2024 spring" = "#006600",  # 深綠
                                "2024 fall"   = "#867052")) +  # 深咖啡
  # 區間顏色設置，確保資料的顏色不會被覆蓋
  scale_fill_manual(values = c("2024 spring" = "#66CC66",  # 淺綠
                               "2024 fall"   = "#C4B7A6")) +  # 淺咖啡
  annotate("text", x = -15, y = -2.4,
           label = paste0("r = ",
                          round(all_WAP_aphid_abundance_1_day_mean_soil_moisture_spring_correlation_coefficient, 3),
                          ", p = ",
                          signif(all_WAP_aphid_abundance_1_day_mean_soil_moisture_spring_p_value, 3)),
           hjust = 0, vjust = 0, size = 2.5, color = "#006600") +
  annotate("text", x = -15, y = -2.8,
           label = paste0("r = ",
                          round(all_WAP_aphid_abundance_1_day_mean_soil_moisture_fall_correlation_coefficient, 3),
                          ", p = ",
                          signif(all_WAP_aphid_abundance_1_day_mean_soil_moisture_fall_p_value, 3)),
           hjust = 0, vjust = 0, size = 2.5, color = "#8B4513") +
  scale_x_continuous(limits = c(-20, 20)) +
  scale_y_continuous(limits = c(-3.0, 3.0))


#3-day mean soil moisture

all_WAP_aphid_abundance_3_day_mean_soil_moisture_correlation_test <- cor.test(df_new$resid_WAP,
                                                                              df_new$resid_WAP_3_day_mois,
                                                                              use = "complete.obs")
# Extract the correlation coefficient and p-value
all_WAP_aphid_abundance_3_day_mean_soil_moisture_correlation_coefficient <- all_WAP_aphid_abundance_3_day_mean_soil_moisture_correlation_test$estimate
all_WAP_aphid_abundance_3_day_mean_soil_moisture_p_value <- all_WAP_aphid_abundance_3_day_mean_soil_moisture_correlation_test$p.value

max(df_new$resid_WAP_3_day_mois)
min(df_new$resid_WAP_3_day_mois)

plot_all_WAP_3_day_mean_soil_moisture_aphid_abundance <- 
  ggplot(df_new,
         aes(x = resid_WAP_3_day_mois,
             y = resid_WAP,
             color = `growing season`)) + 
  geom_point(aes(fill = after_scale(scales::alpha(color, 0.30))),
             stroke = 0.2, shape = 21, size = 0.75) +
  geom_smooth(method = "lm", se = TRUE,col = "blue", aes(group = 1),
              alpha = 0.3, size = 0.5) +   # 信賴區間透明度
  
  labs(title = "",
       x = "Prev. 3-D Mois. ~ WAP",
       y = expression( ~ log[10](Aphids + 1)/m^2 ~ "~ WAP")) +
  theme_classic(base_size = 8, base_family = "sans") +
  theme(plot.title=element_text(size=8, face="bold",hjust = 0.5),
        axis.title=element_text(size=8),
        axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x = element_text(size =6),
        axis.text.y = element_text(size = 6),
        legend.position= "none",        # 顯示圖例
        legend.key.size = unit(12,"points"),
        legend.title = element_blank()) +
  coord_cartesian(clip = "off") +
  scale_color_manual(values = c("2024 spring" = "#006600",  # 深綠
                                "2024 fall"   = "#867052")) +  # 深咖啡
  
  annotate("text", x = -12.5, y = -2.6,
           label = paste0("r = ",
                          round(all_WAP_aphid_abundance_3_day_mean_soil_moisture_correlation_coefficient, 3),
                          ", p < 0.05 "),
           hjust = 0, vjust = 0, size = 2.5, color = "red", fontface = "bold")  +
  scale_x_continuous(limits = c(-15, 15)) +
  scale_y_continuous(limits = c(-3.0, 3.0))


#daily mean soil moisture
all_WAP_aphid_abundance_3_day_mean_soil_moisture_spring_correlation_test <- cor.test(df_new_spring$resid_WAP,
                                                                                     df_new_spring$resid_WAP_3_day_mois,
                                                                                     use = "complete.obs")
# Extract the correlation coefficient and p-value
all_WAP_aphid_abundance_3_day_mean_soil_moisture_spring_correlation_coefficient <- all_WAP_aphid_abundance_3_day_mean_soil_moisture_spring_correlation_test$estimate
all_WAP_aphid_abundance_3_day_mean_soil_moisture_spring_p_value <- all_WAP_aphid_abundance_3_day_mean_soil_moisture_spring_correlation_test$p.value


#daily mean soil moisture
all_WAP_aphid_abundance_3_day_mean_soil_moisture_fall_correlation_test <- cor.test(df_new_fall$resid_WAP,
                                                                                   df_new_fall$resid_MAP_3_day_mois,
                                                                                   use = "complete.obs")
# Extract the correlation coefficient and p-value
all_WAP_aphid_abundance_3_day_mean_soil_moisture_fall_correlation_coefficient <- all_WAP_aphid_abundance_3_day_mean_soil_moisture_fall_correlation_test$estimate
all_WAP_aphid_abundance_3_day_mean_soil_moisture_fall_p_value <- all_WAP_aphid_abundance_3_day_mean_soil_moisture_fall_correlation_test$p.value


library(scales)  # for alpha()
# scale_color_manual(values = c("2024 spring" = "#006600", "2024 fall" = "#867052"))
# Scatter plot with a linear trend line
plot_all_WAP_3_day_mean_soil_moisture_aphid_abundance_season <- 
  ggplot(df_new,
         aes(x = resid_WAP_3_day_mois,
             y = resid_WAP,
             color = `growing season`)) + 
  geom_point(aes(fill = after_scale(scales::alpha(color, 0.30))),
             stroke = 0.2, shape = 21, size = 0.75) + 
  # 設定回歸線顏色，根據不同的 'growing season' 來顯示不同顏色
  geom_smooth(data = df_new %>% filter(`growing season` == "2024 spring"), method = "lm", se = TRUE,
              color = "#006600",  # 深綠色回歸線
              fill = "#66CC66",   # 淺綠色信賴區間
              alpha = 0.4, size = 0.5) +   # 信賴區間透明度
  geom_smooth(data = df_new %>% filter(`growing season` == "2024 fall"), method = "lm", se = TRUE,
              color = "#8B4513",  # 紅棕色回歸線
              fill = "#C4B7A6",   # 淺咖啡信賴區間
              alpha = 0.4, size = 0.5) +   # 信賴區間透明度
  labs(title = "",
       x = "Prev. 3-D Mois. ~ WAP",
       y = expression( ~ log[10](Aphids + 1)/m^2 ~ "~ WAP")) +
  theme_classic(base_size = 8, base_family = "sans") +
  theme(plot.title=element_text(size=8, face="bold",hjust = 0.5),
        axis.title=element_text(size=8),
        axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x = element_text(size =6),
        axis.text.y = element_text(size = 6),
        legend.position= "none",        # 顯示圖例
        legend.key.size = unit(12,"points"),
        legend.title = element_blank()) +
  coord_cartesian(clip = "off") +
  # 資料點顏色設置，確保資料點顏色正確
  scale_color_manual(values = c("2024 spring" = "#006600",  # 深綠
                                "2024 fall"   = "#867052")) +  # 深咖啡
  # 區間顏色設置，確保資料的顏色不會被覆蓋
  scale_fill_manual(values = c("2024 spring" = "#66CC66",  # 淺綠
                               "2024 fall"   = "#C4B7A6")) +  # 淺咖啡
  annotate("text", x = -12.5, y = -2.4,
           label = paste0("r = ",
                          round(all_WAP_aphid_abundance_3_day_mean_soil_moisture_spring_correlation_coefficient, 3),
                          ", p = ",
                          signif(all_WAP_aphid_abundance_3_day_mean_soil_moisture_spring_p_value, 3)),
           hjust = 0, vjust = 0, size = 2.5, color = "#006600") +
  annotate("text", x = -12.5, y = -2.8,
           label = paste0("r = ",
                          round(all_WAP_aphid_abundance_3_day_mean_soil_moisture_fall_correlation_coefficient, 3),
                          ", p = ",
                          signif(all_WAP_aphid_abundance_3_day_mean_soil_moisture_fall_p_value, 3)),
           hjust = 0, vjust = 0, size = 2.5, color = "#8B4513") +
  scale_x_continuous(limits = c(-15, 15)) +
  scale_y_continuous(limits = c(-3.0, 3.0))


#5-day mean soil moisture

all_WAP_aphid_abundance_5_day_mean_soil_moisture_correlation_test <- cor.test(df_new$resid_WAP,
                                                                              df_new$resid_WAP_5_day_mois,
                                                                              use = "complete.obs")
# Extract the correlation coefficient and p-value
all_WAP_aphid_abundance_5_day_mean_soil_moisture_correlation_coefficient <- all_WAP_aphid_abundance_5_day_mean_soil_moisture_correlation_test$estimate
all_WAP_aphid_abundance_5_day_mean_soil_moisture_p_value <- all_WAP_aphid_abundance_5_day_mean_soil_moisture_correlation_test$p.value

max(df_new$resid_WAP_5_day_mois)
min(df_new$resid_WAP_5_day_mois)


plot_all_WAP_5_day_mean_soil_moisture_aphid_abundance <- 
  ggplot(df_new,
         aes(x = resid_WAP_5_day_mois,
             y = resid_WAP,
             color = `growing season`)) + 
  geom_point(aes(fill = after_scale(scales::alpha(color, 0.30))),
             stroke = 0.2, shape = 21, size = 0.75) +
  geom_smooth(method = "lm", se = TRUE,col = "blue", aes(group = 1),
              alpha = 0.3, size = 0.5) +   # 信賴區間透明度
  
  labs(title = "",
       x = "Prev. 5-D Mois. ~ WAP",
       y = expression( ~ log[10](Aphids + 1)/m^2 ~ "~ WAP")) +
  theme_classic(base_size = 8, base_family = "sans") +
  theme(plot.title=element_text(size=8, face="bold",hjust = 0.5),
        axis.title=element_text(size=8),
        axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x = element_text(size =6),
        axis.text.y = element_text(size = 6),
        legend.position= "none",        # 顯示圖例
        legend.key.size = unit(12,"points"),
        legend.title = element_blank()) +
  coord_cartesian(clip = "off") +
  scale_color_manual(values = c("2024 spring" = "#006600",  # 深綠
                                "2024 fall"   = "#867052")) +  # 深咖啡
  
  annotate("text", x = -12.5, y = -2.6,
           label = paste0("r = ",
                          round(all_WAP_aphid_abundance_5_day_mean_soil_moisture_correlation_coefficient, 3),
                          ", p < 0.001 "),
           hjust = 0, vjust = 0, size = 2.5, color = "red", fontface = "bold")  +
  scale_x_continuous(limits = c(-15, 15)) +
  scale_y_continuous(limits = c(-3.0, 3.0))


#daily mean soil moisture
all_WAP_aphid_abundance_5_day_mean_soil_moisture_spring_correlation_test <- cor.test(df_new_spring$resid_WAP,
                                                                                     df_new_spring$resid_WAP_5_day_mois,
                                                                                     use = "complete.obs")
# Extract the correlation coefficient and p-value
all_WAP_aphid_abundance_5_day_mean_soil_moisture_spring_correlation_coefficient <- all_WAP_aphid_abundance_5_day_mean_soil_moisture_spring_correlation_test$estimate
all_WAP_aphid_abundance_5_day_mean_soil_moisture_spring_p_value <- all_WAP_aphid_abundance_5_day_mean_soil_moisture_spring_correlation_test$p.value


#daily mean soil moisture
all_WAP_aphid_abundance_5_day_mean_soil_moisture_fall_correlation_test <- cor.test(df_new_fall$resid_WAP,
                                                                                   df_new_fall$resid_WAP_5_day_mois,
                                                                                   use = "complete.obs")
# Extract the correlation coefficient and p-value
all_WAP_aphid_abundance_5_day_mean_soil_moisture_fall_correlation_coefficient <- all_WAP_aphid_abundance_5_day_mean_soil_moisture_fall_correlation_test$estimate
all_WAP_aphid_abundance_5_day_mean_soil_moisture_fall_p_value <- all_WAP_aphid_abundance_5_day_mean_soil_moisture_fall_correlation_test$p.value


library(scales)  # for alpha()
# scale_color_manual(values = c("2024 spring" = "#006600", "2024 fall" = "#867052"))
# Scatter plot with a linear trend line
plot_all_WAP_5_day_mean_soil_moisture_aphid_abundance_season <- 
  ggplot(df_new,
         aes(x = resid_WAP_5_day_mois,
             y = resid_WAP,
             color = `growing season`)) + 
  geom_point(aes(fill = after_scale(scales::alpha(color, 0.30))),
             stroke = 0.2, shape = 21, size = 0.75) + 
  # 設定回歸線顏色，根據不同的 'growing season' 來顯示不同顏色
  geom_smooth(data = df_new %>% filter(`growing season` == "2024 spring"), method = "lm", se = TRUE,
              color = "#006600",  # 深綠色回歸線
              fill = "#66CC66",   # 淺綠色信賴區間
              alpha = 0.4, size = 0.5) +   # 信賴區間透明度
  geom_smooth(data = df_new %>% filter(`growing season` == "2024 fall"), method = "lm", se = TRUE,
              color = "#8B4513",  # 紅棕色回歸線
              fill = "#C4B7A6",   # 淺咖啡信賴區間
              alpha = 0.4, size = 0.5) +   # 信賴區間透明度
  labs(title = "",
       x = "Prev. 5-D Mois. ~ WAP",
       y = expression( ~ log[10](Aphids + 1)/m^2 ~ "~ WAP")) +
  theme_classic(base_size = 8, base_family = "sans") +
  theme(plot.title=element_text(size=8, face="bold",hjust = 0.5),
        axis.title=element_text(size=8),
        axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x = element_text(size =6),
        axis.text.y = element_text(size = 6),
        legend.position= "none",        # 顯示圖例
        legend.key.size = unit(12,"points"),
        legend.title = element_blank()) +
  coord_cartesian(clip = "off") +
  # 資料點顏色設置，確保資料點顏色正確
  scale_color_manual(values = c("2024 spring" = "#006600",  # 深綠
                                "2024 fall"   = "#867052")) +  # 深咖啡
  # 區間顏色設置，確保資料的顏色不會被覆蓋
  scale_fill_manual(values = c("2024 spring" = "#66CC66",  # 淺綠
                               "2024 fall"   = "#C4B7A6")) +  # 淺咖啡
  annotate("text", x = -12.5, y = -2.4,
           label = paste0("r = ",
                          round(all_WAP_aphid_abundance_5_day_mean_soil_moisture_spring_correlation_coefficient, 3),
                          ", p < 0.05 "),
           hjust = 0, vjust = 0, size = 2.5, color = "#006600", fontface = "bold") +
  annotate("text", x = -12.5, y = -2.8,
           label = paste0("r = ",
                          round(all_WAP_aphid_abundance_5_day_mean_soil_moisture_fall_correlation_coefficient, 3),
                          ", p < 0.05 "),
           hjust = 0, vjust = 0, size = 2.5, color = "#8B4513", fontface = "bold") +
  scale_x_continuous(limits = c(-15, 15)) +
  scale_y_continuous(limits = c(-3.0, 3.0))

# 7-day mean soil moisture

all_WAP_aphid_abundance_7_day_mean_soil_moisture_correlation_test <- cor.test(df_new$resid_WAP,
                                                                              df_new$resid_WAP_7_day_mois,
                                                                              use = "complete.obs")
# Extract the correlation coefficient and p-value
all_WAP_aphid_abundance_7_day_mean_soil_moisture_correlation_coefficient <- all_WAP_aphid_abundance_7_day_mean_soil_moisture_correlation_test$estimate
all_WAP_aphid_abundance_7_day_mean_soil_moisture_p_value <- all_WAP_aphid_abundance_7_day_mean_soil_moisture_correlation_test$p.value

max(df_new$resid_WAP_7_day_mois)
min(df_new$resid_WAP_7_day_mois)

plot_all_WAP_7_day_mean_soil_moisture_aphid_abundance <- 
  ggplot(df_new,
         aes(x = resid_WAP_7_day_mois,
             y = resid_WAP,
             color = `growing season`)) + 
  geom_point(aes(fill = after_scale(scales::alpha(color, 0.30))),
             stroke = 0.2, shape = 21, size = 0.75) +
  geom_smooth(method = "lm", se = TRUE,col = "blue", aes(group = 1),
              alpha = 0.3, size = 0.5) +   # 信賴區間透明度
  
  labs(title = "",
       x = "Prev. 7-D Mois. ~ WAP",
       y = expression( ~ log[10](Aphids + 1)/m^2 ~ "~ WAP")) +
  theme_classic(base_size = 8, base_family = "sans") +
  theme(plot.title=element_text(size=8, face="bold",hjust = 0.5),
        axis.title=element_text(size=8),
        axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x = element_text(size =6),
        axis.text.y = element_text(size = 6),
        legend.position= "none",        # 顯示圖例
        legend.key.size = unit(12,"points"),
        legend.title = element_blank()) +
  coord_cartesian(clip = "off") +
  scale_color_manual(values = c("2024 spring" = "#006600",  # 深綠
                                "2024 fall"   = "#867052")) +  # 深咖啡
  
  annotate("text", x = -12.5, y = -2.6,
           label = paste0("r = ",
                          round(all_WAP_aphid_abundance_7_day_mean_soil_moisture_correlation_coefficient, 3),
                          ", p < 0.001 "),
           hjust = 0, vjust = 0, size = 2.5, color = "red", fontface = "bold")  +
  scale_x_continuous(limits = c(-15, 15)) +
  scale_y_continuous(limits = c(-3.0, 3.0))


#daily mean soil moisture
all_WAP_aphid_abundance_7_day_mean_soil_moisture_spring_correlation_test <- cor.test(df_new_spring$resid_WAP,
                                                                                     df_new_spring$resid_WAP_7_day_mois,
                                                                                     use = "complete.obs")
# Extract the correlation coefficient and p-value
all_WAP_aphid_abundance_7_day_mean_soil_moisture_spring_correlation_coefficient <- all_WAP_aphid_abundance_7_day_mean_soil_moisture_spring_correlation_test$estimate
all_WAP_aphid_abundance_7_day_mean_soil_moisture_spring_p_value <- all_WAP_aphid_abundance_7_day_mean_soil_moisture_spring_correlation_test$p.value


#daily mean soil moisture
all_WAP_aphid_abundance_7_day_mean_soil_moisture_fall_correlation_test <- cor.test(df_new_fall$resid_WAP,
                                                                                   df_new_fall$resid_WAP_7_day_mois,
                                                                                   use = "complete.obs")
# Extract the correlation coefficient and p-value
all_WAP_aphid_abundance_7_day_mean_soil_moisture_fall_correlation_coefficient <- all_WAP_aphid_abundance_7_day_mean_soil_moisture_fall_correlation_test$estimate
all_WAP_aphid_abundance_7_day_mean_soil_moisture_fall_p_value <- all_WAP_aphid_abundance_7_day_mean_soil_moisture_fall_correlation_test$p.value


library(scales)  # for alpha()
# scale_color_manual(values = c("2024 spring" = "#006600", "2024 fall" = "#867052"))
# Scatter plot with a linear trend line
plot_all_WAP_7_day_mean_soil_moisture_aphid_abundance_season <- 
  ggplot(df_new,
         aes(x = resid_WAP_7_day_mois,
             y = resid_WAP,
             color = `growing season`)) + 
  geom_point(aes(fill = after_scale(scales::alpha(color, 0.30))),
             stroke = 0.2, shape = 21, size = 0.75) + 
  # 設定回歸線顏色，根據不同的 'growing season' 來顯示不同顏色
  geom_smooth(data = df_new %>% filter(`growing season` == "2024 spring"), method = "lm", se = TRUE,
              color = "#006600",  # 深綠色回歸線
              fill = "#66CC66",   # 淺綠色信賴區間
              alpha = 0.4, size = 0.5) +   # 信賴區間透明度
  geom_smooth(data = df_new %>% filter(`growing season` == "2024 fall"), method = "lm", se = TRUE,
              color = "#8B4513",  # 紅棕色回歸線
              fill = "#C4B7A6",   # 淺咖啡信賴區間
              alpha = 0.4, size = 0.5) +   # 信賴區間透明度
  labs(title = "",
       x = "Prev. 7-D Mois. ~ WAP",
       y = expression( ~ log[10](Aphids + 1)/m^2 ~ "~ WAP")) +
  theme_classic(base_size = 8, base_family = "sans") +
  theme(plot.title=element_text(size=8, face="bold",hjust = 0.5),
        axis.title=element_text(size=8),
        axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x = element_text(size =6),
        axis.text.y = element_text(size = 6),
        legend.position= "none",        # 顯示圖例
        legend.key.size = unit(12,"points"),
        legend.title = element_blank()) +
  coord_cartesian(clip = "off") +
  # 資料點顏色設置，確保資料點顏色正確
  scale_color_manual(values = c("2024 spring" = "#006600",  # 深綠
                                "2024 fall"   = "#867052")) +  # 深咖啡
  # 區間顏色設置，確保資料的顏色不會被覆蓋
  scale_fill_manual(values = c("2024 spring" = "#66CC66",  # 淺綠
                               "2024 fall"   = "#C4B7A6")) +  # 淺咖啡
  annotate("text", x = -12.5, y = -2.4,
           label = paste0("r = ",
                          round(all_WAP_aphid_abundance_7_day_mean_soil_moisture_spring_correlation_coefficient, 3),
                          ", p < 0.05 "),
           hjust = 0, vjust = 0, size = 2.5, color = "#006600", fontface = "bold") +
  annotate("text", x = -12.5, y = -2.8,
           label = paste0("r = ",
                          round(all_WAP_aphid_abundance_7_day_mean_soil_moisture_fall_correlation_coefficient, 3),
                          ", p < 0.05 "),
           hjust = 0, vjust = 0, size = 2.5, color = "#8B4513", fontface = "bold") +
  scale_x_continuous(limits = c(-15, 15)) +
  scale_y_continuous(limits = c(-3.0, 3.0))

#daily mean soil EC
all_WAP_aphid_abundance_daily_mean_soil_conductivity_correlation_test <- cor.test(df_new$resid_WAP,
                                                                                  df_new$resid_WAP_daily_ec,
                                                                                  use = "complete.obs")
# Extract the correlation coefficient and p-value
all_WAP_aphid_abundance_daily_mean_soil_conductivity_correlation_coefficient <- all_WAP_aphid_abundance_daily_mean_soil_conductivity_correlation_test$estimate
all_WAP_aphid_abundance_daily_mean_soil_conductivity_p_value <- all_WAP_aphid_abundance_daily_mean_soil_conductivity_correlation_test$p.value

max(df_new$resid_WAP_daily_ec)
min(df_new$resid_WAP_daily_ec)

plot_all_WAP_daily_mean_soil_conductivity_aphid_abundance <- 
  ggplot(df_new,
         aes(x = resid_WAP_daily_ec,
             y = resid_WAP,
             color = `growing season`)) + 
  geom_point(aes(fill = after_scale(scales::alpha(color, 0.30))),
             stroke = 0.2, shape = 21, size = 0.75) +
  geom_smooth(method = "lm", se = TRUE,col = "blue", aes(group = 1),
              alpha = 0.3, size = 0.5) +   # 信賴區間透明度
  
  labs(title = "",
       x = "Daily EC ~ WAP",
       y = expression( ~ log[10](Aphids + 1)/m^2 ~ "~ WAP" )) +
  theme_classic(base_size = 8, base_family = "sans") +
  theme(plot.title=element_text(size=8, face="bold",hjust = 0.5),
        axis.title=element_text(size=8),
        axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x = element_text(size =6),
        axis.text.y = element_text(size = 6),
        legend.position= "none",        # 顯示圖例
        legend.key.size = unit(12,"points"),
        legend.title = element_blank()) +
  coord_cartesian(clip = "off") +
  scale_color_manual(values = c("2024 spring" = "#006600",  # 深綠
                                "2024 fall"   = "#867052")) +  # 深咖啡
  
  annotate("text", x = -0.1, y = -2.6,
           label = paste0("r = ",
                          round(all_WAP_aphid_abundance_daily_mean_soil_conductivity_correlation_coefficient, 3),
                          ", p = ",
                          signif(all_WAP_aphid_abundance_daily_mean_soil_conductivity_p_value, 3)),
           hjust = 0, vjust = 0, size = 2.5, color = "black")  +
  scale_x_continuous(limits = c(-0.2, 1.0)) +
  scale_y_continuous(limits = c(-3.0, 3.0))

#daily mean soil EC
all_WAP_aphid_abundance_daily_mean_soil_conductivity_spring_correlation_test <- cor.test(df_new_spring$resid_WAP,
                                                                                         df_new_spring$resid_WAP_daily_ec,
                                                                                         use = "complete.obs")
# Extract the correlation coefficient and p-value
all_WAP_aphid_abundance_daily_mean_soil_conductivity_spring_correlation_coefficient <- all_WAP_aphid_abundance_daily_mean_soil_conductivity_spring_correlation_test$estimate
all_WAP_aphid_abundance_daily_mean_soil_conductivity_spring_p_value <- all_WAP_aphid_abundance_daily_mean_soil_conductivity_spring_correlation_test$p.value


#daily mean soil moisture
all_WAP_aphid_abundance_daily_mean_soil_conductivity_fall_correlation_test <- cor.test(df_new_fall$resid_WAP,
                                                                                       df_new_fall$resid_WAP_daily_ec,
                                                                                       use = "complete.obs")
# Extract the correlation coefficient and p-value
all_WAP_aphid_abundance_daily_mean_soil_conductivity_fall_correlation_coefficient <- all_WAP_aphid_abundance_daily_mean_soil_conductivity_fall_correlation_test$estimate
all_WAP_aphid_abundance_daily_mean_soil_conductivity_fall_p_value <- all_WAP_aphid_abundance_daily_mean_soil_conductivity_fall_correlation_test$p.value



# Scatter plot with a linear trend line
plot_all_WAP_daily_mean_soil_conductivity_aphid_abundance_season <- 
  ggplot(df_new,
         aes(x = resid_WAP_daily_ec,
             y = resid_WAP,
             color = `growing season`)) + 
  geom_point(aes(fill = after_scale(scales::alpha(color, 0.30))),
             stroke = 0.2, shape = 21, size = 0.75) + 
  # 設定回歸線顏色，根據不同的 'growing season' 來顯示不同顏色
  geom_smooth(data = df_new %>% filter(`growing season` == "2024 spring"), method = "lm", se = TRUE,
              color = "#006600",  # 深綠色回歸線
              fill = "#66CC66",   # 淺綠色信賴區間
              alpha = 0.4, size = 0.5) +   # 信賴區間透明度
  geom_smooth(data = df_new %>% filter(`growing season` == "2024 fall"), method = "lm", se = TRUE,
              color = "#8B4513",  # 紅棕色回歸線
              fill = "#C4B7A6",   # 淺咖啡信賴區間
              alpha = 0.4, size = 0.5) +   # 信賴區間透明度
  labs(title = "",
       x = "Daily EC ~ WAP",
       y = expression( ~ log[10](Aphids + 1)/m^2 ~ "~ WAP")) +
  theme_classic(base_size = 8, base_family = "sans") +
  theme(plot.title=element_text(size=8, face="bold",hjust = 0.5),
        axis.title=element_text(size=8),
        axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x = element_text(size =6),
        axis.text.y = element_text(size = 6),
        legend.position= "none",        # 顯示圖例
        legend.key.size = unit(12,"points"),
        legend.title = element_blank()) +
  coord_cartesian(clip = "off") +
  # 資料點顏色設置，確保資料點顏色正確
  scale_color_manual(values = c("2024 spring" = "#006600",  # 深綠
                                "2024 fall"   = "#867052")) +  # 深咖啡
  # 區間顏色設置，確保資料的顏色不會被覆蓋
  scale_fill_manual(values = c("2024 spring" = "#66CC66",  # 淺綠
                               "2024 fall"   = "#C4B7A6")) +  # 淺咖啡
  annotate("text", x = -0.1, y = -2.4,
           label = paste0("r = ",
                          round(all_WAP_aphid_abundance_daily_mean_soil_conductivity_spring_correlation_coefficient, 3),
                          ", p = ",
                          signif(all_WAP_aphid_abundance_daily_mean_soil_conductivity_spring_p_value, 3)),
           hjust = 0, vjust = 0, size = 2.5, color = "#006600") +
  annotate("text", x = -0.1, y = -2.8,
           label = paste0("r = ",
                          round(all_WAP_aphid_abundance_daily_mean_soil_conductivity_fall_correlation_coefficient, 3),
                          ", p = ",
                          signif(all_WAP_aphid_abundance_daily_mean_soil_conductivity_fall_p_value, 3)),
           hjust = 0, vjust = 0, size = 2.5, color = "#8B4513") +
  scale_x_continuous(limits = c(-0.2, 1.0)) +
  scale_y_continuous(limits = c(-3.0, 3.0))



# 1-day mean soil conductivity

all_WAP_aphid_abundance_1_day_mean_soil_conductivity_correlation_test <- cor.test(df_new$resid_WAP,
                                                                                  df_new$resid_WAP_1_day_ec,
                                                                                  use = "complete.obs")
# Extract the correlation coefficient and p-value
all_WAP_aphid_abundance_1_day_mean_soil_conductivity_correlation_coefficient <- all_WAP_aphid_abundance_1_day_mean_soil_conductivity_correlation_test$estimate
all_WAP_aphid_abundance_1_day_mean_soil_conductivity_p_value <- all_WAP_aphid_abundance_1_day_mean_soil_conductivity_correlation_test$p.value

max(df_new$resid_WAP_1_day_ec)
min(df_new$resid_WAP_1_day_ec)

plot_all_WAP_1_day_mean_soil_conductivity_aphid_abundance <- 
  ggplot(df_new,
         aes(x = resid_WAP_1_day_ec,
             y = resid_WAP,
             color = `growing season`)) + 
  geom_point(aes(fill = after_scale(scales::alpha(color, 0.30))),
             stroke = 0.2, shape = 21, size = 0.75) +
  geom_smooth(method = "lm", se = TRUE,col = "blue", aes(group = 1),
              alpha = 0.3, size = 0.5) +   # 信賴區間透明度
  
  labs(title = "",
       x = "Prev. 1-D EC ~ WAP",
       y = expression( ~ log[10](Aphids + 1)/m^2  ~ "~ WAP")) +
  theme_classic(base_size = 8, base_family = "sans") +
  theme(plot.title=element_text(size=8, face="bold",hjust = 0.5),
        axis.title=element_text(size=8),
        axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x = element_text(size =6),
        axis.text.y = element_text(size = 6),
        legend.position= "none",        # 顯示圖例
        legend.key.size = unit(12,"points"),
        legend.title = element_blank()) +
  coord_cartesian(clip = "off") +
  scale_color_manual(values = c("2024 spring" = "#006600",  # 深綠
                                "2024 fall"   = "#867052")) +  # 深咖啡
  
  annotate("text", x = -0.05, y = -2.6,
           label = paste0("r = ",
                          round(all_WAP_aphid_abundance_1_day_mean_soil_conductivity_correlation_coefficient, 3),
                          ", p = ",
                          signif(all_WAP_aphid_abundance_1_day_mean_soil_conductivity_p_value, 3)),
           hjust = 0, vjust = 0, size = 2.5, color = "black")  +
  scale_x_continuous(limits = c(-0.15, 0.6)) +
  scale_y_continuous(limits = c(-3.0, 3.0))

#daily mean soil EC
all_WAP_aphid_abundance_1_day_mean_soil_conductivity_spring_correlation_test <- cor.test(df_new_spring$resid_WAP,
                                                                                         df_new_spring$resid_WAP_1_day_ec,
                                                                                         use = "complete.obs")
# Extract the correlation coefficient and p-value
all_WAP_aphid_abundance_1_day_mean_soil_conductivity_spring_correlation_coefficient <- all_WAP_aphid_abundance_1_day_mean_soil_conductivity_spring_correlation_test$estimate
all_WAP_aphid_abundance_1_day_mean_soil_conductivity_spring_p_value <- all_WAP_aphid_abundance_1_day_mean_soil_conductivity_spring_correlation_test$p.value


#daily mean soil moisture
all_WAP_aphid_abundance_1_day_mean_soil_conductivity_fall_correlation_test <- cor.test(df_new_fall$resid_WAP,
                                                                                       df_new_fall$resid_WAP_1_day_ec,
                                                                                       use = "complete.obs")
# Extract the correlation coefficient and p-value
all_WAP_aphid_abundance_1_day_mean_soil_conductivity_fall_correlation_coefficient <- all_WAP_aphid_abundance_1_day_mean_soil_conductivity_fall_correlation_test$estimate
all_WAP_aphid_abundance_1_day_mean_soil_conductivity_fall_p_value <- all_WAP_aphid_abundance_1_day_mean_soil_conductivity_fall_correlation_test$p.value



# Scatter plot with a linear trend line
plot_all_WAP_1_day_mean_soil_conductivity_aphid_abundance_season <- 
  ggplot(df_new,
         aes(x = resid_WAP_1_day_ec,
             y = resid_WAP,
             color = `growing season`)) + 
  geom_point(aes(fill = after_scale(scales::alpha(color, 0.30))),
             stroke = 0.2, shape = 21, size = 0.75) + 
  # 設定回歸線顏色，根據不同的 'growing season' 來顯示不同顏色
  geom_smooth(data = df_new %>% filter(`growing season` == "2024 spring"), method = "lm", se = TRUE,
              color = "#006600",  # 深綠色回歸線
              fill = "#66CC66",   # 淺綠色信賴區間
              alpha = 0.4, size = 0.5) +   # 信賴區間透明度
  geom_smooth(data = df_new %>% filter(`growing season` == "2024 fall"), method = "lm", se = TRUE,
              color = "#8B4513",  # 紅棕色回歸線
              fill = "#C4B7A6",   # 淺咖啡信賴區間
              alpha = 0.4, size = 0.5) +   # 信賴區間透明度
  labs(title = "",
       x = "Prev. 1-D EC ~ WAP",
       y = expression( ~ log[10](Aphids + 1)/m^2 ~ "~ WAP")) +
  theme_classic(base_size = 8, base_family = "sans") +
  theme(plot.title=element_text(size=8, face="bold",hjust = 0.5),
        axis.title=element_text(size=8),
        axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x = element_text(size =6),
        axis.text.y = element_text(size = 6),
        legend.position= "none",        # 顯示圖例
        legend.key.size = unit(12,"points"),
        legend.title = element_blank()) +
  coord_cartesian(clip = "off") +
  # 資料點顏色設置，確保資料點顏色正確
  scale_color_manual(values = c("2024 spring" = "#006600",  # 深綠
                                "2024 fall"   = "#867052")) +  # 深咖啡
  # 區間顏色設置，確保資料的顏色不會被覆蓋
  scale_fill_manual(values = c("2024 spring" = "#66CC66",  # 淺綠
                               "2024 fall"   = "#C4B7A6")) +  # 淺咖啡
  annotate("text", x = -0.05, y = -2.4,
           label = paste0("r = ",
                          round(all_WAP_aphid_abundance_1_day_mean_soil_conductivity_spring_correlation_coefficient, 3),
                          ", p = ",
                          signif(all_WAP_aphid_abundance_1_day_mean_soil_conductivity_spring_p_value, 3)),
           hjust = 0, vjust = 0, size = 2.5, color = "#006600") +
  annotate("text", x = -0.05, y = -2.8,
           label = paste0("r = ",
                          round(all_WAP_aphid_abundance_1_day_mean_soil_conductivity_fall_correlation_coefficient, 3),
                          ", p = ",
                          signif(all_WAP_aphid_abundance_1_day_mean_soil_conductivity_fall_p_value, 3)),
           hjust = 0, vjust = 0, size = 2.5, color = "#867052") +
  scale_x_continuous(limits = c(-0.15, 0.6)) +
  scale_y_continuous(limits = c(-3.0, 3.0))

# 3-day mean soil conductivity
all_WAP_aphid_abundance_3_day_mean_soil_conductivity_correlation_test <- cor.test(df_new$resid_WAP,
                                                                                  df_new$resid_WAP_3_day_ec,
                                                                                  use = "complete.obs")
# Extract the correlation coefficient and p-value
all_WAP_aphid_abundance_3_day_mean_soil_conductivity_correlation_coefficient <- all_WAP_aphid_abundance_3_day_mean_soil_conductivity_correlation_test$estimate
all_WAP_aphid_abundance_3_day_mean_soil_conductivity_p_value <- all_WAP_aphid_abundance_3_day_mean_soil_conductivity_correlation_test$p.value

max(df_new$resid_WAP_3_day_ec)
min(df_new$resid_WAP_3_day_ec)

plot_all_WAP_3_day_mean_soil_conductivity_aphid_abundance <- 
  ggplot(df_new,
         aes(x = resid_WAP_3_day_ec,
             y = resid_WAP,
             color = `growing season`)) + 
  geom_point(aes(fill = after_scale(scales::alpha(color, 0.30))),
             stroke = 0.2, shape = 21, size = 0.75) +
  geom_smooth(method = "lm", se = TRUE,col = "blue", aes(group = 1),
              alpha = 0.3, size = 0.5) +   # 信賴區間透明度
  
  labs(title = "",
       x = "Prev. 3-D EC ~ WAP",
       y = expression( ~ log[10](Aphids + 1)/m^2  ~ "~ WAP")) +
  theme_classic(base_size = 8, base_family = "sans") +
  theme(plot.title=element_text(size=8, face="bold",hjust = 0.5),
        axis.title=element_text(size=8),
        axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x = element_text(size =6),
        axis.text.y = element_text(size = 6),
        legend.position= "none",        # 顯示圖例
        legend.key.size = unit(12,"points"),
        legend.title = element_blank()) +
  coord_cartesian(clip = "off") +
  scale_color_manual(values = c("2024 spring" = "#006600",  # 深綠
                                "2024 fall"   = "#867052")) +  # 深咖啡
  
  annotate("text", x = -0.05, y = -2.6,
           label = paste0("r = ",
                          round(all_WAP_aphid_abundance_3_day_mean_soil_conductivity_correlation_coefficient, 3),
                          ", p < 0.05 "),
           hjust = 0, vjust = 0, size = 2.5, color = "red", fontface = "bold")  +
  scale_x_continuous(limits = c(-0.1, 0.6)) +
  scale_y_continuous(limits = c(-3.0, 3.0))

#daily mean soil EC
all_WAP_aphid_abundance_3_day_mean_soil_conductivity_spring_correlation_test <- cor.test(df_new_spring$resid_WAP,
                                                                                         df_new_spring$resid_WAP_3_day_ec,
                                                                                         use = "complete.obs")
# Extract the correlation coefficient and p-value
all_WAP_aphid_abundance_3_day_mean_soil_conductivity_spring_correlation_coefficient <- all_WAP_aphid_abundance_3_day_mean_soil_conductivity_spring_correlation_test$estimate
all_WAP_aphid_abundance_3_day_mean_soil_conductivity_spring_p_value <- all_WAP_aphid_abundance_3_day_mean_soil_conductivity_spring_correlation_test$p.value


#daily mean soil moisture
all_WAP_aphid_abundance_3_day_mean_soil_conductivity_fall_correlation_test <- cor.test(df_new_fall$resid_WAP,
                                                                                       df_new_fall$resid_WAP_3_day_ec,
                                                                                       use = "complete.obs")
# Extract the correlation coefficient and p-value
all_WAP_aphid_abundance_3_day_mean_soil_conductivity_fall_correlation_coefficient <- all_WAP_aphid_abundance_3_day_mean_soil_conductivity_fall_correlation_test$estimate
all_WAP_aphid_abundance_3_day_mean_soil_conductivity_fall_p_value <- all_WAP_aphid_abundance_3_day_mean_soil_conductivity_fall_correlation_test$p.value



# Scatter plot with a linear trend line
plot_all_WAP_3_day_mean_soil_conductivity_aphid_abundance_season <- 
  ggplot(df_new,
         aes(x = resid_WAP_3_day_ec,
             y = resid_WAP,
             color = `growing season`)) + 
  geom_point(aes(fill = after_scale(scales::alpha(color, 0.30))),
             stroke = 0.2, shape = 21, size = 0.75) + 
  # 設定回歸線顏色，根據不同的 'growing season' 來顯示不同顏色
  geom_smooth(data = df_new %>% filter(`growing season` == "2024 spring"), method = "lm", se = TRUE,
              color = "#006600",  # 深綠色回歸線
              fill = "#66CC66",   # 淺綠色信賴區間
              alpha = 0.4, size = 0.5) +   # 信賴區間透明度
  geom_smooth(data = df_new %>% filter(`growing season` == "2024 fall"), method = "lm", se = TRUE,
              color = "#8B4513",  # 紅棕色回歸線
              fill = "#C4B7A6",   # 淺咖啡信賴區間
              alpha = 0.4, size = 0.5) +   # 信賴區間透明度
  labs(title = "",
       x = "Prev. 3-D EC ~ WAP",
       y = expression( ~ log[10](Aphids + 1)/m^2 ~ "~ WAP")) +
  theme_classic(base_size = 8, base_family = "sans") +
  theme(plot.title=element_text(size=8, face="bold",hjust = 0.5),
        axis.title=element_text(size=8),
        axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x = element_text(size =6),
        axis.text.y = element_text(size = 6),
        legend.position= "none",        # 顯示圖例
        legend.key.size = unit(12,"points"),
        legend.title = element_blank()) +
  coord_cartesian(clip = "off") +
  # 資料點顏色設置，確保資料點顏色正確
  scale_color_manual(values = c("2024 spring" = "#006600",  # 深綠
                                "2024 fall"   = "#867052")) +  # 深咖啡
  # 區間顏色設置，確保資料的顏色不會被覆蓋
  scale_fill_manual(values = c("2024 spring" = "#66CC66",  # 淺綠
                               "2024 fall"   = "#C4B7A6")) +  # 淺咖啡
  annotate("text", x = -0.05, y = -2.4,
           label = paste0("r = ",
                          round(all_WAP_aphid_abundance_3_day_mean_soil_conductivity_spring_correlation_coefficient, 3),
                          ", p = ",
                          signif(all_WAP_aphid_abundance_3_day_mean_soil_conductivity_spring_p_value, 3)),
           hjust = 0, vjust = 0, size = 2.5, color = "#006600") +
  annotate("text", x = -0.05, y = -2.8,
           label = paste0("r = ",
                          round(all_WAP_aphid_abundance_3_day_mean_soil_conductivity_fall_correlation_coefficient, 3),
                          ", p = ",
                          signif(all_WAP_aphid_abundance_3_day_mean_soil_conductivity_fall_p_value, 3)),
           hjust = 0, vjust = 0, size = 2.5, color = "#8B4513") +
  scale_x_continuous(limits = c(-0.1, 0.6)) +
  scale_y_continuous(limits = c(-3.0, 3.0))


# 5-day mean soil conductivity
all_WAP_aphid_abundance_5_day_mean_soil_conductivity_correlation_test <- cor.test(df_new$resid_WAP,
                                                                                  df_new$resid_WAP_5_day_ec,
                                                                                  use = "complete.obs")
# Extract the correlation coefficient and p-value
all_WAP_aphid_abundance_5_day_mean_soil_conductivity_correlation_coefficient <- all_WAP_aphid_abundance_5_day_mean_soil_conductivity_correlation_test$estimate
all_WAP_aphid_abundance_5_day_mean_soil_conductivity_p_value <- all_WAP_aphid_abundance_5_day_mean_soil_conductivity_correlation_test$p.value

max(df_new$resid_WAP_5_day_ec)
min(df_new$resid_WAP_5_day_ec)

plot_all_WAP_5_day_mean_soil_conductivity_aphid_abundance <- 
  ggplot(df_new,
         aes(x = resid_WAP_5_day_ec,
             y = resid_WAP,
             color = `growing season`)) + 
  geom_point(aes(fill = after_scale(scales::alpha(color, 0.30))),
             stroke = 0.2, shape = 21, size = 0.75) +
  geom_smooth(method = "lm", se = TRUE,col = "blue", aes(group = 1),
              alpha = 0.3, size = 0.5) +   # 信賴區間透明度
  
  labs(title = "",
       x = "Prev. 5-D EC ~ WAP",
       y = expression( ~ log[10](Aphids + 1)/m^2 ~ "~ WAP" )) +
  theme_classic(base_size = 8, base_family = "sans") +
  theme(plot.title=element_text(size=8, face="bold",hjust = 0.5),
        axis.title=element_text(size=8),
        axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x = element_text(size =6),
        axis.text.y = element_text(size = 6),
        legend.position= "none",        # 顯示圖例
        legend.key.size = unit(12,"points"),
        legend.title = element_blank()) +
  coord_cartesian(clip = "off") +
  scale_color_manual(values = c("2024 spring" = "#006600",  # 深綠
                                "2024 fall"   = "#867052")) +  # 深咖啡
  
  annotate("text", x = -0.05, y = -2.6,
           label = paste0("r = ",
                          round(all_WAP_aphid_abundance_5_day_mean_soil_conductivity_correlation_coefficient, 3),
                          ", p < 0.05 "),
           hjust = 0, vjust = 0, size = 2.5, color = "red", fontface = "bold")  +
  scale_x_continuous(limits = c(-0.15, 1.5)) +
  scale_y_continuous(limits = c(-3.0, 3.0))

#daily mean soil EC
all_WAP_aphid_abundance_5_day_mean_soil_conductivity_spring_correlation_test <- cor.test(df_new_spring$resid_WAP,
                                                                                         df_new_spring$resid_WAP_5_day_ec,
                                                                                         use = "complete.obs")
# Extract the correlation coefficient and p-value
all_WAP_aphid_abundance_5_day_mean_soil_conductivity_spring_correlation_coefficient <- all_WAP_aphid_abundance_5_day_mean_soil_conductivity_spring_correlation_test$estimate
all_WAP_aphid_abundance_5_day_mean_soil_conductivity_spring_p_value <- all_WAP_aphid_abundance_5_day_mean_soil_conductivity_spring_correlation_test$p.value


#daily mean soil moisture
all_WAP_aphid_abundance_5_day_mean_soil_conductivity_fall_correlation_test <- cor.test(df_new_fall$resid_WAP,
                                                                                       df_new_fall$resid_WAP_5_day_ec,
                                                                                       use = "complete.obs")
# Extract the correlation coefficient and p-value
all_WAP_aphid_abundance_5_day_mean_soil_conductivity_fall_correlation_coefficient <- all_WAP_aphid_abundance_5_day_mean_soil_conductivity_fall_correlation_test$estimate
all_WAP_aphid_abundance_5_day_mean_soil_conductivity_fall_p_value <- all_WAP_aphid_abundance_5_day_mean_soil_conductivity_fall_correlation_test$p.value



# Scatter plot with a linear trend line
plot_all_WAP_5_day_mean_soil_conductivity_aphid_abundance_season <- 
  ggplot(df_new,
         aes(x = resid_WAP_5_day_ec,
             y = resid_WAP,
             color = `growing season`)) + 
  geom_point(aes(fill = after_scale(scales::alpha(color, 0.30))),
             stroke = 0.2, shape = 21, size = 0.75) + 
  # 設定回歸線顏色，根據不同的 'growing season' 來顯示不同顏色
  geom_smooth(data = df_new %>% filter(`growing season` == "2024 spring"), method = "lm", se = TRUE,
              color = "#006600",  # 深綠色回歸線
              fill = "#66CC66",   # 淺綠色信賴區間
              alpha = 0.4, size = 0.5) +   # 信賴區間透明度
  geom_smooth(data = df_new %>% filter(`growing season` == "2024 fall"), method = "lm", se = TRUE,
              color = "#8B4513",  # 紅棕色回歸線
              fill = "#C4B7A6",   # 淺咖啡信賴區間
              alpha = 0.4, size = 0.5) +   # 信賴區間透明度
  labs(title = "",
       x = "Prev. 5-D EC ~ WAP",
       y = expression( ~ log[10](Aphids + 1)/m^2 ~ "~ WAP")) +
  theme_classic(base_size = 8, base_family = "sans") +
  theme(plot.title=element_text(size=8, face="bold",hjust = 0.5),
        axis.title=element_text(size=8),
        axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x = element_text(size =6),
        axis.text.y = element_text(size = 6),
        legend.position= "none",        # 顯示圖例
        legend.key.size = unit(12,"points"),
        legend.title = element_blank()) +
  coord_cartesian(clip = "off") +
  # 資料點顏色設置，確保資料點顏色正確
  scale_color_manual(values = c("2024 spring" = "#006600",  # 深綠
                                "2024 fall"   = "#867052")) +  # 深咖啡
  # 區間顏色設置，確保資料的顏色不會被覆蓋
  scale_fill_manual(values = c("2024 spring" = "#66CC66",  # 淺綠
                               "2024 fall"   = "#C4B7A6")) +  # 淺咖啡
  annotate("text", x = -0.05, y = -2.4,
           label = paste0("r = ",
                          round(all_WAP_aphid_abundance_5_day_mean_soil_conductivity_spring_correlation_coefficient, 3),
                          ", p < 0.05 "),
           hjust = 0, vjust = 0, size = 2.5, color = "#006600", fontface = "bold") +
  annotate("text", x = -0.05, y = -2.8,
           label = paste0("r = ",
                          round(all_WAP_aphid_abundance_5_day_mean_soil_conductivity_fall_correlation_coefficient, 3),
                          ", p = ",
                          signif(all_WAP_aphid_abundance_5_day_mean_soil_conductivity_fall_p_value, 3)),
           hjust = 0, vjust = 0, size = 2.5, color = "#8B4513") +
  scale_x_continuous(limits = c(-0.15, 1.5)) +
  scale_y_continuous(limits = c(-3.0, 3.0))

# 7-day mean soil conductivity
all_WAP_aphid_abundance_7_day_mean_soil_conductivity_correlation_test <- cor.test(df_new$resid_WAP,
                                                                                  df_new$resid_WAP_7_day_ec,
                                                                                  use = "complete.obs")
# Extract the correlation coefficient and p-value
all_WAP_aphid_abundance_7_day_mean_soil_conductivity_correlation_coefficient <- all_WAP_aphid_abundance_7_day_mean_soil_conductivity_correlation_test$estimate
all_WAP_aphid_abundance_7_day_mean_soil_conductivity_p_value <- all_WAP_aphid_abundance_7_day_mean_soil_conductivity_correlation_test$p.value

max(df_new$resid_WAP_7_day_ec)
min(df_new$resid_WAP_7_day_ec)

plot_all_WAP_7_day_mean_soil_conductivity_aphid_abundance <- 
  ggplot(df_new,
         aes(x = resid_WAP_7_day_ec,
             y = resid_WAP,
             color = `growing season`)) + 
  geom_point(aes(fill = after_scale(scales::alpha(color, 0.30))),
             stroke = 0.2, shape = 21, size = 0.75) +
  geom_smooth(method = "lm", se = TRUE,col = "blue", aes(group = 1),
              alpha = 0.3, size = 0.5) +   # 信賴區間透明度
  
  labs(title = "",
       x = "Prev. 7-D EC ~ WAP",
       y = expression( ~ log[10](Aphids + 1)/m^2 ~ "~ WAP")) +
  theme_classic(base_size = 8, base_family = "sans") +
  theme(plot.title=element_text(size=8, face="bold",hjust = 0.5),
        axis.title=element_text(size=8),
        axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x = element_text(size =6),
        axis.text.y = element_text(size = 6),
        legend.position= "none",        # 顯示圖例
        legend.key.size = unit(12,"points"),
        legend.title = element_blank()) +
  coord_cartesian(clip = "off") +
  scale_color_manual(values = c("2024 spring" = "#006600",  # 深綠
                                "2024 fall"   = "#867052")) +  # 深咖啡
  
  annotate("text", x = -0.05, y = -2.6,
           label = paste0("r = ",
                          round(all_WAP_aphid_abundance_7_day_mean_soil_conductivity_correlation_coefficient, 3),
                          ", p < 0.001 "),
           hjust = 0, vjust = 0, size = 2.5, color = "red", fontface = "bold")  +
  scale_x_continuous(limits = c(-0.15, 1.0)) +
  scale_y_continuous(limits = c(-3.0, 3.0))

#daily mean soil EC
all_WAP_aphid_abundance_7_day_mean_soil_conductivity_spring_correlation_test <- cor.test(df_new_spring$resid_WAP,
                                                                                         df_new_spring$resid_WAP_7_day_ec,
                                                                                         use = "complete.obs")
# Extract the correlation coefficient and p-value
all_WAP_aphid_abundance_7_day_mean_soil_conductivity_spring_correlation_coefficient <- all_WAP_aphid_abundance_7_day_mean_soil_conductivity_spring_correlation_test$estimate
all_WAP_aphid_abundance_7_day_mean_soil_conductivity_spring_p_value <- all_WAP_aphid_abundance_7_day_mean_soil_conductivity_spring_correlation_test$p.value


#daily mean soil moisture
all_WAP_aphid_abundance_7_day_mean_soil_conductivity_fall_correlation_test <- cor.test(df_new_fall$resid_WAP,
                                                                                       df_new_fall$resid_WAP_7_day_ec,
                                                                                       use = "complete.obs")
# Extract the correlation coefficient and p-value
all_WAP_aphid_abundance_7_day_mean_soil_conductivity_fall_correlation_coefficient <- all_WAP_aphid_abundance_7_day_mean_soil_conductivity_fall_correlation_test$estimate
all_WAP_aphid_abundance_7_day_mean_soil_conductivity_fall_p_value <- all_WAP_aphid_abundance_7_day_mean_soil_conductivity_fall_correlation_test$p.value



# Scatter plot with a linear trend line
plot_all_WAP_7_day_mean_soil_conductivity_aphid_abundance_season <- 
  ggplot(df_new,
         aes(x = resid_WAP_7_day_ec,
             y = resid_WAP,
             color = `growing season`)) + 
  geom_point(aes(fill = after_scale(scales::alpha(color, 0.30))),
             stroke = 0.2, shape = 21, size = 0.75) + 
  # 設定回歸線顏色，根據不同的 'growing season' 來顯示不同顏色
  geom_smooth(data = df_new %>% filter(`growing season` == "2024 spring"), method = "lm", se = TRUE,
              color = "#006600",  # 深綠色回歸線
              fill = "#66CC66",   # 淺綠色信賴區間
              alpha = 0.4, size = 0.5) +   # 信賴區間透明度
  geom_smooth(data = df_new %>% filter(`growing season` == "2024 fall"), method = "lm", se = TRUE,
              color = "#8B4513",  # 紅棕色回歸線
              fill = "#C4B7A6",   # 淺咖啡信賴區間
              alpha = 0.4, size = 0.5) +   # 信賴區間透明度
  labs(title = "",
       x = "Prev. 7-D EC ~ WAP",
       y = expression( ~ log[10](Aphids + 1)/m^2 ~ "~ WAP")) +
  theme_classic(base_size = 8, base_family = "sans") +
  theme(plot.title=element_text(size=8, face="bold",hjust = 0.5),
        axis.title=element_text(size=8),
        axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x = element_text(size =6),
        axis.text.y = element_text(size = 6),
        legend.position= "none",        # 顯示圖例
        legend.key.size = unit(12,"points"),
        legend.title = element_blank()) +
  coord_cartesian(clip = "off") +
  # 資料點顏色設置，確保資料點顏色正確
  scale_color_manual(values = c("2024 spring" = "#006600",  # 深綠
                                "2024 fall"   = "#867052")) +  # 深咖啡
  # 區間顏色設置，確保資料的顏色不會被覆蓋
  scale_fill_manual(values = c("2024 spring" = "#66CC66",  # 淺綠
                               "2024 fall"   = "#C4B7A6")) +  # 淺咖啡
  annotate("text", x = -0.05, y = -2.4,
           label = paste0("r = ",
                          round(all_WAP_aphid_abundance_7_day_mean_soil_conductivity_spring_correlation_coefficient, 3),
                          ", p < 0.05 "),
           hjust = 0, vjust = 0, size = 2.5, color = "#006600", fontface = "bold") +
  annotate("text", x = -0.05, y = -2.8,
           label = paste0("r = ",
                          round(all_WAP_aphid_abundance_7_day_mean_soil_conductivity_fall_correlation_coefficient, 3),
                          ", p = ",
                          signif(all_WAP_aphid_abundance_7_day_mean_soil_conductivity_fall_p_value, 3)),
           hjust = 0, vjust = 0, size = 2.5, color = "#8B4513") +
  scale_x_continuous(limits = c(-0.15, 1.0)) +
  scale_y_continuous(limits = c(-3.0, 3.0))

#------------------------------------------------------------------------------------------------------------------------------------------------
# stage ignore for now 2025-10-21

#daily mean soil moisture
all_stage_aphid_abundance_daily_mean_soil_moisture_correlation_test <- cor.test(df_new$resid_stage,
                                                                                df_new$resid_stage_daily_mois,
                                                                                use = "complete.obs")
# Extract the correlation coefficient and p-value
all_stage_aphid_abundance_daily_mean_soil_moisture_correlation_coefficient <- all_stage_aphid_abundance_daily_mean_soil_moisture_correlation_test$estimate
all_stage_aphid_abundance_daily_mean_soil_moisture_p_value <- all_stage_aphid_abundance_daily_mean_soil_moisture_correlation_test$p.value

max(df_new$resid_stage_daily_mois)
min(df_new$resid_stage_daily_mois)

plot_all_stage_daily_mean_soil_moisture_aphid_abundance <- 
  ggplot(df_new,
         aes(x = resid_stage_daily_mois,
             y = resid_stage,
             color = `growing season`)) + 
  geom_point(aes(fill = after_scale(scales::alpha(color, 0.30))),
             stroke = 0.2, shape = 21, size = 0.75) +
  geom_smooth(method = "lm", se = TRUE,col = "blue", aes(group = 1),
              alpha = 0.3, size = 0.5) +   # 信賴區間透明度
  
  labs(title = "",
       x = "Daily Mois. (Stage)",
       y = expression( ~ log[10](Aphids + 1)/m^2 ~ "(Stage)")) +
  theme_classic(base_size = 8, base_family = "sans") +
  theme(plot.title=element_text(size=8, face="bold",hjust = 0.5),
        axis.title=element_text(size=8),
        axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x = element_text(size =6),
        axis.text.y = element_text(size = 6),
        legend.position= "none",        # 顯示圖例
        legend.key.size = unit(12,"points"),
        legend.title = element_blank()) +
  coord_cartesian(clip = "off") +
  scale_color_manual(values = c("2024 spring" = "#006600",  # 深綠
                                "2024 fall"   = "#867052")) +  # 深咖啡
  
  annotate("text", x = -15, y = -2.6,
           label = paste0("r = ",
                          round(all_stage_aphid_abundance_daily_mean_soil_moisture_correlation_coefficient, 3),
                          ", p < 0.05 "),
           hjust = 0, vjust = 0, size = 2.5, color = "red")  +
  scale_x_continuous(limits = c(-21, 20)) +
  scale_y_continuous(limits = c(-3.0, 3.0))


#daily mean soil moisture
all_stage_aphid_abundance_daily_mean_soil_moisture_spring_correlation_test <- cor.test(df_new_spring$resid_stage,
                                                                                       df_new_spring$resid_stage_daily_mois,
                                                                                       use = "complete.obs")
# Extract the correlation coefficient and p-value
all_stage_aphid_abundance_daily_mean_soil_moisture_spring_correlation_coefficient <- all_stage_aphid_abundance_daily_mean_soil_moisture_spring_correlation_test$estimate
all_stage_aphid_abundance_daily_mean_soil_moisture_spring_p_value <- all_stage_aphid_abundance_daily_mean_soil_moisture_spring_correlation_test$p.value


#daily mean soil moisture
all_stage_aphid_abundance_daily_mean_soil_moisture_fall_correlation_test <- cor.test(df_new_fall$resid_stage,
                                                                                     df_new_fall$resid_stage_daily_mois,
                                                                                     use = "complete.obs")
# Extract the correlation coefficient and p-value
all_stage_aphid_abundance_daily_mean_soil_moisture_fall_correlation_coefficient <- all_stage_aphid_abundance_daily_mean_soil_moisture_fall_correlation_test$estimate
all_stage_aphid_abundance_daily_mean_soil_moisture_fall_p_value <- all_stage_aphid_abundance_daily_mean_soil_moisture_fall_correlation_test$p.value


library(scales)  # for alpha()
# scale_color_manual(values = c("2024 spring" = "#006600", "2024 fall" = "#867052"))
# Scatter plot with a linear trend line
plot_all_stage_daily_mean_soil_moisture_aphid_abundance_season <- 
  ggplot(df_new,
         aes(x = resid_stage_daily_mois,
             y = resid_stage,
             color = `growing season`)) + 
  geom_point(aes(fill = after_scale(scales::alpha(color, 0.30))),
             stroke = 0.2, shape = 21, size = 0.75) + 
  # 設定回歸線顏色，根據不同的 'growing season' 來顯示不同顏色
  geom_smooth(data = df_new %>% filter(`growing season` == "2024 spring"), method = "lm", se = TRUE,
              color = "#006600",  # 深綠色回歸線
              fill = "#66CC66",   # 淺綠色信賴區間
              alpha = 0.4, size = 0.5) +   # 信賴區間透明度
  geom_smooth(data = df_new %>% filter(`growing season` == "2024 fall"), method = "lm", se = TRUE,
              color = "#8B4513",  # 紅棕色回歸線
              fill = "#C4B7A6",   # 淺咖啡信賴區間
              alpha = 0.4, size = 0.5) +   # 信賴區間透明度
  labs(title = "",
       x = "Daily Mois. (Stage)",
       y = expression( ~ log[10](Aphids + 1)/m^2 ~ "(Stage)")) +
  theme_classic(base_size = 8, base_family = "sans") +
  theme(plot.title=element_text(size=8, face="bold",hjust = 0.5),
        axis.title=element_text(size=8),
        axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x = element_text(size =6),
        axis.text.y = element_text(size = 6),
        legend.position= "none",        # 顯示圖例
        legend.key.size = unit(12,"points"),
        legend.title = element_blank()) +
  coord_cartesian(clip = "off") +
  # 資料點顏色設置，確保資料點顏色正確
  scale_color_manual(values = c("2024 spring" = "#006600",  # 深綠
                                "2024 fall"   = "#867052")) +  # 深咖啡
  # 區間顏色設置，確保資料的顏色不會被覆蓋
  scale_fill_manual(values = c("2024 spring" = "#66CC66",  # 淺綠
                               "2024 fall"   = "#C4B7A6")) +  # 淺咖啡
  annotate("text", x = -15, y = -2.4,
           label = paste0("r = ",
                          round(all_stage_aphid_abundance_daily_mean_soil_moisture_spring_correlation_coefficient, 3),
                          ", p = ",
                          signif(all_stage_aphid_abundance_daily_mean_soil_moisture_spring_p_value, 3)),
           hjust = 0, vjust = 0, size = 2.5, color = "#006600") +
  annotate("text", x = -15, y = -2.8,
           label = paste0("r = ",
                          round(all_stage_aphid_abundance_daily_mean_soil_moisture_fall_correlation_coefficient, 3),
                          ", p < 0.001 "),
           hjust = 0, vjust = 0, size = 2.5, color = "#8B4513", fontface = "bold") +
  scale_x_continuous(limits = c(-21, 20)) +
  scale_y_continuous(limits = c(-3.0, 3.0))



#daily mean soil moisture
all_stage_aphid_abundance_1_day_mean_soil_moisture_correlation_test <- cor.test(df_new$resid_stage,
                                                                                df_new$resid_stage_1_day_mois,
                                                                                use = "complete.obs")
# Extract the correlation coefficient and p-value
all_stage_aphid_abundance_1_day_mean_soil_moisture_correlation_coefficient <- all_stage_aphid_abundance_1_day_mean_soil_moisture_correlation_test$estimate
all_stage_aphid_abundance_1_day_mean_soil_moisture_p_value <- all_stage_aphid_abundance_1_day_mean_soil_moisture_correlation_test$p.value

max(df_new$resid_stage_1_day_mois)
min(df_new$resid_stage_1_day_mois)

plot_all_stage_1_day_mean_soil_moisture_aphid_abundance <- 
  ggplot(df_new,
         aes(x = resid_stage_1_day_mois,
             y = resid_stage,
             color = `growing season`)) + 
  geom_point(aes(fill = after_scale(scales::alpha(color, 0.30))),
             stroke = 0.2, shape = 21, size = 0.75) +
  geom_smooth(method = "lm", se = TRUE,col = "blue", aes(group = 1),
              alpha = 0.3, size = 0.5) +   # 信賴區間透明度
  
  labs(title = "",
       x = "Prev. 1-D Mois. (Stage)",
       y = expression( ~ log[10](Aphids + 1)/m^2 ~ "(Stage)")) +
  theme_classic(base_size = 8, base_family = "sans") +
  theme(plot.title=element_text(size=8, face="bold",hjust = 0.5),
        axis.title=element_text(size=8),
        axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x = element_text(size =6),
        axis.text.y = element_text(size = 6),
        legend.position= "none",        # 顯示圖例
        legend.key.size = unit(12,"points"),
        legend.title = element_blank()) +
  coord_cartesian(clip = "off") +
  scale_color_manual(values = c("2024 spring" = "#006600",  # 深綠
                                "2024 fall"   = "#867052")) +  # 深咖啡
  
  annotate("text", x = -15, y = -2.6,
           label = paste0("r = ",
                          round(all_stage_aphid_abundance_1_day_mean_soil_moisture_correlation_coefficient, 3),
                          ", p < 0.001 "),
           hjust = 0, vjust = 0, size = 2.5, color = "red", fontface = "bold")  +
  scale_x_continuous(limits = c(-21, 20)) +
  scale_y_continuous(limits = c(-3.0, 3.0))


#daily mean soil moisture
all_stage_aphid_abundance_1_day_mean_soil_moisture_spring_correlation_test <- cor.test(df_new_spring$resid_stage,
                                                                                       df_new_spring$resid_stage_1_day_mois,
                                                                                       use = "complete.obs")
# Extract the correlation coefficient and p-value
all_stage_aphid_abundance_1_day_mean_soil_moisture_spring_correlation_coefficient <- all_stage_aphid_abundance_1_day_mean_soil_moisture_spring_correlation_test$estimate
all_stage_aphid_abundance_1_day_mean_soil_moisture_spring_p_value <- all_stage_aphid_abundance_1_day_mean_soil_moisture_spring_correlation_test$p.value


#daily mean soil moisture
all_stage_aphid_abundance_1_day_mean_soil_moisture_fall_correlation_test <- cor.test(df_new_fall$resid_stage,
                                                                                     df_new_fall$resid_stage_1_day_mois,
                                                                                     use = "complete.obs")
# Extract the correlation coefficient and p-value
all_stage_aphid_abundance_1_day_mean_soil_moisture_fall_correlation_coefficient <- all_stage_aphid_abundance_1_day_mean_soil_moisture_fall_correlation_test$estimate
all_stage_aphid_abundance_1_day_mean_soil_moisture_fall_p_value <- all_stage_aphid_abundance_1_day_mean_soil_moisture_fall_correlation_test$p.value


library(scales)  # for alpha()
# scale_color_manual(values = c("2024 spring" = "#006600", "2024 fall" = "#867052"))
# Scatter plot with a linear trend line
plot_all_stage_1_day_mean_soil_moisture_aphid_abundance_season <- 
  ggplot(df_new,
         aes(x = resid_stage_1_day_mois,
             y = resid_stage,
             color = `growing season`)) + 
  geom_point(aes(fill = after_scale(scales::alpha(color, 0.30))),
             stroke = 0.2, shape = 21, size = 0.75) + 
  # 設定回歸線顏色，根據不同的 'growing season' 來顯示不同顏色
  geom_smooth(data = df_new %>% filter(`growing season` == "2024 spring"), method = "lm", se = TRUE,
              color = "#006600",  # 深綠色回歸線
              fill = "#66CC66",   # 淺綠色信賴區間
              alpha = 0.4, size = 0.5) +   # 信賴區間透明度
  geom_smooth(data = df_new %>% filter(`growing season` == "2024 fall"), method = "lm", se = TRUE,
              color = "#8B4513",  # 紅棕色回歸線
              fill = "#C4B7A6",   # 淺咖啡信賴區間
              alpha = 0.4, size = 0.5) +   # 信賴區間透明度
  labs(title = "",
       x = "Prev. 1-D Mois. (Stage)",
       y = expression( ~ log[10](Aphids + 1)/m^2 ~ "(Stage)")) +
  theme_classic(base_size = 8, base_family = "sans") +
  theme(plot.title=element_text(size=8, face="bold",hjust = 0.5),
        axis.title=element_text(size=8),
        axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x = element_text(size =6),
        axis.text.y = element_text(size = 6),
        legend.position= "none",        # 顯示圖例
        legend.key.size = unit(12,"points"),
        legend.title = element_blank()) +
  coord_cartesian(clip = "off") +
  # 資料點顏色設置，確保資料點顏色正確
  scale_color_manual(values = c("2024 spring" = "#006600",  # 深綠
                                "2024 fall"   = "#867052")) +  # 深咖啡
  # 區間顏色設置，確保資料的顏色不會被覆蓋
  scale_fill_manual(values = c("2024 spring" = "#66CC66",  # 淺綠
                               "2024 fall"   = "#C4B7A6")) +  # 淺咖啡
  annotate("text", x = -15, y = -2.4,
           label = paste0("r = ",
                          round(all_stage_aphid_abundance_1_day_mean_soil_moisture_spring_correlation_coefficient, 3),
                          ", p < 0.05 "),
           hjust = 0, vjust = 0, size = 2.5, color = "#006600", fontface = "bold") +
  annotate("text", x = -15, y = -2.8,
           label = paste0("r = ",
                          round(all_stage_aphid_abundance_1_day_mean_soil_moisture_fall_correlation_coefficient, 3),
                          ", p < 0.001 "),
           hjust = 0, vjust = 0, size = 2.5, color = "#8B4513", fontface = "bold") +
  scale_x_continuous(limits = c(-21, 20)) +
  scale_y_continuous(limits = c(-3.0, 3.0))


#3-day mean soil moisture

all_stage_aphid_abundance_3_day_mean_soil_moisture_correlation_test <- cor.test(df_new$resid_stage,
                                                                                df_new$resid_stage_3_day_mois,
                                                                                use = "complete.obs")
# Extract the correlation coefficient and p-value
all_stage_aphid_abundance_3_day_mean_soil_moisture_correlation_coefficient <- all_stage_aphid_abundance_3_day_mean_soil_moisture_correlation_test$estimate
all_stage_aphid_abundance_3_day_mean_soil_moisture_p_value <- all_stage_aphid_abundance_3_day_mean_soil_moisture_correlation_test$p.value

max(df_new$resid_stage_3_day_mois)
min(df_new$resid_stage_3_day_mois)

plot_all_stage_3_day_mean_soil_moisture_aphid_abundance <- 
  ggplot(df_new,
         aes(x = resid_stage_3_day_mois,
             y = resid_stage,
             color = `growing season`)) + 
  geom_point(aes(fill = after_scale(scales::alpha(color, 0.30))),
             stroke = 0.2, shape = 21, size = 0.75) +
  geom_smooth(method = "lm", se = TRUE,col = "blue", aes(group = 1),
              alpha = 0.3, size = 0.5) +   # 信賴區間透明度
  
  labs(title = "",
       x = "Prev. 3-D Mois. (Stage)",
       y = expression( ~ log[10](Aphids + 1)/m^2 ~ "(Stage)")) +
  theme_classic(base_size = 8, base_family = "sans") +
  theme(plot.title=element_text(size=8, face="bold",hjust = 0.5),
        axis.title=element_text(size=8),
        axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x = element_text(size =6),
        axis.text.y = element_text(size = 6),
        legend.position= "none",        # 顯示圖例
        legend.key.size = unit(12,"points"),
        legend.title = element_blank()) +
  coord_cartesian(clip = "off") +
  scale_color_manual(values = c("2024 spring" = "#006600",  # 深綠
                                "2024 fall"   = "#867052")) +  # 深咖啡
  
  annotate("text", x = -12.5, y = -2.6,
           label = paste0("r = ",
                          round(all_stage_aphid_abundance_3_day_mean_soil_moisture_correlation_coefficient, 3),
                          ", p < 0.05 "),
           hjust = 0, vjust = 0, size = 2.5, color = "red", fontface = "bold")  +
  scale_x_continuous(limits = c(-14, 16)) +
  scale_y_continuous(limits = c(-3.0, 3.0))


#daily mean soil moisture
all_stage_aphid_abundance_3_day_mean_soil_moisture_spring_correlation_test <- cor.test(df_new_spring$resid_stage,
                                                                                       df_new_spring$resid_stage_3_day_mois,
                                                                                       use = "complete.obs")
# Extract the correlation coefficient and p-value
all_stage_aphid_abundance_3_day_mean_soil_moisture_spring_correlation_coefficient <- all_stage_aphid_abundance_3_day_mean_soil_moisture_spring_correlation_test$estimate
all_stage_aphid_abundance_3_day_mean_soil_moisture_spring_p_value <- all_stage_aphid_abundance_3_day_mean_soil_moisture_spring_correlation_test$p.value


#daily mean soil moisture
all_stage_aphid_abundance_3_day_mean_soil_moisture_fall_correlation_test <- cor.test(df_new_fall$resid_stage,
                                                                                     df_new_fall$resid_stage_3_day_mois,
                                                                                     use = "complete.obs")
# Extract the correlation coefficient and p-value
all_stage_aphid_abundance_3_day_mean_soil_moisture_fall_correlation_coefficient <- all_stage_aphid_abundance_3_day_mean_soil_moisture_fall_correlation_test$estimate
all_stage_aphid_abundance_3_day_mean_soil_moisture_fall_p_value <- all_stage_aphid_abundance_3_day_mean_soil_moisture_fall_correlation_test$p.value


library(scales)  # for alpha()
# scale_color_manual(values = c("2024 spring" = "#006600", "2024 fall" = "#867052"))
# Scatter plot with a linear trend line
plot_all_stage_3_day_mean_soil_moisture_aphid_abundance_season <- 
  ggplot(df_new,
         aes(x = resid_stage_3_day_mois,
             y = resid_stage,
             color = `growing season`)) + 
  geom_point(aes(fill = after_scale(scales::alpha(color, 0.30))),
             stroke = 0.2, shape = 21, size = 0.75) + 
  # 設定回歸線顏色，根據不同的 'growing season' 來顯示不同顏色
  geom_smooth(data = df_new %>% filter(`growing season` == "2024 spring"), method = "lm", se = TRUE,
              color = "#006600",  # 深綠色回歸線
              fill = "#66CC66",   # 淺綠色信賴區間
              alpha = 0.4, size = 0.5) +   # 信賴區間透明度
  geom_smooth(data = df_new %>% filter(`growing season` == "2024 fall"), method = "lm", se = TRUE,
              color = "#8B4513",  # 紅棕色回歸線
              fill = "#C4B7A6",   # 淺咖啡信賴區間
              alpha = 0.4, size = 0.5) +   # 信賴區間透明度
  labs(title = "",
       x = "Prev. 3-D Mois. (Stage)",
       y = expression( ~ log[10](Aphids + 1)/m^2 ~ "(Stage)")) +
  theme_classic(base_size = 8, base_family = "sans") +
  theme(plot.title=element_text(size=8, face="bold",hjust = 0.5),
        axis.title=element_text(size=8),
        axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x = element_text(size =6),
        axis.text.y = element_text(size = 6),
        legend.position= "none",        # 顯示圖例
        legend.key.size = unit(12,"points"),
        legend.title = element_blank()) +
  coord_cartesian(clip = "off") +
  # 資料點顏色設置，確保資料點顏色正確
  scale_color_manual(values = c("2024 spring" = "#006600",  # 深綠
                                "2024 fall"   = "#867052")) +  # 深咖啡
  # 區間顏色設置，確保資料的顏色不會被覆蓋
  scale_fill_manual(values = c("2024 spring" = "#66CC66",  # 淺綠
                               "2024 fall"   = "#C4B7A6")) +  # 淺咖啡
  annotate("text", x = -12.5, y = -2.4,
           label = paste0("r = ",
                          round(all_stage_aphid_abundance_3_day_mean_soil_moisture_spring_correlation_coefficient, 3),
                          ", p = ",
                          signif(all_stage_aphid_abundance_3_day_mean_soil_moisture_spring_p_value, 3)),
           hjust = 0, vjust = 0, size = 2.5, color = "#006600") +
  annotate("text", x = -12.5, y = -2.8,
           label = paste0("r = ",
                          round(all_stage_aphid_abundance_3_day_mean_soil_moisture_fall_correlation_coefficient, 3),
                          ", p <0.001 "),
           hjust = 0, vjust = 0, size = 2.5, color = "#8B4513", fontface = "bold") +
  scale_x_continuous(limits = c(-14, 16)) +
  scale_y_continuous(limits = c(-3.0, 3.0))


#5-day mean soil moisture

all_stage_aphid_abundance_5_day_mean_soil_moisture_correlation_test <- cor.test(df_new$resid_stage,
                                                                                df_new$resid_stage_5_day_mois,
                                                                                use = "complete.obs")
# Extract the correlation coefficient and p-value
all_stage_aphid_abundance_5_day_mean_soil_moisture_correlation_coefficient <- all_stage_aphid_abundance_5_day_mean_soil_moisture_correlation_test$estimate
all_stage_aphid_abundance_5_day_mean_soil_moisture_p_value <- all_stage_aphid_abundance_5_day_mean_soil_moisture_correlation_test$p.value

max(df_new$resid_stage_5_day_mois)
min(df_new$resid_stage_5_day_mois)

plot_all_stage_5_day_mean_soil_moisture_aphid_abundance <- 
  ggplot(df_new,
         aes(x = resid_stage_5_day_mois,
             y = resid_stage,
             color = `growing season`)) + 
  geom_point(aes(fill = after_scale(scales::alpha(color, 0.30))),
             stroke = 0.2, shape = 21, size = 0.75) +
  geom_smooth(method = "lm", se = TRUE,col = "blue", aes(group = 1),
              alpha = 0.3, size = 0.5) +   # 信賴區間透明度
  
  labs(title = "",
       x = "Prev. 5-D Mois. (Stage)",
       y = expression( ~ log[10](Aphids + 1)/m^2 ~ "(Stage)")) +
  theme_classic(base_size = 8, base_family = "sans") +
  theme(plot.title=element_text(size=8, face="bold",hjust = 0.5),
        axis.title=element_text(size=8),
        axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x = element_text(size =6),
        axis.text.y = element_text(size = 6),
        legend.position= "none",        # 顯示圖例
        legend.key.size = unit(12,"points"),
        legend.title = element_blank()) +
  coord_cartesian(clip = "off") +
  scale_color_manual(values = c("2024 spring" = "#006600",  # 深綠
                                "2024 fall"   = "#867052")) +  # 深咖啡
  
  annotate("text", x = -12.5, y = -2.6,
           label = paste0("r = ",
                          round(all_stage_aphid_abundance_5_day_mean_soil_moisture_correlation_coefficient, 3),
                          ", p = ",
                          signif (all_stage_aphid_abundance_5_day_mean_soil_moisture_p_value, 3)),
           hjust = 0, vjust = 0, size = 2.5, color = "black")  +
  scale_x_continuous(limits = c(-15, 17)) +
  scale_y_continuous(limits = c(-3.0, 3.0))


#daily mean soil moisture
all_stage_aphid_abundance_5_day_mean_soil_moisture_spring_correlation_test <- cor.test(df_new_spring$resid_stage,
                                                                                       df_new_spring$resid_stage_5_day_mois,
                                                                                       use = "complete.obs")
# Extract the correlation coefficient and p-value
all_stage_aphid_abundance_5_day_mean_soil_moisture_spring_correlation_coefficient <- all_stage_aphid_abundance_5_day_mean_soil_moisture_spring_correlation_test$estimate
all_stage_aphid_abundance_5_day_mean_soil_moisture_spring_p_value <- all_stage_aphid_abundance_5_day_mean_soil_moisture_spring_correlation_test$p.value


#daily mean soil moisture
all_stage_aphid_abundance_5_day_mean_soil_moisture_fall_correlation_test <- cor.test(df_new_fall$resid_stage,
                                                                                     df_new_fall$resid_stage_5_day_mois,
                                                                                     use = "complete.obs")
# Extract the correlation coefficient and p-value
all_stage_aphid_abundance_5_day_mean_soil_moisture_fall_correlation_coefficient <- all_stage_aphid_abundance_5_day_mean_soil_moisture_fall_correlation_test$estimate
all_stage_aphid_abundance_5_day_mean_soil_moisture_fall_p_value <- all_stage_aphid_abundance_5_day_mean_soil_moisture_fall_correlation_test$p.value


library(scales)  # for alpha()
# scale_color_manual(values = c("2024 spring" = "#006600", "2024 fall" = "#867052"))
# Scatter plot with a linear trend line
plot_all_stage_5_day_mean_soil_moisture_aphid_abundance_season <- 
  ggplot(df_new,
         aes(x = resid_stage_5_day_mois,
             y = resid_stage,
             color = `growing season`)) + 
  geom_point(aes(fill = after_scale(scales::alpha(color, 0.30))),
             stroke = 0.2, shape = 21, size = 0.75) + 
  # 設定回歸線顏色，根據不同的 'growing season' 來顯示不同顏色
  geom_smooth(data = df_new %>% filter(`growing season` == "2024 spring"), method = "lm", se = TRUE,
              color = "#006600",  # 深綠色回歸線
              fill = "#66CC66",   # 淺綠色信賴區間
              alpha = 0.4, size = 0.5) +   # 信賴區間透明度
  geom_smooth(data = df_new %>% filter(`growing season` == "2024 fall"), method = "lm", se = TRUE,
              color = "#8B4513",  # 紅棕色回歸線
              fill = "#C4B7A6",   # 淺咖啡信賴區間
              alpha = 0.4, size = 0.5) +   # 信賴區間透明度
  labs(title = "",
       x = "Prev. 5-D Mois. (Stage)",
       y = expression( ~ log[10](Aphids + 1)/m^2 ~ "(Stage)")) +
  theme_classic(base_size = 8, base_family = "sans") +
  theme(plot.title=element_text(size=8, face="bold",hjust = 0.5),
        axis.title=element_text(size=8),
        axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x = element_text(size =6),
        axis.text.y = element_text(size = 6),
        legend.position= "none",        # 顯示圖例
        legend.key.size = unit(12,"points"),
        legend.title = element_blank()) +
  coord_cartesian(clip = "off") +
  # 資料點顏色設置，確保資料點顏色正確
  scale_color_manual(values = c("2024 spring" = "#006600",  # 深綠
                                "2024 fall"   = "#867052")) +  # 深咖啡
  # 區間顏色設置，確保資料的顏色不會被覆蓋
  scale_fill_manual(values = c("2024 spring" = "#66CC66",  # 淺綠
                               "2024 fall"   = "#C4B7A6")) +  # 淺咖啡
  annotate("text", x = -12.5, y = -2.4,
           label = paste0("r = ",
                          round(all_stage_aphid_abundance_5_day_mean_soil_moisture_spring_correlation_coefficient, 3),
                          ", p = ",
                          signif(all_stage_aphid_abundance_5_day_mean_soil_moisture_spring_p_value, 3)),
           hjust = 0, vjust = 0, size = 2.5, color = "#006600") +
  annotate("text", x = -12.5, y = -2.8,
           label = paste0("r = ",
                          round(all_stage_aphid_abundance_5_day_mean_soil_moisture_fall_correlation_coefficient, 3),
                          ", p < 0.05 "),
           hjust = 0, vjust = 0, size = 2.5, color = "#8B4513", fontface = "bold") +
  scale_x_continuous(limits = c(-15, 17)) +
  scale_y_continuous(limits = c(-3.0, 3.0))

# 7-day mean soil moisture

all_stage_aphid_abundance_7_day_mean_soil_moisture_correlation_test <- cor.test(df_new$resid_stage,
                                                                                df_new$resid_stage_7_day_mois,
                                                                                use = "complete.obs")
# Extract the correlation coefficient and p-value
all_stage_aphid_abundance_7_day_mean_soil_moisture_correlation_coefficient <- all_stage_aphid_abundance_7_day_mean_soil_moisture_correlation_test$estimate
all_stage_aphid_abundance_7_day_mean_soil_moisture_p_value <- all_stage_aphid_abundance_7_day_mean_soil_moisture_correlation_test$p.value

max(df_new$resid_stage_7_day_mois)
min(df_new$resid_stage_7_day_mois)

plot_all_stage_7_day_mean_soil_moisture_aphid_abundance <- 
  ggplot(df_new,
         aes(x = resid_stage_7_day_mois,
             y = resid_stage,
             color = `growing season`)) + 
  geom_point(aes(fill = after_scale(scales::alpha(color, 0.30))),
             stroke = 0.2, shape = 21, size = 0.75) +
  geom_smooth(method = "lm", se = TRUE,col = "blue", aes(group = 1),
              alpha = 0.3, size = 0.5) +   # 信賴區間透明度
  
  labs(title = "",
       x = "Prev. 7-D Mois. (Stage)",
       y = expression( ~ log[10](Aphids + 1)/m^2 ~ "(Stage)")) +
  theme_classic(base_size = 8, base_family = "sans") +
  theme(plot.title=element_text(size=8, face="bold",hjust = 0.5),
        axis.title=element_text(size=8),
        axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x = element_text(size =6),
        axis.text.y = element_text(size = 6),
        legend.position= "none",        # 顯示圖例
        legend.key.size = unit(12,"points"),
        legend.title = element_blank()) +
  coord_cartesian(clip = "off") +
  scale_color_manual(values = c("2024 spring" = "#006600",  # 深綠
                                "2024 fall"   = "#867052")) +  # 深咖啡
  
  annotate("text", x = -12.5, y = -2.6,
           label = paste0("r = ",
                          round(all_stage_aphid_abundance_7_day_mean_soil_moisture_correlation_coefficient, 3),
                          ", p = ",
                          signif(all_stage_aphid_abundance_7_day_mean_soil_moisture_p_value, 3)),
           hjust = 0, vjust = 0, size = 2.5, color = "black")  +
  scale_x_continuous(limits = c(-15, 15)) +
  scale_y_continuous(limits = c(-3.0, 3.0))


#daily mean soil moisture
all_stage_aphid_abundance_7_day_mean_soil_moisture_spring_correlation_test <- cor.test(df_new_spring$resid_stage,
                                                                                       df_new_spring$resid_stage_7_day_mois,
                                                                                       use = "complete.obs")
# Extract the correlation coefficient and p-value
all_stage_aphid_abundance_7_day_mean_soil_moisture_spring_correlation_coefficient <- all_stage_aphid_abundance_7_day_mean_soil_moisture_spring_correlation_test$estimate
all_stage_aphid_abundance_7_day_mean_soil_moisture_spring_p_value <- all_stage_aphid_abundance_7_day_mean_soil_moisture_spring_correlation_test$p.value


#daily mean soil moisture
all_stage_aphid_abundance_7_day_mean_soil_moisture_fall_correlation_test <- cor.test(df_new_fall$resid_stage,
                                                                                     df_new_fall$resid_stage_7_day_mois,
                                                                                     use = "complete.obs")
# Extract the correlation coefficient and p-value
all_stage_aphid_abundance_7_day_mean_soil_moisture_fall_correlation_coefficient <- all_stage_aphid_abundance_7_day_mean_soil_moisture_fall_correlation_test$estimate
all_stage_aphid_abundance_7_day_mean_soil_moisture_fall_p_value <- all_stage_aphid_abundance_7_day_mean_soil_moisture_fall_correlation_test$p.value


library(scales)  # for alpha()
# scale_color_manual(values = c("2024 spring" = "#006600", "2024 fall" = "#867052"))
# Scatter plot with a linear trend line
plot_all_stage_7_day_mean_soil_moisture_aphid_abundance_season <- 
  ggplot(df_new,
         aes(x = resid_stage_7_day_mois,
             y = resid_stage,
             color = `growing season`)) + 
  geom_point(aes(fill = after_scale(scales::alpha(color, 0.30))),
             stroke = 0.2, shape = 21, size = 0.75) + 
  # 設定回歸線顏色，根據不同的 'growing season' 來顯示不同顏色
  geom_smooth(data = df_new %>% filter(`growing season` == "2024 spring"), method = "lm", se = TRUE,
              color = "#006600",  # 深綠色回歸線
              fill = "#66CC66",   # 淺綠色信賴區間
              alpha = 0.4, size = 0.5) +   # 信賴區間透明度
  geom_smooth(data = df_new %>% filter(`growing season` == "2024 fall"), method = "lm", se = TRUE,
              color = "#8B4513",  # 紅棕色回歸線
              fill = "#C4B7A6",   # 淺咖啡信賴區間
              alpha = 0.4, size = 0.5) +   # 信賴區間透明度
  labs(title = "",
       x = "Prev. 7-D Mois. (Stage)",
       y = expression( ~ log[10](Aphids + 1)/m^2 ~ "(Stage)")) +
  theme_classic(base_size = 8, base_family = "sans") +
  theme(plot.title=element_text(size=8, face="bold",hjust = 0.5),
        axis.title=element_text(size=8),
        axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x = element_text(size =6),
        axis.text.y = element_text(size = 6),
        legend.position= "none",        # 顯示圖例
        legend.key.size = unit(12,"points"),
        legend.title = element_blank()) +
  coord_cartesian(clip = "off") +
  # 資料點顏色設置，確保資料點顏色正確
  scale_color_manual(values = c("2024 spring" = "#006600",  # 深綠
                                "2024 fall"   = "#867052")) +  # 深咖啡
  # 區間顏色設置，確保資料的顏色不會被覆蓋
  scale_fill_manual(values = c("2024 spring" = "#66CC66",  # 淺綠
                               "2024 fall"   = "#C4B7A6")) +  # 淺咖啡
  annotate("text", x = -12.5, y = -2.4,
           label = paste0("r = ",
                          round(all_stage_aphid_abundance_7_day_mean_soil_moisture_spring_correlation_coefficient, 3),
                          ", p < 0.05 "),
           hjust = 0, vjust = 0, size = 2.5, color = "#006600", fontface = "bold") +
  annotate("text", x = -12.5, y = -2.8,
           label = paste0("r = ",
                          round(all_stage_aphid_abundance_7_day_mean_soil_moisture_fall_correlation_coefficient, 3),
                          ", p < 0.05 "),
           hjust = 0, vjust = 0, size = 2.5, color = "#8B4513", fontface = "bold") +
  scale_x_continuous(limits = c(-15, 15)) +
  scale_y_continuous(limits = c(-3.0, 3.0))

ggsave("plot_MAP_aphid_soil_moisture_EC_2025_09_22_color_MAP.png", plot = plot_aphid_soil_moisture_EC, 
       width = 18.5, height = 16, units = "cm", dpi = 300)




# spring

#daily mean soil EC
all_stage_aphid_abundance_daily_mean_soil_conductivity_correlation_test <- cor.test(df_new$resid_stage,
                                                                                    df_new$resid_stage_daily_ec,
                                                                                    use = "complete.obs")
# Extract the correlation coefficient and p-value
all_stage_aphid_abundance_daily_mean_soil_conductivity_correlation_coefficient <- all_stage_aphid_abundance_daily_mean_soil_conductivity_correlation_test$estimate
all_stage_aphid_abundance_daily_mean_soil_conductivity_p_value <- all_stage_aphid_abundance_daily_mean_soil_conductivity_correlation_test$p.value

max(df_new$resid_stage_daily_ec)
min(df_new$resid_stage_daily_ec)

plot_all_stage_daily_mean_soil_conductivity_aphid_abundance <- 
  ggplot(df_new,
         aes(x = resid_stage_daily_ec,
             y = resid_stage,
             color = `growing season`)) + 
  geom_point(aes(fill = after_scale(scales::alpha(color, 0.30))),
             stroke = 0.2, shape = 21, size = 0.75) +
  geom_smooth(method = "lm", se = TRUE,col = "blue", aes(group = 1),
              alpha = 0.3, size = 0.5) +   # 信賴區間透明度
  
  labs(title = "",
       x = "Daily EC (Stage)",
       y = expression( ~ log[10](Aphids + 1)/m^2 ~ "(Stage)" )) +
  theme_classic(base_size = 8, base_family = "sans") +
  theme(plot.title=element_text(size=8, face="bold",hjust = 0.5),
        axis.title=element_text(size=8),
        axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x = element_text(size =6),
        axis.text.y = element_text(size = 6),
        legend.position= "none",        # 顯示圖例
        legend.key.size = unit(12,"points"),
        legend.title = element_blank()) +
  coord_cartesian(clip = "off") +
  scale_color_manual(values = c("2024 spring" = "#006600",  # 深綠
                                "2024 fall"   = "#867052")) +  # 深咖啡
  
  annotate("text", x = -0.05, y = -2.6,
           label = paste0("r = ",
                          round(all_stage_aphid_abundance_daily_mean_soil_conductivity_correlation_coefficient, 3),
                          ", p = ",
                          signif(all_stage_aphid_abundance_daily_mean_soil_conductivity_p_value, 3)),
           hjust = 0, vjust = 0, size = 2.5, color = "black")  +
  scale_x_continuous(limits = c(-0.15, 1.0)) +
  scale_y_continuous(limits = c(-3.0, 3.0))

#daily mean soil EC
all_stage_aphid_abundance_daily_mean_soil_conductivity_spring_correlation_test <- cor.test(df_new_spring$resid_stage,
                                                                                           df_new_spring$resid_stage_daily_ec,
                                                                                           use = "complete.obs")
# Extract the correlation coefficient and p-value
all_stage_aphid_abundance_daily_mean_soil_conductivity_spring_correlation_coefficient <- all_stage_aphid_abundance_daily_mean_soil_conductivity_spring_correlation_test$estimate
all_stage_aphid_abundance_daily_mean_soil_conductivity_spring_p_value <- all_stage_aphid_abundance_daily_mean_soil_conductivity_spring_correlation_test$p.value


#daily mean soil moisture
all_stage_aphid_abundance_daily_mean_soil_conductivity_fall_correlation_test <- cor.test(df_new_fall$resid_stage,
                                                                                         df_new_fall$resid_stage_daily_ec,
                                                                                         use = "complete.obs")
# Extract the correlation coefficient and p-value
all_stage_aphid_abundance_daily_mean_soil_conductivity_fall_correlation_coefficient <- all_stage_aphid_abundance_daily_mean_soil_conductivity_fall_correlation_test$estimate
all_stage_aphid_abundance_daily_mean_soil_conductivity_fall_p_value <- all_stage_aphid_abundance_daily_mean_soil_conductivity_fall_correlation_test$p.value



# Scatter plot with a linear trend line
plot_all_stage_daily_mean_soil_conductivity_aphid_abundance_season <- 
  ggplot(df_new,
         aes(x = resid_stage_daily_ec,
             y = resid_stage,
             color = `growing season`)) + 
  geom_point(aes(fill = after_scale(scales::alpha(color, 0.30))),
             stroke = 0.2, shape = 21, size = 0.75) + 
  # 設定回歸線顏色，根據不同的 'growing season' 來顯示不同顏色
  geom_smooth(data = df_new %>% filter(`growing season` == "2024 spring"), method = "lm", se = TRUE,
              color = "#006600",  # 深綠色回歸線
              fill = "#66CC66",   # 淺綠色信賴區間
              alpha = 0.4, size = 0.5) +   # 信賴區間透明度
  geom_smooth(data = df_new %>% filter(`growing season` == "2024 fall"), method = "lm", se = TRUE,
              color = "#8B4513",  # 紅棕色回歸線
              fill = "#C4B7A6",   # 淺咖啡信賴區間
              alpha = 0.4, size = 0.5) +   # 信賴區間透明度
  labs(title = "",
       x = "Daily EC (Stage)",
       y = expression( ~ log[10](Aphids + 1)/m^2 ~ "(Stage)")) +
  theme_classic(base_size = 8, base_family = "sans") +
  theme(plot.title=element_text(size=8, face="bold",hjust = 0.5),
        axis.title=element_text(size=8),
        axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x = element_text(size =6),
        axis.text.y = element_text(size = 6),
        legend.position= "none",        # 顯示圖例
        legend.key.size = unit(12,"points"),
        legend.title = element_blank()) +
  coord_cartesian(clip = "off") +
  # 資料點顏色設置，確保資料點顏色正確
  scale_color_manual(values = c("2024 spring" = "#006600",  # 深綠
                                "2024 fall"   = "#867052")) +  # 深咖啡
  # 區間顏色設置，確保資料的顏色不會被覆蓋
  scale_fill_manual(values = c("2024 spring" = "#66CC66",  # 淺綠
                               "2024 fall"   = "#C4B7A6")) +  # 淺咖啡
  annotate("text", x = -0.05, y = -2.4,
           label = paste0("r = ",
                          round(all_stage_aphid_abundance_daily_mean_soil_conductivity_spring_correlation_coefficient, 3),
                          ", p = ",
                          signif(all_stage_aphid_abundance_daily_mean_soil_conductivity_spring_p_value, 3)),
           hjust = 0, vjust = 0, size = 2.5, color = "#006600") +
  annotate("text", x = -0.05, y = -2.8,
           label = paste0("r = ",
                          round(all_stage_aphid_abundance_daily_mean_soil_conductivity_fall_correlation_coefficient, 3),
                          ", p = ",
                          signif(all_stage_aphid_abundance_daily_mean_soil_conductivity_fall_p_value, 3)),
           hjust = 0, vjust = 0, size = 2.5, color = "#8B4513") +
  scale_x_continuous(limits = c(-0.15, 1.0)) +
  scale_y_continuous(limits = c(-3.0, 3.0))



# 1-day mean soil conductivity
#daily mean soil moisture
all_stage_aphid_abundance_1_day_mean_soil_conductivity_correlation_test <- cor.test(df_new$resid_stage,
                                                                                    df_new$resid_stage_1_day_ec,
                                                                                    use = "complete.obs")
# Extract the correlation coefficient and p-value
all_stage_aphid_abundance_1_day_mean_soil_conductivity_correlation_coefficient <- all_stage_aphid_abundance_1_day_mean_soil_conductivity_correlation_test$estimate
all_stage_aphid_abundance_1_day_mean_soil_conductivity_p_value <- all_stage_aphid_abundance_1_day_mean_soil_conductivity_correlation_test$p.value

max(df_new$resid_stage_1_day_ec)
min(df_new$resid_stage_1_day_ec)

plot_all_stage_1_day_mean_soil_conductivity_aphid_abundance <- 
  ggplot(df_new,
         aes(x = resid_stage_1_day_ec,
             y = resid_stage,
             color = `growing season`)) + 
  geom_point(aes(fill = after_scale(scales::alpha(color, 0.30))),
             stroke = 0.2, shape = 21, size = 0.75) +
  geom_smooth(method = "lm", se = TRUE,col = "blue", aes(group = 1),
              alpha = 0.3, size = 0.5) +   # 信賴區間透明度
  
  labs(title = "",
       x = "Prev. 1-D EC (Stage)",
       y = expression( ~ log[10](Aphids + 1)/m^2 ~ "(Stage)" )) +
  theme_classic(base_size = 8, base_family = "sans") +
  theme(plot.title=element_text(size=8, face="bold",hjust = 0.5),
        axis.title=element_text(size=8),
        axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x = element_text(size =6),
        axis.text.y = element_text(size = 6),
        legend.position= "none",        # 顯示圖例
        legend.key.size = unit(12,"points"),
        legend.title = element_blank()) +
  coord_cartesian(clip = "off") +
  scale_color_manual(values = c("2024 spring" = "#006600",  # 深綠
                                "2024 fall"   = "#867052")) +  # 深咖啡
  
  annotate("text", x = -0.1, y = -2.6,
           label = paste0("r = ",
                          round(all_stage_aphid_abundance_1_day_mean_soil_conductivity_correlation_coefficient, 3),
                          ", p = ",
                          signif(all_stage_aphid_abundance_1_day_mean_soil_conductivity_p_value, 3)),
           hjust = 0, vjust = 0, size = 2.5, color = "black")  +
  scale_x_continuous(limits = c(-0.15, 0.5)) +
  scale_y_continuous(limits = c(-3.0, 3.0))

#daily mean soil EC
all_stage_aphid_abundance_1_day_mean_soil_conductivity_spring_correlation_test <- cor.test(df_new_spring$resid_stage,
                                                                                           df_new_spring$resid_stage_1_day_ec,
                                                                                           use = "complete.obs")
# Extract the correlation coefficient and p-value
all_stage_aphid_abundance_1_day_mean_soil_conductivity_spring_correlation_coefficient <- all_stage_aphid_abundance_1_day_mean_soil_conductivity_spring_correlation_test$estimate
all_stage_aphid_abundance_1_day_mean_soil_conductivity_spring_p_value <- all_stage_aphid_abundance_1_day_mean_soil_conductivity_spring_correlation_test$p.value


#daily mean soil moisture
all_stage_aphid_abundance_1_day_mean_soil_conductivity_fall_correlation_test <- cor.test(df_new_fall$resid_stage,
                                                                                         df_new_fall$resid_stage_1_day_ec,
                                                                                         use = "complete.obs")
# Extract the correlation coefficient and p-value
all_stage_aphid_abundance_1_day_mean_soil_conductivity_fall_correlation_coefficient <- all_stage_aphid_abundance_1_day_mean_soil_conductivity_fall_correlation_test$estimate
all_stage_aphid_abundance_1_day_mean_soil_conductivity_fall_p_value <- all_stage_aphid_abundance_1_day_mean_soil_conductivity_fall_correlation_test$p.value



# Scatter plot with a linear trend line
plot_all_stage_1_day_mean_soil_conductivity_aphid_abundance_season <- 
  ggplot(df_new,
         aes(x = resid_stage_1_day_ec,
             y = resid_stage,
             color = `growing season`)) + 
  geom_point(aes(fill = after_scale(scales::alpha(color, 0.30))),
             stroke = 0.2, shape = 21, size = 0.75) + 
  # 設定回歸線顏色，根據不同的 'growing season' 來顯示不同顏色
  geom_smooth(data = df_new %>% filter(`growing season` == "2024 spring"), method = "lm", se = TRUE,
              color = "#006600",  # 深綠色回歸線
              fill = "#66CC66",   # 淺綠色信賴區間
              alpha = 0.4, size = 0.5) +   # 信賴區間透明度
  geom_smooth(data = df_new %>% filter(`growing season` == "2024 fall"), method = "lm", se = TRUE,
              color = "#8B4513",  # 紅棕色回歸線
              fill = "#C4B7A6",   # 淺咖啡信賴區間
              alpha = 0.4, size = 0.5) +   # 信賴區間透明度
  labs(title = "",
       x = "Prev. 1-D EC (Stage)",
       y = expression( ~ log[10](Aphids + 1)/m^2 ~ "(Stage)")) +
  theme_classic(base_size = 8, base_family = "sans") +
  theme(plot.title=element_text(size=8, face="bold",hjust = 0.5),
        axis.title=element_text(size=8),
        axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x = element_text(size =6),
        axis.text.y = element_text(size = 6),
        legend.position= "none",        # 顯示圖例
        legend.key.size = unit(12,"points"),
        legend.title = element_blank()) +
  coord_cartesian(clip = "off") +
  # 資料點顏色設置，確保資料點顏色正確
  scale_color_manual(values = c("2024 spring" = "#006600",  # 深綠
                                "2024 fall"   = "#867052")) +  # 深咖啡
  # 區間顏色設置，確保資料的顏色不會被覆蓋
  scale_fill_manual(values = c("2024 spring" = "#66CC66",  # 淺綠
                               "2024 fall"   = "#C4B7A6")) +  # 淺咖啡
  annotate("text", x = -0.1, y = -2.4,
           label = paste0("r = ",
                          round(all_stage_aphid_abundance_1_day_mean_soil_conductivity_spring_correlation_coefficient, 3),
                          ", p = ",
                          signif(all_stage_aphid_abundance_1_day_mean_soil_conductivity_spring_p_value, 3)),
           hjust = 0, vjust = 0, size = 2.5, color = "#006600") +
  annotate("text", x = -0.1, y = -2.8,
           label = paste0("r = ",
                          round(all_stage_aphid_abundance_1_day_mean_soil_conductivity_fall_correlation_coefficient, 3),
                          ", p = ",
                          signif(all_stage_aphid_abundance_1_day_mean_soil_conductivity_fall_p_value, 3)),
           hjust = 0, vjust = 0, size = 2.5, color = "#8B4513") +
  scale_x_continuous(limits = c(-0.15, 0.5)) +
  scale_y_continuous(limits = c(-3.0, 3.0))

# 3-day mean soil conductivity
all_stage_aphid_abundance_3_day_mean_soil_conductivity_correlation_test <- cor.test(df_new$resid_stage,
                                                                                    df_new$resid_stage_3_day_ec,
                                                                                    use = "complete.obs")
# Extract the correlation coefficient and p-value
all_stage_aphid_abundance_3_day_mean_soil_conductivity_correlation_coefficient <- all_stage_aphid_abundance_3_day_mean_soil_conductivity_correlation_test$estimate
all_stage_aphid_abundance_3_day_mean_soil_conductivity_p_value <- all_stage_aphid_abundance_3_day_mean_soil_conductivity_correlation_test$p.value


max(df_new$resid_stage_3_day_ec)
min(df_new$resid_stage_3_day_ec)

plot_all_stage_3_day_mean_soil_conductivity_aphid_abundance <- 
  ggplot(df_new,
         aes(x = resid_stage_3_day_ec,
             y = resid_stage,
             color = `growing season`)) + 
  geom_point(aes(fill = after_scale(scales::alpha(color, 0.30))),
             stroke = 0.2, shape = 21, size = 0.75) +
  geom_smooth(method = "lm", se = TRUE,col = "blue", aes(group = 1),
              alpha = 0.3, size = 0.5) +   # 信賴區間透明度
  
  labs(title = "",
       x = "Prev. 3-D EC (Stage)",
       y = expression( ~ log[10](Aphids + 1)/m^2 ~ "(Stage)" )) +
  theme_classic(base_size = 8, base_family = "sans") +
  theme(plot.title=element_text(size=8, face="bold",hjust = 0.5),
        axis.title=element_text(size=8),
        axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x = element_text(size =6),
        axis.text.y = element_text(size = 6),
        legend.position= "none",        # 顯示圖例
        legend.key.size = unit(12,"points"),
        legend.title = element_blank()) +
  coord_cartesian(clip = "off") +
  scale_color_manual(values = c("2024 spring" = "#006600",  # 深綠
                                "2024 fall"   = "#867052")) +  # 深咖啡
  
  annotate("text", x = -0.1, y = -2.6,
           label = paste0("r = ",
                          round(all_stage_aphid_abundance_3_day_mean_soil_conductivity_correlation_coefficient, 3),
                          ", p = ",
                          signif(all_stage_aphid_abundance_3_day_mean_soil_conductivity_p_value, 3)),
           hjust = 0, vjust = 0, size = 2.5, color = "black")  +
  scale_x_continuous(limits = c(-0.15, 0.6)) +
  scale_y_continuous(limits = c(-3.0, 3.0))

#daily mean soil EC
all_stage_aphid_abundance_3_day_mean_soil_conductivity_spring_correlation_test <- cor.test(df_new_spring$resid_stage,
                                                                                           df_new_spring$resid_stage_3_day_ec,
                                                                                           use = "complete.obs")
# Extract the correlation coefficient and p-value
all_stage_aphid_abundance_3_day_mean_soil_conductivity_spring_correlation_coefficient <- all_stage_aphid_abundance_3_day_mean_soil_conductivity_spring_correlation_test$estimate
all_stage_aphid_abundance_3_day_mean_soil_conductivity_spring_p_value <- all_stage_aphid_abundance_3_day_mean_soil_conductivity_spring_correlation_test$p.value


#daily mean soil moisture
all_stage_aphid_abundance_3_day_mean_soil_conductivity_fall_correlation_test <- cor.test(df_new_fall$resid_stage,
                                                                                         df_new_fall$resid_stage_3_day_ec,
                                                                                         use = "complete.obs")
# Extract the correlation coefficient and p-value
all_stage_aphid_abundance_3_day_mean_soil_conductivity_fall_correlation_coefficient <- all_stage_aphid_abundance_3_day_mean_soil_conductivity_fall_correlation_test$estimate
all_stage_aphid_abundance_3_day_mean_soil_conductivity_fall_p_value <- all_stage_aphid_abundance_3_day_mean_soil_conductivity_fall_correlation_test$p.value



# Scatter plot with a linear trend line
plot_all_stage_3_day_mean_soil_conductivity_aphid_abundance_season <- 
  ggplot(df_new,
         aes(x = resid_stage_3_day_ec,
             y = resid_stage,
             color = `growing season`)) + 
  geom_point(aes(fill = after_scale(scales::alpha(color, 0.30))),
             stroke = 0.2, shape = 21, size = 0.75) + 
  # 設定回歸線顏色，根據不同的 'growing season' 來顯示不同顏色
  geom_smooth(data = df_new %>% filter(`growing season` == "2024 spring"), method = "lm", se = TRUE,
              color = "#006600",  # 深綠色回歸線
              fill = "#66CC66",   # 淺綠色信賴區間
              alpha = 0.4, size = 0.5) +   # 信賴區間透明度
  geom_smooth(data = df_new %>% filter(`growing season` == "2024 fall"), method = "lm", se = TRUE,
              color = "#8B4513",  # 紅棕色回歸線
              fill = "#C4B7A6",   # 淺咖啡信賴區間
              alpha = 0.4, size = 0.5) +   # 信賴區間透明度
  labs(title = "",
       x = "Prev. 3-D EC (Stage)",
       y = expression( ~ log[10](Aphids + 1)/m^2 ~ "(Stage)")) +
  theme_classic(base_size = 8, base_family = "sans") +
  theme(plot.title=element_text(size=8, face="bold",hjust = 0.5),
        axis.title=element_text(size=8),
        axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x = element_text(size =6),
        axis.text.y = element_text(size = 6),
        legend.position= "none",        # 顯示圖例
        legend.key.size = unit(12,"points"),
        legend.title = element_blank()) +
  coord_cartesian(clip = "off") +
  # 資料點顏色設置，確保資料點顏色正確
  scale_color_manual(values = c("2024 spring" = "#006600",  # 深綠
                                "2024 fall"   = "#867052")) +  # 深咖啡
  # 區間顏色設置，確保資料的顏色不會被覆蓋
  scale_fill_manual(values = c("2024 spring" = "#66CC66",  # 淺綠
                               "2024 fall"   = "#C4B7A6")) +  # 淺咖啡
  annotate("text", x = -0.1, y = -2.4,
           label = paste0("r = ",
                          round(all_stage_aphid_abundance_3_day_mean_soil_conductivity_spring_correlation_coefficient, 3),
                          ", p = ",
                          signif(all_stage_aphid_abundance_3_day_mean_soil_conductivity_spring_p_value, 3)),
           hjust = 0, vjust = 0, size = 2.5, color = "#006600") +
  annotate("text", x = -0.1, y = -2.8,
           label = paste0("r = ",
                          round(all_stage_aphid_abundance_3_day_mean_soil_conductivity_fall_correlation_coefficient, 3),
                          ", p = ",
                          signif(all_stage_aphid_abundance_3_day_mean_soil_conductivity_fall_p_value, 3)),
           hjust = 0, vjust = 0, size = 2.5, color = "#8B4513") +
  scale_x_continuous(limits = c(-0.15, 0.6)) +
  scale_y_continuous(limits = c(-3.0, 3.0))


# 5-day mean soil conductivity

all_stage_aphid_abundance_5_day_mean_soil_conductivity_correlation_test <- cor.test(df_new$resid_stage,
                                                                                    df_new$resid_stage_5_day_ec,
                                                                                    use = "complete.obs")
# Extract the correlation coefficient and p-value
all_stage_aphid_abundance_5_day_mean_soil_conductivity_correlation_coefficient <- all_stage_aphid_abundance_5_day_mean_soil_conductivity_correlation_test$estimate
all_stage_aphid_abundance_5_day_mean_soil_conductivity_p_value <- all_stage_aphid_abundance_5_day_mean_soil_conductivity_correlation_test$p.value

max(df_new$resid_stage_5_day_ec)
min(df_new$resid_stage_5_day_ec)

plot_all_stage_5_day_mean_soil_conductivity_aphid_abundance <- 
  ggplot(df_new,
         aes(x = resid_stage_5_day_ec,
             y = resid_stage,
             color = `growing season`)) + 
  geom_point(aes(fill = after_scale(scales::alpha(color, 0.30))),
             stroke = 0.2, shape = 21, size = 0.75) +
  geom_smooth(method = "lm", se = TRUE,col = "blue", aes(group = 1),
              alpha = 0.3, size = 0.5) +   # 信賴區間透明度
  
  labs(title = "",
       x = "Prev. 5-D EC (Stage)",
       y = expression( ~ log[10](Aphids + 1)/m^2 ~ "(Stage)" )) +
  theme_classic(base_size = 8, base_family = "sans") +
  theme(plot.title=element_text(size=8, face="bold",hjust = 0.5),
        axis.title=element_text(size=8),
        axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x = element_text(size =6),
        axis.text.y = element_text(size = 6),
        legend.position= "none",        # 顯示圖例
        legend.key.size = unit(12,"points"),
        legend.title = element_blank()) +
  coord_cartesian(clip = "off") +
  scale_color_manual(values = c("2024 spring" = "#006600",  # 深綠
                                "2024 fall"   = "#867052")) +  # 深咖啡
  
  annotate("text", x = -0.05, y = -2.6,
           label = paste0("r = ",
                          round(all_stage_aphid_abundance_5_day_mean_soil_conductivity_correlation_coefficient, 3),
                          ", p = ",
                          signif(all_stage_aphid_abundance_5_day_mean_soil_conductivity_p_value, 3)),
           hjust = 0, vjust = 0, size = 2.5, color = "black")  +
  scale_x_continuous(limits = c(-0.15, 1.5)) +
  scale_y_continuous(limits = c(-3.0, 3.0))

#daily mean soil EC
all_stage_aphid_abundance_5_day_mean_soil_conductivity_spring_correlation_test <- cor.test(df_new_spring$resid_stage,
                                                                                           df_new_spring$resid_stage_5_day_ec,
                                                                                           use = "complete.obs")
# Extract the correlation coefficient and p-value
all_stage_aphid_abundance_5_day_mean_soil_conductivity_spring_correlation_coefficient <- all_stage_aphid_abundance_5_day_mean_soil_conductivity_spring_correlation_test$estimate
all_stage_aphid_abundance_5_day_mean_soil_conductivity_spring_p_value <- all_stage_aphid_abundance_5_day_mean_soil_conductivity_spring_correlation_test$p.value


#daily mean soil moisture
all_stage_aphid_abundance_5_day_mean_soil_conductivity_fall_correlation_test <- cor.test(df_new_fall$resid_stage,
                                                                                         df_new_fall$resid_stage_5_day_ec,
                                                                                         use = "complete.obs")
# Extract the correlation coefficient and p-value
all_stage_aphid_abundance_5_day_mean_soil_conductivity_fall_correlation_coefficient <- all_stage_aphid_abundance_5_day_mean_soil_conductivity_fall_correlation_test$estimate
all_stage_aphid_abundance_5_day_mean_soil_conductivity_fall_p_value <- all_stage_aphid_abundance_5_day_mean_soil_conductivity_fall_correlation_test$p.value



# Scatter plot with a linear trend line
plot_all_stage_5_day_mean_soil_conductivity_aphid_abundance_season <- 
  ggplot(df_new,
         aes(x = resid_stage_5_day_ec,
             y = resid_stage,
             color = `growing season`)) + 
  geom_point(aes(fill = after_scale(scales::alpha(color, 0.30))),
             stroke = 0.2, shape = 21, size = 0.75) + 
  # 設定回歸線顏色，根據不同的 'growing season' 來顯示不同顏色
  geom_smooth(data = df_new %>% filter(`growing season` == "2024 spring"), method = "lm", se = TRUE,
              color = "#006600",  # 深綠色回歸線
              fill = "#66CC66",   # 淺綠色信賴區間
              alpha = 0.4, size = 0.5) +   # 信賴區間透明度
  geom_smooth(data = df_new %>% filter(`growing season` == "2024 fall"), method = "lm", se = TRUE,
              color = "#8B4513",  # 紅棕色回歸線
              fill = "#C4B7A6",   # 淺咖啡信賴區間
              alpha = 0.4, size = 0.5) +   # 信賴區間透明度
  labs(title = "",
       x = "Prev. 5-D EC (Stage)",
       y = expression( ~ log[10](Aphids + 1)/m^2 ~ "(Stage)")) +
  theme_classic(base_size = 8, base_family = "sans") +
  theme(plot.title=element_text(size=8, face="bold",hjust = 0.5),
        axis.title=element_text(size=8),
        axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x = element_text(size =6),
        axis.text.y = element_text(size = 6),
        legend.position= "none",        # 顯示圖例
        legend.key.size = unit(12,"points"),
        legend.title = element_blank()) +
  coord_cartesian(clip = "off") +
  # 資料點顏色設置，確保資料點顏色正確
  scale_color_manual(values = c("2024 spring" = "#006600",  # 深綠
                                "2024 fall"   = "#867052")) +  # 深咖啡
  # 區間顏色設置，確保資料的顏色不會被覆蓋
  scale_fill_manual(values = c("2024 spring" = "#66CC66",  # 淺綠
                               "2024 fall"   = "#C4B7A6")) +  # 淺咖啡
  annotate("text", x = -0.05, y = -2.4,
           label = paste0("r = ",
                          round(all_stage_aphid_abundance_5_day_mean_soil_conductivity_spring_correlation_coefficient, 3),
                          ", p = ",
                          signif(all_stage_aphid_abundance_5_day_mean_soil_conductivity_spring_p_value, 3)),
           hjust = 0, vjust = 0, size = 2.5, color = "#006600") +
  annotate("text", x = -0.05, y = -2.8,
           label = paste0("r = ",
                          round(all_stage_aphid_abundance_5_day_mean_soil_conductivity_fall_correlation_coefficient, 3),
                          ", p = ",
                          signif(all_stage_aphid_abundance_5_day_mean_soil_conductivity_fall_p_value, 3)),
           hjust = 0, vjust = 0, size = 2.5, color = "#8B4513") +
  scale_x_continuous(limits = c(-0.15, 1.5)) +
  scale_y_continuous(limits = c(-3.0, 3.0))

# 7-day mean soil conductivity
all_stage_aphid_abundance_7_day_mean_soil_conductivity_correlation_test <- cor.test(df_new$resid_stage,
                                                                                    df_new$resid_stage_7_day_ec,
                                                                                    use = "complete.obs")
# Extract the correlation coefficient and p-value
all_stage_aphid_abundance_7_day_mean_soil_conductivity_correlation_coefficient <- all_stage_aphid_abundance_7_day_mean_soil_conductivity_correlation_test$estimate
all_stage_aphid_abundance_7_day_mean_soil_conductivity_p_value <- all_stage_aphid_abundance_7_day_mean_soil_conductivity_correlation_test$p.value

max(df_new$resid_stage_7_day_ec)
min(df_new$resid_stage_7_day_ec)

plot_all_stage_7_day_mean_soil_conductivity_aphid_abundance <- 
  ggplot(df_new,
         aes(x = resid_stage_7_day_ec,
             y = resid_stage,
             color = `growing season`)) + 
  geom_point(aes(fill = after_scale(scales::alpha(color, 0.30))),
             stroke = 0.2, shape = 21, size = 0.75) +
  geom_smooth(method = "lm", se = TRUE,col = "blue", aes(group = 1),
              alpha = 0.3, size = 0.5) +   # 信賴區間透明度
  
  labs(title = "",
       x = "Prev. 7-D EC (Stage)",
       y = expression( ~ log[10](Aphids + 1)/m^2 ~ "(Stage)" )) +
  theme_classic(base_size = 8, base_family = "sans") +
  theme(plot.title=element_text(size=8, face="bold",hjust = 0.5),
        axis.title=element_text(size=8),
        axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x = element_text(size =6),
        axis.text.y = element_text(size = 6),
        legend.position= "none",        # 顯示圖例
        legend.key.size = unit(12,"points"),
        legend.title = element_blank()) +
  coord_cartesian(clip = "off") +
  scale_color_manual(values = c("2024 spring" = "#006600",  # 深綠
                                "2024 fall"   = "#867052")) +  # 深咖啡
  
  annotate("text", x = -0.05, y = -2.6,
           label = paste0("r = ",
                          round(all_stage_aphid_abundance_7_day_mean_soil_conductivity_correlation_coefficient, 3),
                          ", p = ",
                          signif(all_stage_aphid_abundance_7_day_mean_soil_conductivity_p_value, 3)),
           hjust = 0, vjust = 0, size = 2.5, color = "black")  +
  scale_x_continuous(limits = c(-0.13, 1.0)) +
  scale_y_continuous(limits = c(-3.0, 3.0))

#daily mean soil EC
all_stage_aphid_abundance_7_day_mean_soil_conductivity_spring_correlation_test <- cor.test(df_new_spring$resid_stage,
                                                                                           df_new_spring$resid_stage_7_day_ec,
                                                                                           use = "complete.obs")
# Extract the correlation coefficient and p-value
all_stage_aphid_abundance_7_day_mean_soil_conductivity_spring_correlation_coefficient <- all_stage_aphid_abundance_7_day_mean_soil_conductivity_spring_correlation_test$estimate
all_stage_aphid_abundance_7_day_mean_soil_conductivity_spring_p_value <- all_stage_aphid_abundance_7_day_mean_soil_conductivity_spring_correlation_test$p.value


#daily mean soil moisture
all_stage_aphid_abundance_7_day_mean_soil_conductivity_fall_correlation_test <- cor.test(df_new_fall$resid_stage,
                                                                                         df_new_fall$resid_stage_7_day_ec,
                                                                                         use = "complete.obs")
# Extract the correlation coefficient and p-value
all_stage_aphid_abundance_7_day_mean_soil_conductivity_fall_correlation_coefficient <- all_stage_aphid_abundance_7_day_mean_soil_conductivity_fall_correlation_test$estimate
all_stage_aphid_abundance_7_day_mean_soil_conductivity_fall_p_value <- all_stage_aphid_abundance_7_day_mean_soil_conductivity_fall_correlation_test$p.value



# Scatter plot with a linear trend line
plot_all_stage_7_day_mean_soil_conductivity_aphid_abundance_season <- 
  ggplot(df_new,
         aes(x = resid_stage_7_day_ec,
             y = resid_stage,
             color = `growing season`)) + 
  geom_point(aes(fill = after_scale(scales::alpha(color, 0.30))),
             stroke = 0.2, shape = 21, size = 0.75) + 
  # 設定回歸線顏色，根據不同的 'growing season' 來顯示不同顏色
  geom_smooth(data = df_new %>% filter(`growing season` == "2024 spring"), method = "lm", se = TRUE,
              color = "#006600",  # 深綠色回歸線
              fill = "#66CC66",   # 淺綠色信賴區間
              alpha = 0.3, size = 0.5) +   # 信賴區間透明度
  geom_smooth(data = df_new %>% filter(`growing season` == "2024 fall"), method = "lm", se = TRUE,
              color = "#8B4513",  # 紅棕色回歸線
              fill = "#C4B7A6",   # 淺咖啡信賴區間
              alpha = 0.3, size = 0.5) +   # 信賴區間透明度
  labs(title = "",
       x = "Prev. 7-D EC (Stage)",
       y = expression( ~ log[10](Aphids + 1)/m^2 ~ "(Stage)")) +
  theme_classic(base_size = 8, base_family = "sans") +
  theme(plot.title=element_text(size=8, face="bold",hjust = 0.5),
        axis.title=element_text(size=8),
        axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x = element_text(size =6),
        axis.text.y = element_text(size = 6),
        legend.position= "none",        # 顯示圖例
        legend.key.size = unit(12,"points"),
        legend.title = element_blank()) +
  coord_cartesian(clip = "off") +
  # 資料點顏色設置，確保資料點顏色正確
  scale_color_manual(values = c("2024 spring" = "#006600",  # 深綠
                                "2024 fall"   = "#867052")) +  # 深咖啡
  # 區間顏色設置，確保資料的顏色不會被覆蓋
  scale_fill_manual(values = c("2024 spring" = "#66CC66",  # 淺綠
                               "2024 fall"   = "#C4B7A6")) +  # 淺咖啡
  annotate("text", x = -0.05, y = -2.4,
           label = paste0("r = ",
                          round(all_stage_aphid_abundance_7_day_mean_soil_conductivity_spring_correlation_coefficient, 3),
                          ", p = ",
                          signif(all_stage_aphid_abundance_7_day_mean_soil_conductivity_spring_p_value, 3)),
           hjust = 0, vjust = 0, size = 2.5, color = "#006600") +
  annotate("text", x = -0.05, y = -2.8,
           label = paste0("r = ",
                          round(all_stage_aphid_abundance_7_day_mean_soil_conductivity_fall_correlation_coefficient, 3),
                          ", p = ",
                          signif(all_stage_aphid_abundance_7_day_mean_soil_conductivity_fall_p_value, 3)),
           hjust = 0, vjust = 0, size = 2.5, color = "#8B4513") +
  scale_x_continuous(limits = c(-0.13, 1.0)) +
  scale_y_continuous(limits = c(-3.0, 3.0))

#-----------------------------------------------------------------------------------------------------------------------
# MAP
all_MAP_aphid_abundance_daily_mean_soil_moisture_correlation_test <- cor.test(df_new$resid_MAP,
                                                                              df_new$resid_MAP_daily_mois,
                                                                              use = "complete.obs")
# Extract the correlation coefficient and p-value
all_MAP_aphid_abundance_daily_mean_soil_moisture_correlation_coefficient <- all_MAP_aphid_abundance_daily_mean_soil_moisture_correlation_test$estimate
all_MAP_aphid_abundance_daily_mean_soil_moisture_p_value <- all_MAP_aphid_abundance_daily_mean_soil_moisture_correlation_test$p.value

max(df_new$resid_MAP_daily_mois)
min(df_new$resid_MAP_daily_mois)

plot_all_MAP_daily_mean_soil_moisture_aphid_abundance <- 
  ggplot(df_new,
         aes(x = resid_MAP_daily_mois,
             y = resid_MAP,
             color = `growing season`)) + 
  geom_point(aes(fill = after_scale(scales::alpha(color, 0.30))),
             stroke = 0.2, shape = 21, size = 0.75) +
  geom_smooth(method = "lm", se = TRUE,col = "blue", aes(group = 1),
              alpha = 0.3, size = 0.5) +   # 信賴區間透明度
  
  labs(title = "",
       x = "Daily Mois. ~ MAP",
       y = expression( ~ log[10](Aphids + 1)/m^2 ~ " ~ MAP")) +
  theme_classic(base_size = 8, base_family = "sans") +
  theme(plot.title=element_text(size=8, face="bold",hjust = 0.5),
        axis.title=element_text(size=8),
        axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x = element_text(size =6),
        axis.text.y = element_text(size = 6),
        legend.position= "none",        # 顯示圖例
        legend.key.size = unit(12,"points"),
        legend.title = element_blank()) +
  coord_cartesian(clip = "off") +
  scale_color_manual(values = c("2024 spring" = "#006600",  # 深綠
                                "2024 fall"   = "#867052")) +  # 深咖啡
  
  annotate("text", x = -13, y = -2.6,
           label = paste0("r = ",
                          round(all_MAP_aphid_abundance_daily_mean_soil_moisture_correlation_coefficient, 3),
                          ", p < 0.001 "),
           hjust = 0, vjust = 0, size = 2.5, color = "red", fontface = "bold") +
  scale_x_continuous(limits = c(-16, 19)) +
  scale_y_continuous(limits = c(-3.0, 3.0))

#daily mean soil moisture
all_MAP_aphid_abundance_daily_mean_soil_moisture_spring_correlation_test <- cor.test(df_new_spring$resid_MAP,
                                                                                     df_new_spring$resid_MAP_daily_mois,
                                                                                     use = "complete.obs")
# Extract the correlation coefficient and p-value
all_MAP_aphid_abundance_daily_mean_soil_moisture_spring_correlation_coefficient <- all_MAP_aphid_abundance_daily_mean_soil_moisture_spring_correlation_test$estimate
all_MAP_aphid_abundance_daily_mean_soil_moisture_spring_p_value <- all_MAP_aphid_abundance_daily_mean_soil_moisture_spring_correlation_test$p.value


#daily mean soil moisture
all_MAP_aphid_abundance_daily_mean_soil_moisture_fall_correlation_test <- cor.test(df_new_fall$resid_MAP,
                                                                                   df_new_fall$resid_MAP_daily_mois,
                                                                                   use = "complete.obs")
# Extract the correlation coefficient and p-value
all_MAP_aphid_abundance_daily_mean_soil_moisture_fall_correlation_coefficient <- all_MAP_aphid_abundance_daily_mean_soil_moisture_fall_correlation_test$estimate
all_MAP_aphid_abundance_daily_mean_soil_moisture_fall_p_value <- all_MAP_aphid_abundance_daily_mean_soil_moisture_fall_correlation_test$p.value


library(scales)  # for alpha()
# scale_color_manual(values = c("2024 spring" = "#006600", "2024 fall" = "#867052"))
# Scatter plot with a linear trend line
plot_all_MAP_daily_mean_soil_moisture_aphid_abundance_season <- 
  ggplot(df_new,
         aes(x = resid_MAP_daily_mois,
             y = resid_MAP,
             color = `growing season`)) + 
  geom_point(aes(fill = after_scale(scales::alpha(color, 0.30))),
             stroke = 0.2, shape = 21, size = 0.75) + 
  # 設定回歸線顏色，根據不同的 'growing season' 來顯示不同顏色
  geom_smooth(data = df_new %>% filter(`growing season` == "2024 spring"), method = "lm", se = TRUE,
              color = "#006600",  # 深綠色回歸線
              fill = "#66CC66",   # 淺綠色信賴區間
              alpha = 0.4, size = 0.5) +   # 信賴區間透明度
  geom_smooth(data = df_new %>% filter(`growing season` == "2024 fall"), method = "lm", se = TRUE,
              color = "#8B4513",  # 紅棕色回歸線
              fill = "#C4B7A6",   # 淺咖啡信賴區間
              alpha = 0.4, size = 0.5) +   # 信賴區間透明度
  labs(title = "",
       x = "Daily Mois. ~ MAP",
       y = expression( ~ log[10](Aphids + 1)/m^2 ~ " ~ MAP")) +
  theme_classic(base_size = 8, base_family = "sans") +
  theme(plot.title=element_text(size=8, face="bold",hjust = 0.5),
        axis.title=element_text(size=8),
        axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x = element_text(size =6),
        axis.text.y = element_text(size = 6),
        legend.position= "none",        # 顯示圖例
        legend.key.size = unit(12,"points"),
        legend.title = element_blank()) +
  coord_cartesian(clip = "off") +
  # 資料點顏色設置，確保資料點顏色正確
  scale_color_manual(values = c("2024 spring" = "#006600",  # 深綠
                                "2024 fall"   = "#867052")) +  # 深咖啡
  # 區間顏色設置，確保資料的顏色不會被覆蓋
  scale_fill_manual(values = c("2024 spring" = "#66CC66",  # 淺綠
                               "2024 fall"   = "#C4B7A6")) +  # 淺咖啡
  annotate("text", x = -13, y = -2.4,
           label = paste0("r = ",
                          round(all_MAP_aphid_abundance_daily_mean_soil_moisture_spring_correlation_coefficient, 3),
                          ", p < 0.001 "),
           hjust = 0, vjust = 0, size = 2.5, color = "#006600", fontface = "bold") +
  annotate("text", x = -13, y = -2.8,
           label = paste0("r = ",
                          round(all_MAP_aphid_abundance_daily_mean_soil_moisture_fall_correlation_coefficient, 3),
                          ", p < 0.001 "),
           hjust = 0, vjust = 0, size = 2.5, color = "#8B4513", fontface = "bold") +
  scale_x_continuous(limits = c(-16, 19)) +
  scale_y_continuous(limits = c(-3.0, 3.0))


# previous 1-day

all_MAP_aphid_abundance_1_day_mean_soil_moisture_correlation_test <- cor.test(df_new$resid_MAP,
                                                                              df_new$resid_MAP_1_day_mois,
                                                                              use = "complete.obs")
# Extract the correlation coefficient and p-value
all_MAP_aphid_abundance_1_day_mean_soil_moisture_correlation_coefficient <- all_MAP_aphid_abundance_1_day_mean_soil_moisture_correlation_test$estimate
all_MAP_aphid_abundance_1_day_mean_soil_moisture_p_value <- all_MAP_aphid_abundance_1_day_mean_soil_moisture_correlation_test$p.value

max(df_new$resid_MAP_1_day_mois)
min(df_new$resid_MAP_1_day_mois)

plot_all_MAP_1_day_mean_soil_moisture_aphid_abundance <- 
  ggplot(df_new,
         aes(x = resid_MAP_1_day_mois,
             y = resid_MAP,
             color = `growing season`)) + 
  geom_point(aes(fill = after_scale(scales::alpha(color, 0.30))),
             stroke = 0.2, shape = 21, size = 0.75) +
  geom_smooth(method = "lm", se = TRUE,col = "blue", aes(group = 1),
              alpha = 0.3, size = 0.5) +   # 信賴區間透明度
  
  labs(title = "",
       x = "Prev. 1-D Mois. ~ MAP",
       y = expression( ~ log[10](Aphids + 1)/m^2 ~ " ~ MAP")) +
  theme_classic(base_size = 8, base_family = "sans") +
  theme(plot.title=element_text(size=8, face="bold",hjust = 0.5),
        axis.title=element_text(size=8),
        axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x = element_text(size =6),
        axis.text.y = element_text(size = 6),
        legend.position= "none",        # 顯示圖例
        legend.key.size = unit(12,"points"),
        legend.title = element_blank()) +
  coord_cartesian(clip = "off") +
  scale_color_manual(values = c("2024 spring" = "#006600",  # 深綠
                                "2024 fall"   = "#867052")) +  # 深咖啡
  
  annotate("text", x = -14, y = -2.6,
           label = paste0("r = ",
                          round(all_MAP_aphid_abundance_1_day_mean_soil_moisture_correlation_coefficient, 3),
                          ", p < 0.001"),
           hjust = 0, vjust = 0, size = 2.5, color = "red", fontface = "bold") +
  scale_x_continuous(limits = c(-17, 17)) +
  scale_y_continuous(limits = c(-3.0, 3.0))


#daily mean soil moisture
all_MAP_aphid_abundance_1_day_mean_soil_moisture_spring_correlation_test <- cor.test(df_new_spring$resid_MAP,
                                                                                     df_new_spring$resid_MAP_1_day_mois,
                                                                                     use = "complete.obs")
# Extract the correlation coefficient and p-value
all_MAP_aphid_abundance_1_day_mean_soil_moisture_spring_correlation_coefficient <- all_MAP_aphid_abundance_1_day_mean_soil_moisture_spring_correlation_test$estimate
all_MAP_aphid_abundance_1_day_mean_soil_moisture_spring_p_value <- all_MAP_aphid_abundance_1_day_mean_soil_moisture_spring_correlation_test$p.value


#daily mean soil moisture
all_MAP_aphid_abundance_1_day_mean_soil_moisture_fall_correlation_test <- cor.test(df_new_fall$resid_MAP,
                                                                                   df_new_fall$resid_MAP_1_day_mois,
                                                                                   use = "complete.obs")
# Extract the correlation coefficient and p-value
all_MAP_aphid_abundance_1_day_mean_soil_moisture_fall_correlation_coefficient <- all_MAP_aphid_abundance_1_day_mean_soil_moisture_fall_correlation_test$estimate
all_MAP_aphid_abundance_1_day_mean_soil_moisture_fall_p_value <- all_MAP_aphid_abundance_1_day_mean_soil_moisture_fall_correlation_test$p.value


library(scales)  # for alpha()
# scale_color_manual(values = c("2024 spring" = "#006600", "2024 fall" = "#867052"))
# Scatter plot with a linear trend line
plot_all_MAP_1_day_mean_soil_moisture_aphid_abundance_season <- 
  ggplot(df_new,
         aes(x = resid_MAP_1_day_mois,
             y = resid_MAP,
             color = `growing season`)) + 
  geom_point(aes(fill = after_scale(scales::alpha(color, 0.30))),
             stroke = 0.2, shape = 21, size = 0.75) + 
  # 設定回歸線顏色，根據不同的 'growing season' 來顯示不同顏色
  geom_smooth(data = df_new %>% filter(`growing season` == "2024 spring"), method = "lm", se = TRUE,
              color = "#006600",  # 深綠色回歸線
              fill = "#66CC66",   # 淺綠色信賴區間
              alpha = 0.4, size = 0.5) +   # 信賴區間透明度
  geom_smooth(data = df_new %>% filter(`growing season` == "2024 fall"), method = "lm", se = TRUE,
              color = "#8B4513",  # 紅棕色回歸線
              fill = "#C4B7A6",   # 淺咖啡信賴區間
              alpha = 0.4, size = 0.5) +   # 信賴區間透明度
  labs(title = "",
       x = "Prev. 1-D Mois. ~ MAP",
       y = expression( ~ log[10](Aphids + 1)/m^2 ~ " ~ MAP")) +
  theme_classic(base_size = 8, base_family = "sans") +
  theme(plot.title=element_text(size=8, face="bold",hjust = 0.5),
        axis.title=element_text(size=8),
        axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x = element_text(size =6),
        axis.text.y = element_text(size = 6),
        legend.position= "none",        # 顯示圖例
        legend.key.size = unit(12,"points"),
        legend.title = element_blank()) +
  coord_cartesian(clip = "off") +
  # 資料點顏色設置，確保資料點顏色正確
  scale_color_manual(values = c("2024 spring" = "#006600",  # 深綠
                                "2024 fall"   = "#867052")) +  # 深咖啡
  # 區間顏色設置，確保資料的顏色不會被覆蓋
  scale_fill_manual(values = c("2024 spring" = "#66CC66",  # 淺綠
                               "2024 fall"   = "#C4B7A6")) +  # 淺咖啡
  annotate("text", x = -14, y = -2.4,
           label = paste0("r = ",
                          round(all_MAP_aphid_abundance_1_day_mean_soil_moisture_spring_correlation_coefficient, 3),
                          ", p < 0.001 "),
           hjust = 0, vjust = 0, size = 2.5, color = "#006600", fontface = "bold") +
  annotate("text", x = -14, y = -2.8,
           label = paste0("r = ",
                          round(all_MAP_aphid_abundance_1_day_mean_soil_moisture_fall_correlation_coefficient, 3),
                          ", p < 0.001 "),
           hjust = 0, vjust = 0, size = 2.5, color = "#8B4513", fontface = "bold") +
  scale_x_continuous(limits = c(-17,17)) +
  scale_y_continuous(limits = c(-3.0, 3.0))


# 3-day mean soil moisture
all_MAP_aphid_abundance_3_day_mean_soil_moisture_correlation_test <- cor.test(df_new$resid_MAP,
                                                                              df_new$resid_MAP_3_day_mois,
                                                                              use = "complete.obs")
# Extract the correlation coefficient and p-value
all_MAP_aphid_abundance_3_day_mean_soil_moisture_correlation_coefficient <- all_MAP_aphid_abundance_3_day_mean_soil_moisture_correlation_test$estimate
all_MAP_aphid_abundance_3_day_mean_soil_moisture_p_value <- all_MAP_aphid_abundance_3_day_mean_soil_moisture_correlation_test$p.value

max(df_new$resid_MAP_3_day_mois)
min(df_new$resid_MAP_3_day_mois)

plot_all_MAP_3_day_mean_soil_moisture_aphid_abundance <- 
  ggplot(df_new,
         aes(x = resid_MAP_3_day_mois,
             y = resid_MAP,
             color = `growing season`)) + 
  geom_point(aes(fill = after_scale(scales::alpha(color, 0.30))),
             stroke = 0.2, shape = 21, size = 0.75) +
  geom_smooth(method = "lm", se = TRUE,col = "blue", aes(group = 1),
              alpha = 0.3, size = 0.5) +   # 信賴區間透明度
  
  labs(title = "",
       x = "Prev. 3-D Mois. ~ MAP",
       y = expression( ~ log[10](Aphids + 1)/m^2 ~ " ~ MAP")) +
  theme_classic(base_size = 8, base_family = "sans") +
  theme(plot.title=element_text(size=8, face="bold",hjust = 0.5),
        axis.title=element_text(size=8),
        axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x = element_text(size =6),
        axis.text.y = element_text(size = 6),
        legend.position= "none",        # 顯示圖例
        legend.key.size = unit(12,"points"),
        legend.title = element_blank()) +
  coord_cartesian(clip = "off") +
  scale_color_manual(values = c("2024 spring" = "#006600",  # 深綠
                                "2024 fall"   = "#867052")) +  # 深咖啡
  
  annotate("text", x = -13, y = -2.6,
           label = paste0("r = ",
                          round(all_MAP_aphid_abundance_3_day_mean_soil_moisture_correlation_coefficient, 3),
                          ", p < 0.05 "),
           hjust = 0, vjust = 0, size = 2.5, color = "red", fontface = "bold") +
  scale_x_continuous(limits = c(-16, 16)) +
  scale_y_continuous(limits = c(-3.0, 3.0))


#daily mean soil moisture
all_MAP_aphid_abundance_3_day_mean_soil_moisture_spring_correlation_test <- cor.test(df_new_spring$resid_MAP,
                                                                                     df_new_spring$resid_MAP_3_day_mois,
                                                                                     use = "complete.obs")
# Extract the correlation coefficient and p-value
all_MAP_aphid_abundance_3_day_mean_soil_moisture_spring_correlation_coefficient <- all_MAP_aphid_abundance_3_day_mean_soil_moisture_spring_correlation_test$estimate
all_MAP_aphid_abundance_3_day_mean_soil_moisture_spring_p_value <- all_MAP_aphid_abundance_3_day_mean_soil_moisture_spring_correlation_test$p.value


#daily mean soil moisture
all_MAP_aphid_abundance_3_day_mean_soil_moisture_fall_correlation_test <- cor.test(df_new_fall$resid_MAP,
                                                                                   df_new_fall$resid_MAP_3_day_mois,
                                                                                   use = "complete.obs")
# Extract the correlation coefficient and p-value
all_MAP_aphid_abundance_3_day_mean_soil_moisture_fall_correlation_coefficient <- all_MAP_aphid_abundance_3_day_mean_soil_moisture_fall_correlation_test$estimate
all_MAP_aphid_abundance_3_day_mean_soil_moisture_fall_p_value <- all_MAP_aphid_abundance_3_day_mean_soil_moisture_fall_correlation_test$p.value


library(scales)  # for alpha()
# scale_color_manual(values = c("2024 spring" = "#006600", "2024 fall" = "#867052"))
# Scatter plot with a linear trend line
plot_all_MAP_3_day_mean_soil_moisture_aphid_abundance_season <- 
  ggplot(df_new,
         aes(x = resid_MAP_3_day_mois,
             y = resid_MAP,
             color = `growing season`)) + 
  geom_point(aes(fill = after_scale(scales::alpha(color, 0.30))),
             stroke = 0.2, shape = 21, size = 0.75) + 
  # 設定回歸線顏色，根據不同的 'growing season' 來顯示不同顏色
  geom_smooth(data = df_new %>% filter(`growing season` == "2024 spring"), method = "lm", se = TRUE,
              color = "#006600",  # 深綠色回歸線
              fill = "#66CC66",   # 淺綠色信賴區間
              alpha = 0.4, size = 0.5) +   # 信賴區間透明度
  geom_smooth(data = df_new %>% filter(`growing season` == "2024 fall"), method = "lm", se = TRUE,
              color = "#8B4513",  # 紅棕色回歸線
              fill = "#C4B7A6",   # 淺咖啡信賴區間
              alpha = 0.4, size = 0.5) +   # 信賴區間透明度
  labs(title = "",
       x = "Prev. 3-D Mois. ~ MAP",
       y = expression( ~ log[10](Aphids + 1)/m^2 ~ " ~ MAP")) +
  theme_classic(base_size = 8, base_family = "sans") +
  theme(plot.title=element_text(size=8, face="bold",hjust = 0.5),
        axis.title=element_text(size=8),
        axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x = element_text(size =6),
        axis.text.y = element_text(size = 6),
        legend.position= "none",        # 顯示圖例
        legend.key.size = unit(12,"points"),
        legend.title = element_blank()) +
  coord_cartesian(clip = "off") +
  # 資料點顏色設置，確保資料點顏色正確
  scale_color_manual(values = c("2024 spring" = "#006600",  # 深綠
                                "2024 fall"   = "#867052")) +  # 深咖啡
  # 區間顏色設置，確保資料的顏色不會被覆蓋
  scale_fill_manual(values = c("2024 spring" = "#66CC66",  # 淺綠
                               "2024 fall"   = "#C4B7A6")) +  # 淺咖啡
  annotate("text", x = -13, y = -2.4,
           label = paste0("r = ",
                          round(all_MAP_aphid_abundance_3_day_mean_soil_moisture_spring_correlation_coefficient, 3),
                          ", p < 0.05 "),
           hjust = 0, vjust = 0, size = 2.5, color = "#006600", fontface = "bold") +
  annotate("text", x = -13, y = -2.8,
           label = paste0("r = ",
                          round(all_MAP_aphid_abundance_3_day_mean_soil_moisture_fall_correlation_coefficient, 3),
                          ", p < 0.05 "),
           hjust = 0, vjust = 0, size = 2.5, color = "#8B4513", fontface = "bold") +
  scale_x_continuous(limits = c(-16, 16)) +
  scale_y_continuous(limits = c(-3.0, 3.0))


# 5-day mean soil moisture

all_MAP_aphid_abundance_5_day_mean_soil_moisture_correlation_test <- cor.test(df_new$resid_MAP,
                                                                              df_new$resid_MAP_5_day_mois,
                                                                              use = "complete.obs")
# Extract the correlation coefficient and p-value
all_MAP_aphid_abundance_5_day_mean_soil_moisture_correlation_coefficient <- all_MAP_aphid_abundance_5_day_mean_soil_moisture_correlation_test$estimate
all_MAP_aphid_abundance_5_day_mean_soil_moisture_p_value <- all_MAP_aphid_abundance_5_day_mean_soil_moisture_correlation_test$p.value

max(df_new$resid_MAP_5_day_mois)
min(df_new$resid_MAP_5_day_mois)

plot_all_MAP_5_day_mean_soil_moisture_aphid_abundance <- 
  ggplot(df_new,
         aes(x = resid_MAP_5_day_mois,
             y = resid_MAP,
             color = `growing season`)) + 
  geom_point(aes(fill = after_scale(scales::alpha(color, 0.30))),
             stroke = 0.2, shape = 21, size = 0.75) +
  geom_smooth(method = "lm", se = TRUE,col = "blue", aes(group = 1),
              alpha = 0.3, size = 0.5) +   # 信賴區間透明度
  
  labs(title = "",
       x = "Prev. 5-D Mois. ~ MAP",
       y = expression( ~ log[10](Aphids + 1)/m^2 ~ " ~ MAP")) +
  theme_classic(base_size = 8, base_family = "sans") +
  theme(plot.title=element_text(size=8, face="bold",hjust = 0.5),
        axis.title=element_text(size=8),
        axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x = element_text(size =6),
        axis.text.y = element_text(size = 6),
        legend.position= "none",        # 顯示圖例
        legend.key.size = unit(12,"points"),
        legend.title = element_blank()) +
  coord_cartesian(clip = "off") +
  scale_color_manual(values = c("2024 spring" = "#006600",  # 深綠
                                "2024 fall"   = "#867052")) +  # 深咖啡
  
  annotate("text", x = -13, y = -2.6,
           label = paste0("r = ",
                          round(all_MAP_aphid_abundance_5_day_mean_soil_moisture_correlation_coefficient, 3),
                          ", p = ",
                          signif(all_MAP_aphid_abundance_5_day_mean_soil_moisture_p_value, 3)),
           hjust = 0, vjust = 0, size = 2.5, color = "black") +
  scale_x_continuous(limits = c(-16, 16)) +
  scale_y_continuous(limits = c(-3.0, 3.0))


#daily mean soil moisture
all_MAP_aphid_abundance_5_day_mean_soil_moisture_spring_correlation_test <- cor.test(df_new_spring$resid_MAP,
                                                                                     df_new_spring$resid_MAP_5_day_mois,
                                                                                     use = "complete.obs")
# Extract the correlation coefficient and p-value
all_MAP_aphid_abundance_5_day_mean_soil_moisture_spring_correlation_coefficient <- all_MAP_aphid_abundance_5_day_mean_soil_moisture_spring_correlation_test$estimate
all_MAP_aphid_abundance_5_day_mean_soil_moisture_spring_p_value <- all_MAP_aphid_abundance_5_day_mean_soil_moisture_spring_correlation_test$p.value


#daily mean soil moisture
all_MAP_aphid_abundance_5_day_mean_soil_moisture_fall_correlation_test <- cor.test(df_new_fall$resid_MAP,
                                                                                   df_new_fall$resid_MAP_5_day_mois,
                                                                                   use = "complete.obs")
# Extract the correlation coefficient and p-value
all_MAP_aphid_abundance_5_day_mean_soil_moisture_fall_correlation_coefficient <- all_MAP_aphid_abundance_5_day_mean_soil_moisture_fall_correlation_test$estimate
all_MAP_aphid_abundance_5_day_mean_soil_moisture_fall_p_value <- all_MAP_aphid_abundance_5_day_mean_soil_moisture_fall_correlation_test$p.value


library(scales)  # for alpha()
# scale_color_manual(values = c("2024 spring" = "#006600", "2024 fall" = "#867052"))
# Scatter plot with a linear trend line
plot_all_MAP_5_day_mean_soil_moisture_aphid_abundance_season <- 
  ggplot(df_new,
         aes(x = resid_MAP_5_day_mois,
             y = resid_MAP,
             color = `growing season`)) + 
  geom_point(aes(fill = after_scale(scales::alpha(color, 0.30))),
             stroke = 0.2, shape = 21, size = 0.75) + 
  # 設定回歸線顏色，根據不同的 'growing season' 來顯示不同顏色
  geom_smooth(data = df_new %>% filter(`growing season` == "2024 spring"), method = "lm", se = TRUE,
              color = "#006600",  # 深綠色回歸線
              fill = "#66CC66",   # 淺綠色信賴區間
              alpha = 0.4, size = 0.5) +   # 信賴區間透明度
  geom_smooth(data = df_new %>% filter(`growing season` == "2024 fall"), method = "lm", se = TRUE,
              color = "#8B4513",  # 紅棕色回歸線
              fill = "#C4B7A6",   # 淺咖啡信賴區間
              alpha = 0.4, size = 0.5) +   # 信賴區間透明度
  labs(title = "",
       x = "Prev. 5-D Mois. ~ MAP",
       y = expression( ~ log[10](Aphids + 1)/m^2 ~ " ~ MAP")) +
  theme_classic(base_size = 8, base_family = "sans") +
  theme(plot.title=element_text(size=8, face="bold",hjust = 0.5),
        axis.title=element_text(size=8),
        axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x = element_text(size =6),
        axis.text.y = element_text(size = 6),
        legend.position= "none",        # 顯示圖例
        legend.key.size = unit(12,"points"),
        legend.title = element_blank()) +
  coord_cartesian(clip = "off") +
  # 資料點顏色設置，確保資料點顏色正確
  scale_color_manual(values = c("2024 spring" = "#006600",  # 深綠
                                "2024 fall"   = "#867052")) +  # 深咖啡
  # 區間顏色設置，確保資料的顏色不會被覆蓋
  scale_fill_manual(values = c("2024 spring" = "#66CC66",  # 淺綠
                               "2024 fall"   = "#C4B7A6")) +  # 淺咖啡
  annotate("text", x = -13, y = -2.4,
           label = paste0("r = ",
                          round(all_MAP_aphid_abundance_5_day_mean_soil_moisture_spring_correlation_coefficient, 3),
                          ", p < 0.05 "),
           hjust = 0, vjust = 0, size = 2.5, color = "#006600", fontface = "bold") +
  annotate("text", x = -13, y = -2.8,
           label = paste0("r = ",
                          round(all_MAP_aphid_abundance_5_day_mean_soil_moisture_fall_correlation_coefficient, 3),
                          ", p = ",
                          signif(all_MAP_aphid_abundance_5_day_mean_soil_moisture_fall_p_value)),
           hjust = 0, vjust = 0, size = 2.5, color = "#8B4513") +
  scale_x_continuous(limits = c(-16, 16)) +
  scale_y_continuous(limits = c(-3.0, 3.0))


# 7-day mean soil moisture

all_MAP_aphid_abundance_7_day_mean_soil_moisture_correlation_test <- cor.test(df_new$resid_MAP,
                                                                              df_new$resid_MAP_7_day_mois,
                                                                              use = "complete.obs")
# Extract the correlation coefficient and p-value
all_MAP_aphid_abundance_7_day_mean_soil_moisture_correlation_coefficient <- all_MAP_aphid_abundance_7_day_mean_soil_moisture_correlation_test$estimate
all_MAP_aphid_abundance_7_day_mean_soil_moisture_p_value <- all_MAP_aphid_abundance_7_day_mean_soil_moisture_correlation_test$p.value

max(df_new$resid_MAP_7_day_mois)
min(df_new$resid_MAP_7_day_mois)

plot_all_MAP_7_day_mean_soil_moisture_aphid_abundance <- 
  ggplot(df_new,
         aes(x = resid_MAP_7_day_mois,
             y = resid_MAP,
             color = `growing season`)) + 
  geom_point(aes(fill = after_scale(scales::alpha(color, 0.30))),
             stroke = 0.2, shape = 21, size = 0.75) +
  geom_smooth(method = "lm", se = TRUE,col = "blue", aes(group = 1),
              alpha = 0.3, size = 0.5) +   # 信賴區間透明度
  
  labs(title = "",
       x = "Prev. 7-D Mois. ~ MAP",
       y = expression( ~ log[10](Aphids + 1)/m^2 ~ " ~ MAP")) +
  theme_classic(base_size = 8, base_family = "sans") +
  theme(plot.title=element_text(size=8, face="bold",hjust = 0.5),
        axis.title=element_text(size=8),
        axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x = element_text(size =6),
        axis.text.y = element_text(size = 6),
        legend.position= "none",        # 顯示圖例
        legend.key.size = unit(12,"points"),
        legend.title = element_blank()) +
  coord_cartesian(clip = "off") +
  scale_color_manual(values = c("2024 spring" = "#006600",  # 深綠
                                "2024 fall"   = "#867052")) +  # 深咖啡
  
  annotate("text", x = -12.5, y = -2.6,
           label = paste0("r = ",
                          round(all_MAP_aphid_abundance_7_day_mean_soil_moisture_correlation_coefficient, 3),
                          ", p = ",
                          signif(all_MAP_aphid_abundance_7_day_mean_soil_moisture_p_value, 3)),
           hjust = 0, vjust = 0, size = 2.5, color = "black") +
  scale_x_continuous(limits = c(-15, 15)) +
  scale_y_continuous(limits = c(-3.0, 3.0))


#daily mean soil moisture
all_MAP_aphid_abundance_7_day_mean_soil_moisture_spring_correlation_test <- cor.test(df_new_spring$resid_MAP,
                                                                                     df_new_spring$resid_MAP_7_day_mois,
                                                                                     use = "complete.obs")
# Extract the correlation coefficient and p-value
all_MAP_aphid_abundance_7_day_mean_soil_moisture_spring_correlation_coefficient <- all_MAP_aphid_abundance_7_day_mean_soil_moisture_spring_correlation_test$estimate
all_MAP_aphid_abundance_7_day_mean_soil_moisture_spring_p_value <- all_MAP_aphid_abundance_7_day_mean_soil_moisture_spring_correlation_test$p.value


#daily mean soil moisture
all_MAP_aphid_abundance_7_day_mean_soil_moisture_fall_correlation_test <- cor.test(df_new_fall$resid_MAP,
                                                                                   df_new_fall$resid_MAP_7_day_mois,
                                                                                   use = "complete.obs")
# Extract the correlation coefficient and p-value
all_MAP_aphid_abundance_7_day_mean_soil_moisture_fall_correlation_coefficient <- all_MAP_aphid_abundance_7_day_mean_soil_moisture_fall_correlation_test$estimate
all_MAP_aphid_abundance_7_day_mean_soil_moisture_fall_p_value <- all_MAP_aphid_abundance_7_day_mean_soil_moisture_fall_correlation_test$p.value


library(scales)  # for alpha()
# scale_color_manual(values = c("2024 spring" = "#006600", "2024 fall" = "#867052"))
# Scatter plot with a linear trend line
plot_all_MAP_7_day_mean_soil_moisture_aphid_abundance_season <- 
  ggplot(df_new,
         aes(x = resid_MAP_7_day_mois,
             y = resid_MAP,
             color = `growing season`)) + 
  geom_point(aes(fill = after_scale(scales::alpha(color, 0.30))),
             stroke = 0.2, shape = 21, size = 0.75) + 
  # 設定回歸線顏色，根據不同的 'growing season' 來顯示不同顏色
  geom_smooth(data = df_new %>% filter(`growing season` == "2024 spring"), method = "lm", se = TRUE,
              color = "#006600",  # 深綠色回歸線
              fill = "#66CC66",   # 淺綠色信賴區間
              alpha = 0.4, size = 0.5) +   # 信賴區間透明度
  geom_smooth(data = df_new %>% filter(`growing season` == "2024 fall"), method = "lm", se = TRUE,
              color = "#8B4513",  # 紅棕色回歸線
              fill = "#C4B7A6",   # 淺咖啡信賴區間
              alpha = 0.4, size = 0.5) +   # 信賴區間透明度
  labs(title = "",
       x = "Prev. 7-D Mois. ~ MAP",
       y = expression( ~ log[10](Aphids + 1)/m^2 ~ " ~ MAP")) +
  theme_classic(base_size = 8, base_family = "sans") +
  theme(plot.title=element_text(size=8, face="bold",hjust = 0.5),
        axis.title=element_text(size=8),
        axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x = element_text(size =6),
        axis.text.y = element_text(size = 6),
        legend.position= "none",        # 顯示圖例
        legend.key.size = unit(12,"points"),
        legend.title = element_blank()) +
  coord_cartesian(clip = "off") +
  # 資料點顏色設置，確保資料點顏色正確
  scale_color_manual(values = c("2024 spring" = "#006600",  # 深綠
                                "2024 fall"   = "#867052")) +  # 深咖啡
  # 區間顏色設置，確保資料的顏色不會被覆蓋
  scale_fill_manual(values = c("2024 spring" = "#66CC66",  # 淺綠
                               "2024 fall"   = "#C4B7A6")) +  # 淺咖啡
  annotate("text", x = -12.5, y = -2.4,
           label = paste0("r = ",
                          round(all_MAP_aphid_abundance_7_day_mean_soil_moisture_spring_correlation_coefficient, 3),
                          ", p = ",
                          signif(all_MAP_aphid_abundance_7_day_mean_soil_moisture_spring_p_value)),
           hjust = 0, vjust = 0, size = 2.5, color = "#006600") +
  annotate("text", x = -12.5, y = -2.8,
           label = paste0("r = ",
                          round(all_MAP_aphid_abundance_7_day_mean_soil_moisture_fall_correlation_coefficient, 3),
                          ", p = ",
                          signif(all_MAP_aphid_abundance_7_day_mean_soil_moisture_fall_p_value)),
           hjust = 0, vjust = 0, size = 2.5, color = "#8B4513") +
  scale_x_continuous(limits = c(-15, 15)) +
  scale_y_continuous(limits = c(-3.0, 3.0))



all_MAP_aphid_abundance_daily_mean_soil_conductivity_correlation_test <- cor.test(df_new$resid_MAP,
                                                                                  df_new$resid_MAP_daily_ec,
                                                                                  use = "complete.obs")
# Extract the correlation coefficient and p-value
all_MAP_aphid_abundance_daily_mean_soil_conductivity_correlation_coefficient <- all_MAP_aphid_abundance_daily_mean_soil_conductivity_correlation_test$estimate
all_MAP_aphid_abundance_daily_mean_soil_conductivity_p_value <- all_MAP_aphid_abundance_daily_mean_soil_conductivity_correlation_test$p.value

max(df_new$resid_MAP_daily_ec)
min(df_new$resid_MAP_daily_ec)

plot_all_MAP_daily_mean_soil_conductivity_aphid_abundance <- 
  ggplot(df_new,
         aes(x = resid_MAP_daily_ec,
             y = resid_MAP,
             color = `growing season`)) + 
  geom_point(aes(fill = after_scale(scales::alpha(color, 0.30))),
             stroke = 0.2, shape = 21, size = 0.75) +
  geom_smooth(method = "lm", se = TRUE,col = "blue", aes(group = 1),
              alpha = 0.3, size = 0.5) +   # 信賴區間透明度
  
  labs(title = "",
       x = "Daily EC ~ MAP",
       y = expression( ~ log[10](Aphids + 1)/m^2 ~ " ~ MAP" )) +
  theme_classic(base_size = 8, base_family = "sans") +
  theme(plot.title=element_text(size=8, face="bold",hjust = 0.5),
        axis.title=element_text(size=8),
        axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x = element_text(size =6),
        axis.text.y = element_text(size = 6),
        legend.position= "none",        # 顯示圖例
        legend.key.size = unit(12,"points"),
        legend.title = element_blank()) +
  coord_cartesian(clip = "off") +
  scale_color_manual(values = c("2024 spring" = "#006600",  # 深綠
                                "2024 fall"   = "#867052")) +  # 深咖啡
  
  annotate("text", x = -0.05, y = -2.6,
           label = paste0("r = ",
                          round(all_MAP_aphid_abundance_daily_mean_soil_conductivity_correlation_coefficient, 3),
                          ", p = ",
                          signif(all_MAP_aphid_abundance_daily_mean_soil_conductivity_p_value, 3)),
           hjust = 0, vjust = 0, size = 2.5, color = "black")  +
  scale_x_continuous(limits = c(-0.15, 1.0)) +
  scale_y_continuous(limits = c(-3.0, 3.0))

#daily mean soil EC
all_MAP_aphid_abundance_daily_mean_soil_conductivity_spring_correlation_test <- cor.test(df_new_spring$resid_MAP,
                                                                                         df_new_spring$resid_MAP_daily_ec,
                                                                                         use = "complete.obs")
# Extract the correlation coefficient and p-value
all_MAP_aphid_abundance_daily_mean_soil_conductivity_spring_correlation_coefficient <- all_MAP_aphid_abundance_daily_mean_soil_conductivity_spring_correlation_test$estimate
all_MAP_aphid_abundance_daily_mean_soil_conductivity_spring_p_value <- all_MAP_aphid_abundance_daily_mean_soil_conductivity_spring_correlation_test$p.value


#daily mean soil moisture
all_MAP_aphid_abundance_daily_mean_soil_conductivity_fall_correlation_test <- cor.test(df_new_fall$resid_MAP,
                                                                                       df_new_fall$resid_MAP_daily_ec,
                                                                                       use = "complete.obs")
# Extract the correlation coefficient and p-value
all_MAP_aphid_abundance_daily_mean_soil_conductivity_fall_correlation_coefficient <- all_MAP_aphid_abundance_daily_mean_soil_conductivity_fall_correlation_test$estimate
all_MAP_aphid_abundance_daily_mean_soil_conductivity_fall_p_value <- all_MAP_aphid_abundance_daily_mean_soil_conductivity_fall_correlation_test$p.value



# Scatter plot with a linear trend line
plot_all_MAP_daily_mean_soil_conductivity_aphid_abundance_season <- 
  ggplot(df_new,
         aes(x = resid_MAP_daily_ec,
             y = resid_MAP,
             color = `growing season`)) + 
  geom_point(aes(fill = after_scale(scales::alpha(color, 0.30))),
             stroke = 0.2, shape = 21, size = 0.75) + 
  # 設定回歸線顏色，根據不同的 'growing season' 來顯示不同顏色
  geom_smooth(data = df_new %>% filter(`growing season` == "2024 spring"), method = "lm", se = TRUE,
              color = "#006600",  # 深綠色回歸線
              fill = "#66CC66",   # 淺綠色信賴區間
              alpha = 0.4, size = 0.5) +   # 信賴區間透明度
  geom_smooth(data = df_new %>% filter(`growing season` == "2024 fall"), method = "lm", se = TRUE,
              color = "#8B4513",  # 紅棕色回歸線
              fill = "#C4B7A6",   # 淺咖啡信賴區間
              alpha = 0.4, size = 0.5) +   # 信賴區間透明度
  labs(title = "",
       x = "Daily EC ~ MAP",
       y = expression( ~ log[10](Aphids + 1)/m^2 ~ " ~ MAP")) +
  theme_classic(base_size = 8, base_family = "sans") +
  theme(plot.title=element_text(size=8, face="bold",hjust = 0.5),
        axis.title=element_text(size=8),
        axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x = element_text(size =6),
        axis.text.y = element_text(size = 6),
        legend.position= "none",        # 顯示圖例
        legend.key.size = unit(12,"points"),
        legend.title = element_blank()) +
  coord_cartesian(clip = "off") +
  # 資料點顏色設置，確保資料點顏色正確
  scale_color_manual(values = c("2024 spring" = "#006600",  # 深綠
                                "2024 fall"   = "#867052")) +  # 深咖啡
  # 區間顏色設置，確保資料的顏色不會被覆蓋
  scale_fill_manual(values = c("2024 spring" = "#66CC66",  # 淺綠
                               "2024 fall"   = "#C4B7A6")) +  # 淺咖啡
  annotate("text", x = -0.05, y = -2.4,
           label = paste0("r = ",
                          round(all_MAP_aphid_abundance_daily_mean_soil_conductivity_spring_correlation_coefficient, 3),
                          ", p = ",
                          signif(all_MAP_aphid_abundance_daily_mean_soil_conductivity_spring_p_value, 3)),
           hjust = 0, vjust = 0, size = 2.5, color = "#006600") +
  annotate("text", x = -0.05, y = -2.8,
           label = paste0("r = ",
                          round(all_MAP_aphid_abundance_daily_mean_soil_conductivity_fall_correlation_coefficient, 3),
                          ", p = ",
                          signif(all_MAP_aphid_abundance_daily_mean_soil_conductivity_fall_p_value, 3)),
           hjust = 0, vjust = 0, size = 2.5, color = "#8B4513") +
  scale_x_continuous(limits = c(-0.15, 1.0)) +
  scale_y_continuous(limits = c(-3.0, 3.0))



# Previous 1-day mean soil EC
all_MAP_aphid_abundance_1_day_mean_soil_conductivity_correlation_test <- cor.test(df_new$resid_MAP,
                                                                                  df_new$resid_MAP_1_day_ec,
                                                                                  use = "complete.obs")
# Extract the correlation coefficient and p-value
all_MAP_aphid_abundance_1_day_mean_soil_conductivity_correlation_coefficient <- all_MAP_aphid_abundance_1_day_mean_soil_conductivity_correlation_test$estimate
all_MAP_aphid_abundance_1_day_mean_soil_conductivity_p_value <- all_MAP_aphid_abundance_1_day_mean_soil_conductivity_correlation_test$p.value

max(df_new$resid_MAP_1_day_ec)
min(df_new$resid_MAP_1_day_ec)

plot_all_MAP_1_day_mean_soil_conductivity_aphid_abundance <- 
  ggplot(df_new,
         aes(x = resid_MAP_1_day_ec,
             y = resid_MAP,
             color = `growing season`)) + 
  geom_point(aes(fill = after_scale(scales::alpha(color, 0.30))),
             stroke = 0.2, shape = 21, size = 0.75) +
  geom_smooth(method = "lm", se = TRUE,col = "blue", aes(group = 1),
              alpha = 0.3, size = 0.5) +   # 信賴區間透明度
  
  labs(title = "",
       x = "Prev. 1-D EC ~ MAP",
       y = expression( ~ log[10](Aphids + 1)/m^2 ~ " ~ MAP" )) +
  theme_classic(base_size = 8, base_family = "sans") +
  theme(plot.title=element_text(size=8, face="bold",hjust = 0.5),
        axis.title=element_text(size=8),
        axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x = element_text(size =6),
        axis.text.y = element_text(size = 6),
        legend.position= "none",        # 顯示圖例
        legend.key.size = unit(12,"points"),
        legend.title = element_blank()) +
  coord_cartesian(clip = "off") +
  scale_color_manual(values = c("2024 spring" = "#006600",  # 深綠
                                "2024 fall"   = "#867052")) +  # 深咖啡
  
  annotate("text", x = -0.05, y = -2.6,
           label = paste0("r = ",
                          round(all_MAP_aphid_abundance_1_day_mean_soil_conductivity_correlation_coefficient, 3),
                          ", p = ",
                          signif(all_MAP_aphid_abundance_1_day_mean_soil_conductivity_p_value, 3)),
           hjust = 0, vjust = 0, size = 2.5, color = "black")  +
  scale_x_continuous(limits = c(-0.15, 0.5)) +
  scale_y_continuous(limits = c(-3.0, 3.0))

#daily mean soil EC
all_MAP_aphid_abundance_1_day_mean_soil_conductivity_spring_correlation_test <- cor.test(df_new_spring$resid_MAP,
                                                                                         df_new_spring$resid_MAP_1_day_ec,
                                                                                         use = "complete.obs")
# Extract the correlation coefficient and p-value
all_MAP_aphid_abundance_1_day_mean_soil_conductivity_spring_correlation_coefficient <- all_MAP_aphid_abundance_1_day_mean_soil_conductivity_spring_correlation_test$estimate
all_MAP_aphid_abundance_1_day_mean_soil_conductivity_spring_p_value <- all_MAP_aphid_abundance_1_day_mean_soil_conductivity_spring_correlation_test$p.value


#daily mean soil moisture
all_MAP_aphid_abundance_1_day_mean_soil_conductivity_fall_correlation_test <- cor.test(df_new_fall$resid_MAP,
                                                                                       df_new_fall$resid_MAP_1_day_ec,
                                                                                       use = "complete.obs")
# Extract the correlation coefficient and p-value
all_MAP_aphid_abundance_1_day_mean_soil_conductivity_fall_correlation_coefficient <- all_MAP_aphid_abundance_1_day_mean_soil_conductivity_fall_correlation_test$estimate
all_MAP_aphid_abundance_1_day_mean_soil_conductivity_fall_p_value <- all_MAP_aphid_abundance_1_day_mean_soil_conductivity_fall_correlation_test$p.value



# Scatter plot with a linear trend line
plot_all_MAP_1_day_mean_soil_conductivity_aphid_abundance_season <- 
  ggplot(df_new,
         aes(x = resid_MAP_1_day_ec,
             y = resid_MAP,
             color = `growing season`)) + 
  geom_point(aes(fill = after_scale(scales::alpha(color, 0.30))),
             stroke = 0.2, shape = 21, size = 0.75) + 
  # 設定回歸線顏色，根據不同的 'growing season' 來顯示不同顏色
  geom_smooth(data = df_new %>% filter(`growing season` == "2024 spring"), method = "lm", se = TRUE,
              color = "#006600",  # 深綠色回歸線
              fill = "#66CC66",   # 淺綠色信賴區間
              alpha = 0.4, size = 0.5) +   # 信賴區間透明度
  geom_smooth(data = df_new %>% filter(`growing season` == "2024 fall"), method = "lm", se = TRUE,
              color = "#8B4513",  # 紅棕色回歸線
              fill = "#C4B7A6",   # 淺咖啡信賴區間
              alpha = 0.4, size = 0.5) +   # 信賴區間透明度
  labs(title = "",
       x = "Prev. 1-D EC ~ MAP",
       y = expression( ~ log[10](Aphids + 1)/m^2 ~ " ~ MAP")) +
  theme_classic(base_size = 8, base_family = "sans") +
  theme(plot.title=element_text(size=8, face="bold",hjust = 0.5),
        axis.title=element_text(size=8),
        axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x = element_text(size =6),
        axis.text.y = element_text(size = 6),
        legend.position= "none",        # 顯示圖例
        legend.key.size = unit(12,"points"),
        legend.title = element_blank()) +
  coord_cartesian(clip = "off") +
  # 資料點顏色設置，確保資料點顏色正確
  scale_color_manual(values = c("2024 spring" = "#006600",  # 深綠
                                "2024 fall"   = "#867052")) +  # 深咖啡
  # 區間顏色設置，確保資料的顏色不會被覆蓋
  scale_fill_manual(values = c("2024 spring" = "#66CC66",  # 淺綠
                               "2024 fall"   = "#C4B7A6")) +  # 淺咖啡
  annotate("text", x = -0.05, y = -2.4,
           label = paste0("r = ",
                          round(all_MAP_aphid_abundance_1_day_mean_soil_conductivity_spring_correlation_coefficient, 3),
                          ", p = ",
                          signif(all_MAP_aphid_abundance_1_day_mean_soil_conductivity_spring_p_value, 3)),
           hjust = 0, vjust = 0, size = 2.5, color = "#006600") +
  annotate("text", x = -0.05, y = -2.8,
           label = paste0("r = ",
                          round(all_MAP_aphid_abundance_1_day_mean_soil_conductivity_fall_correlation_coefficient, 3),
                          ", p = ",
                          signif(all_MAP_aphid_abundance_1_day_mean_soil_conductivity_fall_p_value, 3)),
           hjust = 0, vjust = 0, size = 2.5, color = "#8B4513") +
  scale_x_continuous(limits = c(-0.15, 0.5)) +
  scale_y_continuous(limits = c(-3.0, 3.0))

# 3-day mean soil conductivity
all_MAP_aphid_abundance_3_day_mean_soil_conductivity_correlation_test <- cor.test(df_new$resid_MAP,
                                                                                  df_new$resid_MAP_3_day_ec,
                                                                                  use = "complete.obs")
# Extract the correlation coefficient and p-value
all_MAP_aphid_abundance_3_day_mean_soil_conductivity_correlation_coefficient <- all_MAP_aphid_abundance_3_day_mean_soil_conductivity_correlation_test$estimate
all_MAP_aphid_abundance_3_day_mean_soil_conductivity_p_value <- all_MAP_aphid_abundance_3_day_mean_soil_conductivity_correlation_test$p.value

max(df_new$resid_MAP_3_day_ec)
min(df_new$resid_MAP_3_day_ec)

plot_all_MAP_3_day_mean_soil_conductivity_aphid_abundance <- 
  ggplot(df_new,
         aes(x = resid_MAP_3_day_ec,
             y = resid_MAP,
             color = `growing season`)) + 
  geom_point(aes(fill = after_scale(scales::alpha(color, 0.30))),
             stroke = 0.2, shape = 21, size = 0.75) +
  geom_smooth(method = "lm", se = TRUE,col = "blue", aes(group = 1),
              alpha = 0.3, size = 0.5) +   # 信賴區間透明度
  
  labs(title = "",
       x = "Prev. 3-D EC ~ MAP",
       y = expression( ~ log[10](Aphids + 1)/m^2 ~ " ~ MAP" )) +
  theme_classic(base_size = 8, base_family = "sans") +
  theme(plot.title=element_text(size=8, face="bold",hjust = 0.5),
        axis.title=element_text(size=8),
        axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x = element_text(size =6),
        axis.text.y = element_text(size = 6),
        legend.position= "none",        # 顯示圖例
        legend.key.size = unit(12,"points"),
        legend.title = element_blank()) +
  coord_cartesian(clip = "off") +
  scale_color_manual(values = c("2024 spring" = "#006600",  # 深綠
                                "2024 fall"   = "#867052")) +  # 深咖啡
  
  annotate("text", x = -0.08, y = -2.6,
           label = paste0("r = ",
                          round(all_MAP_aphid_abundance_3_day_mean_soil_conductivity_correlation_coefficient, 3),
                          ", p = ",
                          signif(all_MAP_aphid_abundance_3_day_mean_soil_conductivity_p_value, 3)),
           hjust = 0, vjust = 0, size = 2.5, color = "black")  +
  scale_x_continuous(limits = c(-0.15, 0.6)) +
  scale_y_continuous(limits = c(-3.0, 3.0))

#daily mean soil EC
all_MAP_aphid_abundance_3_day_mean_soil_conductivity_spring_correlation_test <- cor.test(df_new_spring$resid_MAP,
                                                                                         df_new_spring$resid_MAP_3_day_ec,
                                                                                         use = "complete.obs")
# Extract the correlation coefficient and p-value
all_MAP_aphid_abundance_3_day_mean_soil_conductivity_spring_correlation_coefficient <- all_MAP_aphid_abundance_3_day_mean_soil_conductivity_spring_correlation_test$estimate
all_MAP_aphid_abundance_3_day_mean_soil_conductivity_spring_p_value <- all_MAP_aphid_abundance_3_day_mean_soil_conductivity_spring_correlation_test$p.value


#daily mean soil moisture
all_MAP_aphid_abundance_3_day_mean_soil_conductivity_fall_correlation_test <- cor.test(df_new_fall$resid_MAP,
                                                                                       df_new_fall$resid_MAP_3_day_ec,
                                                                                       use = "complete.obs")
# Extract the correlation coefficient and p-value
all_MAP_aphid_abundance_3_day_mean_soil_conductivity_fall_correlation_coefficient <- all_MAP_aphid_abundance_3_day_mean_soil_conductivity_fall_correlation_test$estimate
all_MAP_aphid_abundance_3_day_mean_soil_conductivity_fall_p_value <- all_MAP_aphid_abundance_3_day_mean_soil_conductivity_fall_correlation_test$p.value



# Scatter plot with a linear trend line
plot_all_MAP_3_day_mean_soil_conductivity_aphid_abundance_season <- 
  ggplot(df_new,
         aes(x = resid_MAP_3_day_ec,
             y = resid_MAP,
             color = `growing season`)) + 
  geom_point(aes(fill = after_scale(scales::alpha(color, 0.30))),
             stroke = 0.2, shape = 21, size = 0.75) + 
  # 設定回歸線顏色，根據不同的 'growing season' 來顯示不同顏色
  geom_smooth(data = df_new %>% filter(`growing season` == "2024 spring"), method = "lm", se = TRUE,
              color = "#006600",  # 深綠色回歸線
              fill = "#66CC66",   # 淺綠色信賴區間
              alpha = 0.4, size = 0.5) +   # 信賴區間透明度
  geom_smooth(data = df_new %>% filter(`growing season` == "2024 fall"), method = "lm", se = TRUE,
              color = "#8B4513",  # 紅棕色回歸線
              fill = "#C4B7A6",   # 淺咖啡信賴區間
              alpha = 0.4, size = 0.5) +   # 信賴區間透明度
  labs(title = "",
       x = "Prev. 3-D EC ~ MAP",
       y = expression( ~ log[10](Aphids + 1)/m^2 ~ " ~ MAP")) +
  theme_classic(base_size = 8, base_family = "sans") +
  theme(plot.title=element_text(size=8, face="bold",hjust = 0.5),
        axis.title=element_text(size=8),
        axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x = element_text(size =6),
        axis.text.y = element_text(size = 6),
        legend.position= "none",        # 顯示圖例
        legend.key.size = unit(12,"points"),
        legend.title = element_blank()) +
  coord_cartesian(clip = "off") +
  # 資料點顏色設置，確保資料點顏色正確
  scale_color_manual(values = c("2024 spring" = "#006600",  # 深綠
                                "2024 fall"   = "#867052")) +  # 深咖啡
  # 區間顏色設置，確保資料的顏色不會被覆蓋
  scale_fill_manual(values = c("2024 spring" = "#66CC66",  # 淺綠
                               "2024 fall"   = "#C4B7A6")) +  # 淺咖啡
  annotate("text", x = -0.08, y = -2.4,
           label = paste0("r = ",
                          round(all_MAP_aphid_abundance_3_day_mean_soil_conductivity_spring_correlation_coefficient, 3),
                          ", p < 0.05 "),
           hjust = 0, vjust = 0, size = 2.5, color = "#006600", fontface = "bold") +
  annotate("text", x = -0.08, y = -2.8,
           label = paste0("r = ",
                          round(all_MAP_aphid_abundance_3_day_mean_soil_conductivity_fall_correlation_coefficient, 3),
                          ", p = ",
                          signif(all_MAP_aphid_abundance_3_day_mean_soil_conductivity_fall_p_value, 3)),
           hjust = 0, vjust = 0, size = 2.5, color = "#8B4513") +
  scale_x_continuous(limits = c(-0.15, 0.6)) +
  scale_y_continuous(limits = c(-3.0, 3.0))


# 5-day mean soil conductivity
all_MAP_aphid_abundance_5_day_mean_soil_conductivity_correlation_test <- cor.test(df_new$resid_MAP,
                                                                                  df_new$resid_MAP_5_day_ec,
                                                                                  use = "complete.obs")
# Extract the correlation coefficient and p-value
all_MAP_aphid_abundance_5_day_mean_soil_conductivity_correlation_coefficient <- all_MAP_aphid_abundance_5_day_mean_soil_conductivity_correlation_test$estimate
all_MAP_aphid_abundance_5_day_mean_soil_conductivity_p_value <- all_MAP_aphid_abundance_5_day_mean_soil_conductivity_correlation_test$p.value

max(df_new$resid_MAP_5_day_ec)
min(df_new$resid_MAP_5_day_ec)

plot_all_MAP_5_day_mean_soil_conductivity_aphid_abundance <- 
  ggplot(df_new,
         aes(x = resid_MAP_5_day_ec,
             y = resid_MAP,
             color = `growing season`)) + 
  geom_point(aes(fill = after_scale(scales::alpha(color, 0.30))),
             stroke = 0.2, shape = 21, size = 0.75) +
  geom_smooth(method = "lm", se = TRUE,col = "blue", aes(group = 1),
              alpha = 0.3, size = 0.5) +   # 信賴區間透明度
  
  labs(title = "",
       x = "Prev. 5-D EC ~ MAP",
       y = expression( ~ log[10](Aphids + 1)/m^2 ~ " ~ MAP" )) +
  theme_classic(base_size = 8, base_family = "sans") +
  theme(plot.title=element_text(size=8, face="bold",hjust = 0.5),
        axis.title=element_text(size=8),
        axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x = element_text(size =6),
        axis.text.y = element_text(size = 6),
        legend.position= "none",        # 顯示圖例
        legend.key.size = unit(12,"points"),
        legend.title = element_blank()) +
  coord_cartesian(clip = "off") +
  scale_color_manual(values = c("2024 spring" = "#006600",  # 深綠
                                "2024 fall"   = "#867052")) +  # 深咖啡
  
  annotate("text", x = -0.1, y = -2.6,
           label = paste0("r = ",
                          round(all_MAP_aphid_abundance_5_day_mean_soil_conductivity_correlation_coefficient, 3),
                          ", p = ",
                          signif(all_MAP_aphid_abundance_5_day_mean_soil_conductivity_p_value, 3)),
           hjust = 0, vjust = 0, size = 2.5, color = "black")  +
  scale_x_continuous(limits = c(-0.14, 1.4)) +
  scale_y_continuous(limits = c(-3.0, 3.0))

#daily mean soil EC
all_MAP_aphid_abundance_5_day_mean_soil_conductivity_spring_correlation_test <- cor.test(df_new_spring$resid_MAP,
                                                                                         df_new_spring$resid_MAP_5_day_ec,
                                                                                         use = "complete.obs")
# Extract the correlation coefficient and p-value
all_MAP_aphid_abundance_5_day_mean_soil_conductivity_spring_correlation_coefficient <- all_MAP_aphid_abundance_5_day_mean_soil_conductivity_spring_correlation_test$estimate
all_MAP_aphid_abundance_5_day_mean_soil_conductivity_spring_p_value <- all_MAP_aphid_abundance_5_day_mean_soil_conductivity_spring_correlation_test$p.value


#daily mean soil moisture
all_MAP_aphid_abundance_5_day_mean_soil_conductivity_fall_correlation_test <- cor.test(df_new_fall$resid_MAP,
                                                                                       df_new_fall$resid_MAP_5_day_ec,
                                                                                       use = "complete.obs")
# Extract the correlation coefficient and p-value
all_MAP_aphid_abundance_5_day_mean_soil_conductivity_fall_correlation_coefficient <- all_MAP_aphid_abundance_5_day_mean_soil_conductivity_fall_correlation_test$estimate
all_MAP_aphid_abundance_5_day_mean_soil_conductivity_fall_p_value <- all_MAP_aphid_abundance_5_day_mean_soil_conductivity_fall_correlation_test$p.value



# Scatter plot with a linear trend line
plot_all_MAP_5_day_mean_soil_conductivity_aphid_abundance_season <- 
  ggplot(df_new,
         aes(x = resid_MAP_5_day_ec,
             y = resid_MAP,
             color = `growing season`)) + 
  geom_point(aes(fill = after_scale(scales::alpha(color, 0.30))),
             stroke = 0.2, shape = 21, size = 0.75) + 
  # 設定回歸線顏色，根據不同的 'growing season' 來顯示不同顏色
  geom_smooth(data = df_new %>% filter(`growing season` == "2024 spring"), method = "lm", se = TRUE,
              color = "#006600",  # 深綠色回歸線
              fill = "#66CC66",   # 淺綠色信賴區間
              alpha = 0.4, size = 0.5) +   # 信賴區間透明度
  geom_smooth(data = df_new %>% filter(`growing season` == "2024 fall"), method = "lm", se = TRUE,
              color = "#8B4513",  # 紅棕色回歸線
              fill = "#C4B7A6",   # 淺咖啡信賴區間
              alpha = 0.4, size = 0.5) +   # 信賴區間透明度
  labs(title = "",
       x = "Prev. 5-D EC ~ MAP",
       y = expression( ~ log[10](Aphids + 1)/m^2 ~ " ~ MAP")) +
  theme_classic(base_size = 8, base_family = "sans") +
  theme(plot.title=element_text(size=8, face="bold",hjust = 0.5),
        axis.title=element_text(size=8),
        axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x = element_text(size =6),
        axis.text.y = element_text(size = 6),
        legend.position= "none",        # 顯示圖例
        legend.key.size = unit(12,"points"),
        legend.title = element_blank()) +
  coord_cartesian(clip = "off") +
  # 資料點顏色設置，確保資料點顏色正確
  scale_color_manual(values = c("2024 spring" = "#006600",  # 深綠
                                "2024 fall"   = "#867052")) +  # 深咖啡
  # 區間顏色設置，確保資料的顏色不會被覆蓋
  scale_fill_manual(values = c("2024 spring" = "#66CC66",  # 淺綠
                               "2024 fall"   = "#C4B7A6")) +  # 淺咖啡
  annotate("text", x = -0.1, y = -2.4,
           label = paste0("r = ",
                          round(all_MAP_aphid_abundance_5_day_mean_soil_conductivity_spring_correlation_coefficient, 3),
                          ", p = ",
                          signif(all_MAP_aphid_abundance_5_day_mean_soil_conductivity_spring_p_value, 3)),
           hjust = 0, vjust = 0, size = 2.5, color = "#006600") +
  annotate("text", x = -0.1, y = -2.8,
           label = paste0("r = ",
                          round(all_MAP_aphid_abundance_5_day_mean_soil_conductivity_fall_correlation_coefficient, 3),
                          ", p = ",
                          signif(all_MAP_aphid_abundance_5_day_mean_soil_conductivity_fall_p_value, 3)),
           hjust = 0, vjust = 0, size = 2.5, color = "#8B4513") +
  scale_x_continuous(limits = c(-0.14, 1.4)) +
  scale_y_continuous(limits = c(-3.0, 3.0))

# 7-day mean soil conductivity
all_MAP_aphid_abundance_7_day_mean_soil_conductivity_correlation_test <- cor.test(df_new$resid_MAP,
                                                                                  df_new$resid_MAP_7_day_ec,
                                                                                  use = "complete.obs")
# Extract the correlation coefficient and p-value
all_MAP_aphid_abundance_7_day_mean_soil_conductivity_correlation_coefficient <- all_MAP_aphid_abundance_7_day_mean_soil_conductivity_correlation_test$estimate
all_MAP_aphid_abundance_7_day_mean_soil_conductivity_p_value <- all_MAP_aphid_abundance_7_day_mean_soil_conductivity_correlation_test$p.value

max(df_new$resid_MAP_7_day_ec)
min(df_new$resid_MAP_7_day_ec)

plot_all_MAP_7_day_mean_soil_conductivity_aphid_abundance <- 
  ggplot(df_new,
         aes(x = resid_MAP_7_day_ec,
             y = resid_MAP,
             color = `growing season`)) + 
  geom_point(aes(fill = after_scale(scales::alpha(color, 0.30))),
             stroke = 0.2, shape = 21, size = 0.75) +
  geom_smooth(method = "lm", se = TRUE,col = "blue", aes(group = 1),
              alpha = 0.3, size = 0.5) +   # 信賴區間透明度
  
  labs(title = "",
       x = "Prev. 7-D EC ~ MAP",
       y = expression( ~ log[10](Aphids + 1)/m^2 ~ " ~ MAP" )) +
  theme_classic(base_size = 8, base_family = "sans") +
  theme(plot.title=element_text(size=8, face="bold",hjust = 0.5),
        axis.title=element_text(size=8),
        axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x = element_text(size =6),
        axis.text.y = element_text(size = 6),
        legend.position= "none",        # 顯示圖例
        legend.key.size = unit(12,"points"),
        legend.title = element_blank()) +
  coord_cartesian(clip = "off") +
  scale_color_manual(values = c("2024 spring" = "#006600",  # 深綠
                                "2024 fall"   = "#867052")) +  # 深咖啡
  
  annotate("text", x = -0.08, y = -2.6,
           label = paste0("r = ",
                          round(all_MAP_aphid_abundance_7_day_mean_soil_conductivity_correlation_coefficient, 3),
                          ", p = ",
                          signif(all_MAP_aphid_abundance_7_day_mean_soil_conductivity_p_value, 3)),
           hjust = 0, vjust = 0, size = 2.5, color = "black")  +
  scale_x_continuous(limits = c(-0.13, 1.0)) +
  scale_y_continuous(limits = c(-3.0, 3.0))

#daily mean soil EC
all_MAP_aphid_abundance_7_day_mean_soil_conductivity_spring_correlation_test <- cor.test(df_new_spring$resid_MAP,
                                                                                         df_new_spring$resid_MAP_7_day_ec,
                                                                                         use = "complete.obs")
# Extract the correlation coefficient and p-value
all_MAP_aphid_abundance_7_day_mean_soil_conductivity_spring_correlation_coefficient <- all_MAP_aphid_abundance_7_day_mean_soil_conductivity_spring_correlation_test$estimate
all_MAP_aphid_abundance_7_day_mean_soil_conductivity_spring_p_value <- all_MAP_aphid_abundance_7_day_mean_soil_conductivity_spring_correlation_test$p.value


#daily mean soil moisture
all_MAP_aphid_abundance_7_day_mean_soil_conductivity_fall_correlation_test <- cor.test(df_new_fall$resid_MAP,
                                                                                       df_new_fall$resid_MAP_7_day_ec,
                                                                                       use = "complete.obs")
# Extract the correlation coefficient and p-value
all_MAP_aphid_abundance_7_day_mean_soil_conductivity_fall_correlation_coefficient <- all_MAP_aphid_abundance_7_day_mean_soil_conductivity_fall_correlation_test$estimate
all_MAP_aphid_abundance_7_day_mean_soil_conductivity_fall_p_value <- all_MAP_aphid_abundance_7_day_mean_soil_conductivity_fall_correlation_test$p.value



# Scatter plot with a linear trend line
plot_all_MAP_7_day_mean_soil_conductivity_aphid_abundance_season <- 
  ggplot(df_new,
         aes(x = resid_MAP_7_day_ec,
             y = resid_MAP,
             color = `growing season`)) + 
  geom_point(aes(fill = after_scale(scales::alpha(color, 0.30))),
             stroke = 0.2, shape = 21, size = 0.75) + 
  # 設定回歸線顏色，根據不同的 'growing season' 來顯示不同顏色
  geom_smooth(data = df_new %>% filter(`growing season` == "2024 spring"), method = "lm", se = TRUE,
              color = "#006600",  # 深綠色回歸線
              fill = "#66CC66",   # 淺綠色信賴區間
              alpha = 0.4, size = 0.5) +   # 信賴區間透明度
  geom_smooth(data = df_new %>% filter(`growing season` == "2024 fall"), method = "lm", se = TRUE,
              color = "#8B4513",  # 紅棕色回歸線
              fill = "#C4B7A6",   # 淺咖啡信賴區間
              alpha = 0.4, size = 0.5) +   # 信賴區間透明度
  labs(title = "",
       x = "Prev. 7-D EC ~ MAP",
       y = expression( ~ log[10](Aphids + 1)/m^2 ~ " ~ MAP")) +
  theme_classic(base_size = 8, base_family = "sans") +
  theme(plot.title=element_text(size=8, face="bold",hjust = 0.5),
        axis.title=element_text(size=8),
        axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x = element_text(size =6),
        axis.text.y = element_text(size = 6),
        legend.position= "none",        # 顯示圖例
        legend.key.size = unit(12,"points"),
        legend.title = element_blank()) +
  coord_cartesian(clip = "off") +
  # 資料點顏色設置，確保資料點顏色正確
  scale_color_manual(values = c("2024 spring" = "#006600",  # 深綠
                                "2024 fall"   = "#867052")) +  # 深咖啡
  # 區間顏色設置，確保資料的顏色不會被覆蓋
  scale_fill_manual(values = c("2024 spring" = "#66CC66",  # 淺綠
                               "2024 fall"   = "#C4B7A6")) +  # 淺咖啡
  annotate("text", x = -0.08, y = -2.4,
           label = paste0("r = ",
                          round(all_MAP_aphid_abundance_7_day_mean_soil_conductivity_spring_correlation_coefficient, 3),
                          ", p = ",
                          signif(all_MAP_aphid_abundance_7_day_mean_soil_conductivity_spring_p_value, 3)),
           hjust = 0, vjust = 0, size = 2.5, color = "#006600") +
  annotate("text", x = -0.08, y = -2.8,
           label = paste0("r = ",
                          round(all_MAP_aphid_abundance_7_day_mean_soil_conductivity_fall_correlation_coefficient, 3),
                          ", p = ",
                          signif(all_MAP_aphid_abundance_7_day_mean_soil_conductivity_fall_p_value, 3)),
           hjust = 0, vjust = 0, size = 2.5, color = "#8B4513") +
  scale_x_continuous(limits = c(-0.13, 1.0)) +
  scale_y_continuous(limits = c(-3.0, 3.0))

#--------------------------------------------------------------------------------------------------------------------

# raw, WAP, stage, MAP

plot_all_daily_soil_moisture <- plot_grid(
  plot_all_raw_daily_mean_soil_moisture_aphid_abundance,
  plot_all_WAP_daily_mean_soil_moisture_aphid_abundance,
  
  plot_all_MAP_daily_mean_soil_moisture_aphid_abundance,
  plot_all_raw_daily_mean_soil_moisture_aphid_abundance_seasons,
  plot_all_WAP_daily_mean_soil_moisture_aphid_abundance_season,

  plot_all_MAP_daily_mean_soil_moisture_aphid_abundance_season,
  ncol = 3, labels = c("A","B","C","D","E","F"),
  label_size = 10,          # ← 用這個，而不是 size
  label_fontface = "bold")


ggsave("plot_all_daily_soil_moisture_2025_10_21.png", 
       plot = plot_all_daily_soil_moisture, 
       width = 16, height = 10, units = "cm", dpi = 300)

# 加上文字並保持原圖大小 (16x12 cm)，並增加 2 cm 底部空間
library(cowplot)



# 3-day moisture
plot_all_1_day_soil_moisture <- plot_grid(
  plot_all_raw_1_day_mean_soil_moisture_aphid_abundance,
  plot_all_WAP_1_day_mean_soil_moisture_aphid_abundance,
  
  plot_all_MAP_1_day_mean_soil_moisture_aphid_abundance,
  plot_all_raw_1_day_mean_soil_moisture_aphid_abundance_seasons,
  plot_all_WAP_1_day_mean_soil_moisture_aphid_abundance_season,
  
  plot_all_MAP_1_day_mean_soil_moisture_aphid_abundance_season,
  ncol = 3, labels = c("A","B","C","D","E","F"),
  label_size = 10,          # ← 用這個，而不是 size
  label_fontface = "bold")


ggsave("plot_all_1_day_soil_moisture_2025_10_21.png", 
       plot = plot_all_1_day_soil_moisture, 
       width = 16, height = 10, units = "cm", dpi = 300)




# 3-day moisture
plot_all_3_day_soil_moisture <- plot_grid(
  plot_all_raw_3_day_mean_soil_moisture_aphid_abundance,
  plot_all_WAP_3_day_mean_soil_moisture_aphid_abundance,
  
  plot_all_MAP_3_day_mean_soil_moisture_aphid_abundance,
  plot_all_raw_3_day_mean_soil_moisture_aphid_abundance_seasons,
  plot_all_WAP_3_day_mean_soil_moisture_aphid_abundance_season,
  
  plot_all_MAP_3_day_mean_soil_moisture_aphid_abundance_season,
  ncol = 3, labels = c("A","B","C","D","E","F"),
  label_size = 10,          # ← 用這個，而不是 size
  label_fontface = "bold")


ggsave("plot_all_3_day_soil_moisture_2025_10_21.png", 
       plot = plot_all_3_day_soil_moisture, 
       width = 16, height = 10, units = "cm", dpi = 300)



# 5-day moisture
plot_all_5_day_soil_moisture <- plot_grid(
  plot_all_raw_5_day_mean_soil_moisture_aphid_abundance,
  plot_all_WAP_5_day_mean_soil_moisture_aphid_abundance,
 
  plot_all_MAP_5_day_mean_soil_moisture_aphid_abundance,
  plot_all_raw_5_day_mean_soil_moisture_aphid_abundance_seasons,
  plot_all_WAP_5_day_mean_soil_moisture_aphid_abundance_season,
  
  plot_all_MAP_5_day_mean_soil_moisture_aphid_abundance_season,
  ncol = 3, labels = c("A","B","C","D","E","F"),
  label_size = 10,          # ← 用這個，而不是 size
  label_fontface = "bold")


ggsave("plot_all_5_day_soil_moisture_2025_10_21.png", 
       plot = plot_all_5_day_soil_moisture, 
       width = 16, height = 10, units = "cm", dpi = 300)

# 加上文字並保持原圖大小 (16x12 cm)，並增加 2 cm 底部空間
library(cowplot)

plot_all_5_day_soil_moisture_with_text <- ggdraw(plot_all_5_day_soil_moisture) +
  draw_label("Previous 5-day mean Soil Moisture (%)",
             x = 0.5, y = -0.05, vjust = 0, size = 12, fontface = "bold") +  # 調整文字位置
  theme(plot.margin = margin(0, 0, 1, 0, "cm"))  # 底部留 2 cm 空間給文字

# 儲存圖形，保持原圖大小 (16x12 cm)，並讓總高度為 16x14 cm
ggsave("plot_all_5_day_soil_moisture_with_text_2025_10_04.png", 
       plot = plot_all_5_day_soil_moisture_with_text, 
       width = 16, height = 11, units = "cm", dpi = 300)

# 7-day moisture
plot_all_7_day_soil_moisture <- plot_grid(
  plot_all_raw_7_day_mean_soil_moisture_aphid_abundance,
  plot_all_WAP_7_day_mean_soil_moisture_aphid_abundance,
  
  plot_all_MAP_7_day_mean_soil_moisture_aphid_abundance,
  plot_all_raw_7_day_mean_soil_moisture_aphid_abundance_seasons,
  plot_all_WAP_7_day_mean_soil_moisture_aphid_abundance_season,
  
  plot_all_MAP_7_day_mean_soil_moisture_aphid_abundance_season,
  ncol = 3, labels = c("A","B","C","D","E","F"),
  label_size = 10,          # ← 用這個，而不是 size
  label_fontface = "bold")


ggsave("plot_all_7_day_soil_moisture_2025_10_21.png", 
       plot = plot_all_7_day_soil_moisture, 
       width = 16, height = 10, units = "cm", dpi = 300)

# 加上文字並保持原圖大小 (16x12 cm)，並增加 2 cm 底部空間
library(cowplot)

plot_all_7_day_soil_moisture_with_text <- ggdraw(plot_all_7_day_soil_moisture) +
  draw_label("Previous 7-day mean Soil Moisture (%)",
             x = 0.5, y = -0.05, vjust = 0, size = 12, fontface = "bold") +  # 調整文字位置
  theme(plot.margin = margin(0, 0, 1, 0, "cm"))  # 底部留 2 cm 空間給文字

# 儲存圖形，保持原圖大小 (16x12 cm)，並讓總高度為 16x14 cm
ggsave("plot_all_7_day_soil_moisture_with_text_2025_10_04.png", 
       plot = plot_all_7_day_soil_moisture_with_text, 
       width = 16, height = 11, units = "cm", dpi = 300)

# daily EC
plot_all_daily_soil_EC <- plot_grid(
  plot_all_raw_daily_mean_soil_conductivity_aphid_abundance,
  plot_all_WAP_daily_mean_soil_conductivity_aphid_abundance,
  
  plot_all_MAP_daily_mean_soil_conductivity_aphid_abundance,
  plot_all_raw_daily_mean_soil_conductivity_aphid_abundance_season,
  plot_all_WAP_daily_mean_soil_conductivity_aphid_abundance_season,
  
  plot_all_MAP_daily_mean_soil_conductivity_aphid_abundance_season,
  ncol = 3, labels = c("A","B","C","D","E","F"),
  label_size = 10,          # ← 用這個，而不是 size
  label_fontface = "bold")


ggsave("plot_all_daily_soil_EC_2025_10_21.png", 
       plot = plot_all_daily_soil_EC, 
       width = 16, height = 10, units = "cm", dpi = 300)

# 加上文字並保持原圖大小 (16x12 cm)，並增加 2 cm 底部空間
library(cowplot)

plot_all_daily_soil_EC_with_text <- ggdraw(plot_all_daily_soil_EC) +
  draw_label("Daily mean Soil EC (dS/m)",
             x = 0.5, y = -0.05, vjust = 0, size = 12, fontface = "bold") +  # 調整文字位置
  theme(plot.margin = margin(0, 0, 1, 0, "cm"))  # 底部留 2 cm 空間給文字

# 儲存圖形，保持原圖大小 (16x12 cm)，並讓總高度為 16x14 cm
ggsave("plot_all_daily_soil_EC_with_text_2025_10_04.png", 
       plot = plot_all_daily_soil_EC_with_text, 
       width = 16, height = 11, units = "cm", dpi = 300)

# 1-day EC
plot_all_1_day_soil_EC <- plot_grid(
  plot_all_raw_1_day_mean_soil_conductivity_aphid_abundance,
  plot_all_WAP_1_day_mean_soil_conductivity_aphid_abundance,
 
  plot_all_MAP_1_day_mean_soil_conductivity_aphid_abundance,
  plot_all_raw_1_day_mean_soil_conductivity_aphid_abundance_season,
  plot_all_WAP_1_day_mean_soil_conductivity_aphid_abundance_season,
 
  plot_all_MAP_1_day_mean_soil_conductivity_aphid_abundance_season,
  ncol = 3, labels = c("A","B","C","D","E","F"),
  label_size = 10,          # ← 用這個，而不是 size
  label_fontface = "bold")

ggsave("plot_all_1_day_soil_EC_2025_10_21.png", 
       plot = plot_all_1_day_soil_EC, 
       width = 16, height = 10, units = "cm", dpi = 300)




# 3-day EC
plot_all_3_day_soil_EC <- plot_grid(
  plot_all_raw_3_day_mean_soil_conductivity_aphid_abundance,
  plot_all_WAP_3_day_mean_soil_conductivity_aphid_abundance,
  
  plot_all_MAP_3_day_mean_soil_conductivity_aphid_abundance,
  plot_all_raw_3_day_mean_soil_conductivity_aphid_abundance_season,
  plot_all_WAP_3_day_mean_soil_conductivity_aphid_abundance_season,
  
  plot_all_MAP_3_day_mean_soil_conductivity_aphid_abundance_season,
  ncol = 3, labels = c("A","B","C","D","E","F"),
  label_size = 10,          # ← 用這個，而不是 size
  label_fontface = "bold")


ggsave("plot_all_3_day_soil_EC_2025_10_21.png", 
       plot = plot_all_3_day_soil_EC, 
       width = 16, height = 10, units = "cm", dpi = 300)




# 5-day EC
plot_all_5_day_soil_EC <- plot_grid(
  plot_all_raw_5_day_mean_soil_conductivity_aphid_abundance,
  plot_all_WAP_5_day_mean_soil_conductivity_aphid_abundance,
  
  plot_all_MAP_5_day_mean_soil_conductivity_aphid_abundance,
  plot_all_raw_5_day_mean_soil_conductivity_aphid_abundance_season,
  plot_all_WAP_5_day_mean_soil_conductivity_aphid_abundance_season,
  
  plot_all_MAP_5_day_mean_soil_conductivity_aphid_abundance_season,
  ncol = 3, labels = c("A","B","C","D","E","F"),
  label_size = 10,          # ← 用這個，而不是 size
  label_fontface = "bold")


ggsave("plot_all_5_day_soil_EC_2025_10_21.png", 
       plot = plot_all_5_day_soil_EC, 
       width = 16, height = 10, units = "cm", dpi = 300)




# 7-day EC
plot_all_7_day_soil_EC <- plot_grid(
  plot_all_raw_7_day_mean_soil_conductivity_aphid_abundance,
  plot_all_WAP_7_day_mean_soil_conductivity_aphid_abundance,
  
  plot_all_MAP_7_day_mean_soil_conductivity_aphid_abundance,
  plot_all_raw_7_day_mean_soil_conductivity_aphid_abundance_season,
  plot_all_WAP_7_day_mean_soil_conductivity_aphid_abundance_season,
  
  plot_all_MAP_7_day_mean_soil_conductivity_aphid_abundance_season,
  ncol = 3, labels = c("A","B","C","D","E","F"),
  label_size = 10,          # ← 用這個，而不是 size
  label_fontface = "bold")


ggsave("plot_all_7_day_soil_EC_2025_10_21.png", 
       plot = plot_all_7_day_soil_EC, 
       width = 16, height = 10, units = "cm", dpi = 300)

# 加上文字並保持原圖大小 (16x12 cm)，並增加 2 cm 底部空間
library(cowplot)

plot_all_7_day_soil_EC_with_text <- ggdraw(plot_all_7_day_soil_EC) +
  draw_label("Previous 7-day mean Soil EC (dS/m)",
             x = 0.5, y = -0.05, vjust = 0, size = 12, fontface = "bold") +  # 調整文字位置
  theme(plot.margin = margin(0, 0, 1, 0, "cm"))  # 底部留 2 cm 空間給文字

# 儲存圖形，保持原圖大小 (16x12 cm)，並讓總高度為 16x14 cm
ggsave("plot_all_7_day_soil_EC_with_text_2025_10_04.png", 
       plot = plot_all_7_day_soil_EC_with_text, 
       width = 16, height = 11, units = "cm", dpi = 300)



