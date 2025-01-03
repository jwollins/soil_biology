### Earthworm data
### J Collins 
### 2024-06-01
###

setwd(dir = "~/Documents/GitHub/soil_biology/")

# 00 PACKAGES ####

source(file = "earthworm_scripts/01_packages.R")

# 01 DATA ####

source(file = "~/Documents/GitHub/soil_biology/earthworm_scripts/02_data.R")

setwd(dir = "~/OneDrive - Harper Adams University/Data/Soil/worms/")

## 03 PLOTS ####


### 03.1 packages ####

# source(file = "scripts/01_packages.R")
# 
# 
# 
# ### 03.2 Data ####
# 
# source(file = "scripts/02_data.R")


### 03.3 Plots  ####



#### 03.4 Juviniles  ####

# this is the legend title with correct notation
title_exp <- expression(Juvinile~Earthworms~(m^{-2})) 
y_title <- expression(Abundance~(m^{-2})) 

a <- ggplot(data = juvinile_sum, 
       aes(x = treatment, 
           y = mean, 
           fill = treatment)) + 
  geom_bar(stat = "identity", 
           color = "black", 
           position = "dodge") + 
  labs(
    x = element_blank(),
    y = y_title,
    subtitle = title_exp, 
    caption = element_blank()) +
  theme_bw() +
  scale_fill_manual(values=c("tomato2", "turquoise3"), 
                    name = "Treatment") +
  theme(strip.text.x = element_text(size = 12, 
                                    color = "black", 
                                    face = "bold.italic"), 
        axis.text.x=element_blank(),
        legend.position = "bottom") +
  geom_errorbar(aes(ymin=mean-se,
                    ymax=mean+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  # geom_signif(
  #   data = subset(juvinile_sum, year == "2022"), # Subset data for Crop1
  #   comparisons = list(c("Conventional", "Conservation")),
  #   map_signif_level = TRUE,
  #   textsize = 4,
  #   tip_length = 0.01, 
  #   annotations = "NS.", 
  #   y_position = c(300) # Adjust y-position if necessary
  # ) +
  # geom_signif(
  #   data = subset(juvinile_sum, year == "2023"), # Subset data for Crop2
  #   comparisons = list(c("Conventional", "Conservation")),
  #   map_signif_level = TRUE,
  #   textsize = 4,
  #   tip_length = 0.01,
  #   annotations = "NS.",
  #   y_position = c(300) # Adjust y-position if necessary
  # ) +
  facet_wrap(~ year, ncol = 3)


# ggsave(filename = "juvinile_m2_plot.png", 
#        path = "plots/", 
#        width = 6, 
#        height = 6)



#### 03.5 Epigeic  ####

# this is the legend title with correct notation
title_exp <- expression(Epigeic~Earthworms~(m^{-2})) 

b <- ggplot(data = epigeic_sum, 
       aes(x = treatment, 
           y = mean, 
           fill = treatment)) + 
  geom_bar(stat = "identity", 
           color = "black", 
           position = "dodge") + 
  labs(
    x = element_blank(),
    y = y_title,
    subtitle = title_exp, 
    caption = element_blank()) +
  theme_bw() +
  scale_fill_manual(values=c("tomato2", "turquoise3"), 
                    name = "Treatment") +
  theme(strip.text.x = element_text(size = 12, 
                                    color = "black", 
                                    face = "bold.italic"), 
        axis.text.x=element_blank(),
        legend.position = "bottom") +
  geom_errorbar(aes(ymin=mean-se, 
                    ymax=mean+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  # geom_signif(
  #   data = subset(juvinile_sum, year == "2022"), # Subset data for Crop1
  #   comparisons = list(c("Conventional", "Conservation")),
  #   map_signif_level = TRUE,
  #   textsize = 4,
  #   tip_length = 0.01, 
  #   annotations = "NS.", 
  #   fontface = 'italic', 
  #   y_position = c(85) # Adjust y-position if necessary
  # ) +
  # geom_signif(
  #   data = subset(juvinile_sum, year == "2023"), # Subset data for Crop2
  #   comparisons = list(c("Conventional", "Conservation")),
  #   map_signif_level = TRUE,
  #   textsize = 4,
  #   tip_length = 0.01,
  #   annotations = "NS.",
  #   fontface = 'italic', 
  #   y_position = c(85) # Adjust y-position if necessary
  # ) +
  facet_wrap(~ year, ncol = 3)

b

# ggsave(filename = "epigeic_m2_plot.png", 
#        path = "plots/", 
#        width = 6, 
#        height = 6)










#### 03.6 Anecic  ####

# this is the legend title with correct notation
title_exp <- expression(Anecic~Earthworms~(m^{-2})) 

c <- ggplot(data = anecic_sum, 
       aes(x = treatment, 
           y = mean, 
           fill = treatment)) + 
  geom_bar(stat = "identity", 
           color = "black", 
           position = "dodge") + 
  labs(
    x = element_blank(),
    y = y_title,
    subtitle = title_exp, 
    caption = element_blank()) +
  theme_bw() +
  scale_fill_manual(values=c("tomato2", "turquoise3"), 
                    name = "Treatment") +
  theme(strip.text.x = element_text(size = 12, 
                                    color = "black", 
                                    face = "bold.italic"), 
        axis.text.x=element_blank(),
        legend.position = "bottom") +
  geom_errorbar(aes(ymin=mean-se, 
                    ymax=mean+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  # geom_signif(
  #   data = subset(juvinile_sum, year == "2022"), # Subset data for Crop1
  #   comparisons = list(c("Conventional", "Conservation")),
  #   map_signif_level = TRUE,
  #   textsize = 4,
  #   tip_length = 0.01, 
  #   annotations = "NS", 
  #   fontface = 'italic', 
  #   y_position = c(35) # Adjust y-position if necessary
  # ) +
  # geom_signif(
  #   data = subset(juvinile_sum, year == "2023"), # Subset data for Crop2
  #   comparisons = list(c("Conventional", "Conservation")),
  #   map_signif_level = TRUE,
  #   textsize = 4,
  #   tip_length = 0.01,
  #   annotations = "NS.",
  #   fontface = 'italic', 
  #   y_position = c(35) # Adjust y-position if necessary
  # ) +
  facet_wrap(~ year, ncol = 3)

# ggsave(filename = "anecic_m2_plot.png", 
#        path = "plots/", 
#        width = 6, 
#        height = 6)




#### 03.6 Endogeic  ####

# this is the legend title with correct notation
title_exp <- expression(Endogeic~Earthworms~(m^{-2})) 

d <- ggplot(data = endogeic_sum, 
       aes(x = treatment, 
           y = mean, 
           fill = treatment)) + 
  geom_bar(stat = "identity", 
           color = "black", 
           position = "dodge") + 
  labs(
    x = element_blank(),
    y = y_title,
    subtitle = title_exp, 
    caption = element_blank()) +
  theme_bw() +
  scale_fill_manual(values=c("tomato2", "turquoise3"), 
                    name = "Treatment") +
  theme(strip.text.x = element_text(size = 12, 
                                    color = "black", 
                                    face = "bold.italic"), 
        axis.text.x=element_blank(),
        legend.position = "bottom") +
  geom_errorbar(aes(ymin=mean-se, 
                    ymax=mean+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  # geom_signif(
  #   data = subset(juvinile_sum, year == "2022"), # Subset data for Crop1
  #   comparisons = list(c("Conventional", "Conservation")),
  #   map_signif_level = TRUE,
  #   textsize = 4,
  #   tip_length = 0.01, 
  #   annotations = "NS",
  #   fontface = 'italic', 
  #   y_position = c(100) # Adjust y-position if necessary
  # ) +
  # geom_signif(
  #   data = subset(juvinile_sum, year == "2023"), # Subset data for Crop2
  #   comparisons = list(c("Conventional", "Conservation")),
  #   map_signif_level = TRUE,
  #   textsize = 4,
  #   tip_length = 0.01,
  #   annotations = "NS.",
  #   fontface = 'italic', 
  #   y_position = c(100) # Adjust y-position if necessary
  # ) +
  facet_wrap(~ year, ncol = 3)

# ggsave(filename = "endogeic_m2_plot.png", 
#        path = "plots/", 
#        width = 6, 
#        height = 6)









#### 03.6 Total Quantity ####

# this is the legend title with correct notation
title_exp <- expression(Total~Earthworms~(m^{-2})) 

e <- ggplot(data = total_worms_m2_sum, 
       aes(x = treatment, 
           y = mean, 
           fill = treatment)) + 
  geom_bar(stat = "identity", 
           color = "black", 
           position = "dodge") + 
  labs(
    x = element_blank(),
    y = y_title,
    subtitle = title_exp, 
    caption = element_blank()) +
  theme_bw() +
  scale_fill_manual(values=c("tomato2", "turquoise3"), 
                    name = "Treatment") +
  theme(strip.text.x = element_text(size = 12, 
                                    color = "black", 
                                    face = "bold.italic"), 
        axis.text.x=element_blank(),
        legend.position = "bottom") +
  geom_errorbar(aes(ymin=mean-se, 
                    ymax=mean+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  # geom_signif(
  #   data = subset(juvinile_sum, year == "2022"), # Subset data for Crop1
  #   comparisons = list(c("Conventional", "Conservation")),
  #   map_signif_level = TRUE,
  #   textsize = 4,
  #   tip_length = 0.01, 
  #   annotations = "NS", 
  #   fontface = 'italic', 
  #   y_position = c(500) # Adjust y-position if necessary
  # ) +
  # geom_signif(
  #   data = subset(juvinile_sum, year == "2023"), # Subset data for Crop2
  #   comparisons = list(c("Conventional", "Conservation")),
  #   map_signif_level = TRUE,
  #   textsize = 4,
  #   tip_length = 0.01,
  #   annotations = "NS.",
  #   fontface = 'italic',
  #   y_position = c(500) # Adjust y-position if necessary
  # ) +
  facet_wrap(~ year, ncol = 3)

# ggsave(filename = "total_worms_m2_plot.png", 
#        path = "plots/", 
#        width = 6, 
#        height = 6)






#### 03.6 Total Mass g ####

# this is the legend title with correct notation
title_exp <- expression(Total~Mass~Earthworms~(g~m^{-2})) 
y_title <- expression(Mass~(g~m^{-2}))


f <- ggplot(data = total_mass_g_m2_sum, 
       aes(x = treatment, 
           y = mean, 
           fill = treatment)) + 
  geom_bar(stat = "identity", 
           color = "black", 
           position = "dodge") + 
  labs(
    x = element_blank(),
    y = y_title,
    subtitle = title_exp, 
    caption = element_blank()) +
  theme_bw() +
  scale_fill_manual(values=c("tomato2", "turquoise3"), 
                    name = "Treatment") +
  theme(strip.text.x = element_text(size = 12, 
                                    color = "black", 
                                    face = "bold.italic"), 
        axis.text.x=element_blank(),
        legend.position = "bottom") +
  geom_errorbar(aes(ymin=mean-se,
                    ymax=mean+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  # geom_signif(
  #   data = subset(juvinile_sum, year == "2022"), # Subset data for Crop1
  #   comparisons = list(c("Conventional", "Conservation")),
  #   map_signif_level = TRUE,
  #   textsize = 4,
  #   tip_length = 0.01, 
  #   annotations = "NS", 
  #   fontface = 'italic',
  #   y_position = c(100) # Adjust y-position if necessary
  # ) +
  # geom_signif(
  #   data = subset(juvinile_sum, year == "2023"), # Subset data for Crop2
  #   comparisons = list(c("Conventional", "Conservation")),
  #   map_signif_level = TRUE,
  #   textsize = 4,
  #   tip_length = 0.01,
  #   annotations = "NS.",
  #   fontface = 'italic',
  #   y_position = c(100) # Adjust y-position if necessary
  # ) +
  facet_wrap(~ year, ncol = 3)

# ggsave(filename = "total_mass_g_m2_plot.png", 
#        path = "plots/", 
#        width = 6, 
#        height = 6)


# combined plot ####

ggarrange(a,b,c,d,e,f, 
          ncol = 3, nrow = 2, 
          common.legend = TRUE, 
          legend = "bottom", 
          labels = c("A", "B", "C", "D", "E", "F"))

ggsave(filename = "plots/worms_fig_plot.png", width = 10, height = 6)

