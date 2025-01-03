# 08 FIGURES ####
## WHO: JOE COLLINS
## WHAT: QBS data normality test and distribution plots
## LAST EDIT: 2024-10-16
####

setwd(dir = "~/Documents/GitHub/soil_biology/")

# 00 PACKAGES ####

source(file = "qbs_scripts/02 packages.R")

# 01 DATA ####

source(file = "qbs_scripts/03 qbs.data.R")

setwd(dir = "~/OneDrive - Harper Adams University/Data/QBS/")

# create a directory to save the plots to
dir.create(path = "Plots/figures/")



## 01 FIG 1 ####

### 01 A qbs-ar ####

fig1a <- ggplot(data = qbs_sum, 
       aes(x = Treatment, 
           y = mean, 
           fill = Treatment)) + 
  geom_bar(stat = "identity", 
           color = "black", 
           position = "dodge") + 
  labs(
    x = "Treatment",
    y = "Mean QBS-ar Score",
    subtitle = "Mean QBS-ar Score", 
    caption = "") +
  theme_bw() +
  scale_fill_manual(values = c("turquoise3", "tomato2"), 
                    name = "Treatment") +
  theme(strip.text.x = element_text(size = 12, 
                                    color = "black", 
                                    face = "bold.italic"), 
        axis.text.x=element_blank(),
        legend.position = "bottom") +
  geom_errorbar(aes(ymin = mean - se, 
                    ymax = mean + se),
                width = .2,                    # Width of the error bars
                position = position_dodge(.9)) +
  # geom_signif(
  #   data = subset(qbs_sum, Year == 2022), # Subset data for Crop1
  #   comparisons = list(c("Conventional", "Conservation")),
  #   map_signif_level = TRUE,
  #   textsize = 4,
  #   tip_length = 0.01, 
  #   annotations = "NS.", 
  #   fontface = 'italic', 
  #   y_position = c(120) # Adjust y-position if necessary
  # ) +
  # geom_signif(
  #   data = subset(qbs_sum, Year == 2023), # Subset data for Crop2
  #   comparisons = list(c("Conventional", "Conservation")),
  #   map_signif_level = TRUE,
  #   textsize = 4,
  #   tip_length = 0.01,
  #   annotations = "p = 0.02",
  #   fontface = 'italic', 
  #   y_position = c(120) # Adjust y-position if necessary
  # ) +
  # geom_signif(
  #   data = subset(qbs_sum, Year == 2024), # Subset data for Crop2
  #   comparisons = list(c("Conventional", "Conservation")),
  #   map_signif_level = TRUE,
  #   textsize = 4,
  #   tip_length = 0.01,
  #   annotations = "NS.",
  #   fontface = 'italic', 
  #   y_position = c(120) # Adjust y-position if necessary
  # ) +
  facet_wrap(~ Year, 
             ncol = 4, 
             scales = 'free_x')

fig1a


# Save the plot to a file, using the column name in the file name
file_name <- paste0("plots/figures/", "qbs_ar_score", "_fig.png")
ggsave(filename = file_name, width = 8, height = 5, plot = fig1a)




### 01 B qbs-c ####

fig1b <- ggplot(data = qbs_c_sum, 
               aes(x = Treatment, 
                   y = mean, 
                   fill = Treatment)) + 
  geom_bar(stat = "identity", 
           color = "black", 
           position = "dodge") + 
  labs(
    x = "Treatment",
    y = "Mean QBS-c Score",
    subtitle = "Mean QBS-c Score", 
    caption = "") +
  theme_bw() +
  scale_fill_manual(values = c("turquoise3", "tomato2"), 
                    name = "Treatment") +
  theme(strip.text.x = element_text(size = 12, 
                                    color = "black", 
                                    face = "bold.italic"), 
        axis.text.x=element_blank(),
        legend.position = "bottom") +
  geom_errorbar(aes(ymin = mean - se, 
                    ymax = mean + se),
                width = .2,                    # Width of the error bars
                position = position_dodge(.9)) +
  # geom_signif(
  #   data = subset(qbs_sum, Year == 2022), # Subset data for Crop1
  #   comparisons = list(c("Conventional", "Conservation")),
  #   map_signif_level = TRUE,
  #   textsize = 4,
  #   tip_length = 0.01, 
  #   annotations = "NS.", 
  #   fontface = 'italic', 
  #   y_position = c(8000) # Adjust y-position if necessary
  # ) +
  # geom_signif(
  #   data = subset(qbs_sum, Year == 2023), # Subset data for Crop2
  #   comparisons = list(c("Conventional", "Conservation")),
  #   map_signif_level = TRUE,
  #   textsize = 4,
  #   tip_length = 0.01,
  #   annotations = "NS.",
  #   fontface = 'italic', 
  #   y_position = c(8000) # Adjust y-position if necessary
  # ) +
  # geom_signif(
  #   data = subset(qbs_sum, Year == 2024), # Subset data for Crop2
  #   comparisons = list(c("Conventional", "Conservation")),
  #   map_signif_level = TRUE,
  #   textsize = 4,
  #   tip_length = 0.01,
  #   annotations = "NS.",
  #   fontface = 'italic', 
  #   y_position = c(8000) # Adjust y-position if necessary
  # ) +
  facet_wrap(~ Year, 
             ncol = 4, 
             scales = 'free_x')

fig1b


# # Save the plot to a file, using the column name in the file name
# file_name <- paste0("plots/figures/", "1B_qbs_c_score", "_fig.png")
# ggsave(filename = file_name, width = 8, height = 5, plot = fig1b)


qbs_e_sum <- read.csv(file = "Statistics/summary.stats/qbs_e_sum.csv")

### 01 C - qbs-e ####

fig1c <- ggplot(data = qbs_e_sum, 
                aes(x = Treatment, 
                    y = mean, 
                    fill = Treatment)) + 
  geom_bar(stat = "identity", 
           color = "black", 
           position = "dodge") + 
  labs(
    x = "Treatment",
    y = "Mean QBS-e Score",
    subtitle = "Mean QBS-e Score", 
    caption = "") +
  theme_bw() +
  scale_fill_manual(values = c("turquoise3", "tomato2"), 
                    name = "Treatment") +
  theme(strip.text.x = element_text(size = 12, 
                                    color = "black", 
                                    face = "bold.italic"), 
        axis.text.x=element_blank(),
        legend.position = "bottom") +
  geom_errorbar(aes(ymin = mean - se, 
                    ymax = mean + se),
                width = .2,                    # Width of the error bars
                position = position_dodge(.9)) +
  # geom_signif(
  #   data = subset(qbs_sum, Year == 2022), # Subset data for Crop1
  #   comparisons = list(c("Conventional", "Conservation")),
  #   map_signif_level = TRUE,
  #   textsize = 4,
  #   tip_length = 0.01, 
  #   annotations = "NS.", 
  #   fontface = 'italic', 
  #   y_position = c(1200) # Adjust y-position if necessary
  # ) +
  # geom_signif(
  #   data = subset(qbs_sum, Year == 2023), # Subset data for Crop2
  #   comparisons = list(c("Conventional", "Conservation")),
  #   map_signif_level = TRUE,
  #   textsize = 4,
  #   tip_length = 0.01,
  #   annotations = "NS.",
  #   fontface = 'italic', 
  #   y_position = c(1200) # Adjust y-position if necessary
  # ) +
  # geom_signif(
  #   data = subset(qbs_sum, Year == 2024), # Subset data for Crop2
  #   comparisons = list(c("Conventional", "Conservation")),
  #   map_signif_level = TRUE,
  #   textsize = 4,
  #   tip_length = 0.01,
  #   annotations = "NS.",
  #   fontface = 'italic', 
  #   y_position = c(1200) # Adjust y-position if necessary
  # ) +
  facet_wrap(~ Year, 
             ncol = 4, 
             scales = 'free_x')

fig1c


# # Save the plot to a file, using the column name in the file name
# file_name <- paste0("plots/figures/", "1C_qbs_e_score", "_fig.png")
# ggsave(filename = file_name, width = 8, height = 5, plot = fig1b)




### 01 D - shannon ####

fig1d <- ggplot(data = shannon_sum, 
                aes(x = Treatment, 
                    y = mean, 
                    fill = Treatment)) + 
  geom_bar(stat = "identity", 
           color = "black", 
           position = "dodge") + 
  labs(
    x = "Treatment",
    y = "Mean Shannon Diversity Index",
    subtitle = "Mean Shannon Diversity Index", 
    caption = "") +
  theme_bw() +
  scale_fill_manual(values = c("turquoise3", "tomato2"), 
                    name = "Treatment") +
  theme(strip.text.x = element_text(size = 12, 
                                    color = "black", 
                                    face = "bold.italic"), 
        axis.text.x=element_blank(),
        legend.position = "bottom") +
  geom_errorbar(aes(ymin = mean - se, 
                    ymax = mean + se),
                width = .2,                    # Width of the error bars
                position = position_dodge(.9)) +
  # geom_signif(
  #   data = subset(qbs_sum, Year == 2022), # Subset data for Crop1
  #   comparisons = list(c("Conventional", "Conservation")),
  #   map_signif_level = TRUE,
  #   textsize = 4,
  #   tip_length = 0.0005, 
  #   annotations = "NS.", 
  #   fontface = 'italic', 
  #   y_position = c(-3.7) # Adjust y-position if necessary
  # ) +
  # geom_signif(
  #   data = subset(qbs_sum, Year == 2023), # Subset data for Crop2
  #   comparisons = list(c("Conventional", "Conservation")),
  #   map_signif_level = TRUE,
  #   textsize = 4,
  #   tip_length = 0.0005,
  #   annotations = "p = 0.02",
  #   fontface = 'italic', 
  #   y_position = c(-3.7) # Adjust y-position if necessary
  # ) +
  # geom_signif(
  #   data = subset(qbs_sum, Year == 2024), # Subset data for Crop2
  #   comparisons = list(c("Conventional", "Conservation")),
  #   map_signif_level = TRUE,
  #   textsize = 4,
  #   tip_length = 0.0005,
  #   annotations = "NS.",
  #   fontface = 'italic', 
  #   y_position = c(-3.7) # Adjust y-position if necessary
  # ) +
  facet_wrap(~ Year, 
             ncol = 4, 
             scales = 'free_x')

fig1d


# correlation plot 

# Assuming `cor_long` is the melted correlation data
cor_plot <- ggplot(data = cor_long, 
                   aes(x = Index1, 
                       y = Index2, 
                       fill = Correlation)) +
  geom_tile(color = "white") +
  scale_fill_viridis(option = "viridis", 
                     name = "Correlation") +  # Use viridis scale with "plasma" palette
  geom_text(aes(label = round(Correlation, 2)), 
            color = "white", 
            # size = 5, 
            fontface = 4) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    x = "Index",
    y = "Index")


ggsave(filename = "Plots/figures/index_correlation_plot.png", width = 5, height = 5)



### 01 joint fig ####



figure1 <- ggarrange(fig1a, 
                    fig1b,
                    fig1c,
                    fig1d,
                    cor_plot,
                    ncol = 3, 
                    nrow = 2, 
                    align = "v",
                    vjust = 1, 
                    labels = c("A", "B", "C", "D", "E"), 
                    legend = "bottom", 
                    common.legend = TRUE,
                    widths = 1,
                    heights = 1) +
  theme(legend.text=element_text(size = 20))

figure1

# 
# annotate_figure(figure1,
#                 bottom = text_grob("n = 25", 
#                                    color = "black",
#                                    hjust = 1, 
#                                    x = 1, 
#                                    face = "italic", 
#                                    size = 10))

ggsave(filename = "fig_1_plot.png", 
       path = "plots/figures/", 
       width = 10, 
       height = 8)






## 02 FIG 2 ####

dat <- as.data.frame(tax_ord_list[1])

names(dat) <- names(qbs_dat_summary)


### 02 A - Chelicerata ####

fig2a <- ggplot(data = dat, 
                aes(x = Treatment, 
                    y = mean, 
                    fill = Treatment)) + 
  geom_bar(stat = "identity", 
           color = "black", 
           position = "dodge") + 
  labs(
    x = "Treatment",
    y = "Mean Chelicerata Abundance",
    subtitle = "Mean Chelicerata Abundance", 
    caption = "") +
  theme_bw() +
  scale_fill_manual(values = c("turquoise3", "tomato2"), 
                    name = "Treatment") +
  theme(strip.text.x = element_text(size = 12, 
                                    color = "black", 
                                    face = "bold.italic"), 
        axis.text.x=element_blank(),
        legend.position = "bottom") +
  geom_errorbar(aes(ymin = mean - se, 
                    ymax = mean + se),
                width = .2,                    # Width of the error bars
                position = position_dodge(.9)) +
  # geom_signif(
  #   data = subset(qbs_sum, Year == 2022), # Subset data for Crop1
  #   comparisons = list(c("Conventional", "Conservation")),
  #   map_signif_level = TRUE,
  #   textsize = 4,
  #   tip_length = 0.01, 
  #   annotations = "NS.", 
  #   fontface = 'italic', 
  #   y_position = c(250) # Adjust y-position if necessary
  # ) +
  # geom_signif(
  #   data = subset(qbs_sum, Year == 2023), # Subset data for Crop2
  #   comparisons = list(c("Conventional", "Conservation")),
  #   map_signif_level = TRUE,
  #   textsize = 4,
  #   tip_length = 0.01,
  #   annotations = "NS.",
  #   fontface = 'italic', 
  #   y_position = c(250) # Adjust y-position if necessary
  # ) +
  # geom_signif(
  #   data = subset(qbs_sum, Year == 2024), # Subset data for Crop2
  #   comparisons = list(c("Conventional", "Conservation")),
  #   map_signif_level = TRUE,
  #   textsize = 4,
  #   tip_length = 0.01,
  #   annotations = "NS.",
  #   fontface = 'italic', 
  #   y_position = c(250) # Adjust y-position if necessary
  # ) +
  facet_wrap(~ Year, 
             ncol = 4, 
             scales = 'free_x')

fig2a


# # Save the plot to a file, using the column name in the file name
# file_name <- paste0("plots/figures/", "2A_Chelicerata", "_fig.png")
# ggsave(filename = file_name, width = 8, height = 5, plot = fig2a)


### 02 B - Crustacea ####

dat <- as.data.frame(tax_ord_list[2])

names(dat) <- names(qbs_dat_summary)

fig2b <- ggplot(data = dat, 
                aes(x = Treatment, 
                    y = mean, 
                    fill = Treatment)) + 
  geom_bar(stat = "identity", 
           color = "black", 
           position = "dodge") + 
  labs(
    x = "Treatment",
    y = "Mean Crustacea Abundance",
    subtitle = "Mean Crustacea Abundance", 
    caption = "") +
  theme_bw() +
  scale_fill_manual(values = c("turquoise3", "tomato2"), 
                    name = "Treatment") +
  theme(strip.text.x = element_text(size = 12, 
                                    color = "black", 
                                    face = "bold.italic"), 
        axis.text.x=element_blank(),
        legend.position = "bottom") +
  geom_errorbar(aes(ymin = mean - se, 
                    ymax = mean + se),
                width = .2,                    # Width of the error bars
                position = position_dodge(.9)) +
  # geom_signif(
  #   data = subset(qbs_sum, Year == 2022), # Subset data for Crop1
  #   comparisons = list(c("Conventional", "Conservation")),
  #   map_signif_level = TRUE,
  #   textsize = 4,
  #   tip_length = 0.001, 
  #   annotations = "NS.", 
  #   fontface = 'italic', 
  #   y_position = -3.5 # Adjust y-position if necessary
  # ) +
  # geom_signif(
  #   data = subset(qbs_sum, Year == 2023), # Subset data for Crop2
  #   comparisons = list(c("Conventional", "Conservation")),
  #   map_signif_level = TRUE,
  #   textsize = 4,
  #   tip_length = 0.001,
  #   annotations = "NS.",
  #   fontface = 'italic', 
  #   y_position = -3.5 # Adjust y-position if necessary
  # ) +
  # geom_signif(
  #   data = subset(qbs_sum, Year == 2024), # Subset data for Crop2
  #   comparisons = list(c("Conventional", "Conservation")),
  #   map_signif_level = TRUE,
  #   textsize = 4,
  #   tip_length = 0.001,
  #   annotations = "NS.",
  #   fontface = 'italic', 
  #   y_position = -3.5 # Adjust y-position if necessary
  # ) +
  facet_wrap(~ Year, 
             ncol = 4, 
             scales = 'free_x')

fig2b



### 02 C - Myriapoda ####

dat <- as.data.frame(tax_ord_list[3])

names(dat) <- names(qbs_dat_summary)

fig2c <- ggplot(data = dat, 
                aes(x = Treatment, 
                    y = mean, 
                    fill = Treatment)) + 
  geom_bar(stat = "identity", 
           color = "black", 
           position = "dodge") + 
  labs(
    x = "Treatment",
    y = "Mean Myriapoda Abundance",
    subtitle = "Mean Myriapoda Abundance", 
    caption = "") +
  theme_bw() +
  scale_fill_manual(values = c("turquoise3", "tomato2"), 
                    name = "Treatment") +
  theme(strip.text.x = element_text(size = 12, 
                                    color = "black", 
                                    face = "bold.italic"), 
        axis.text.x=element_blank(),
        legend.position = "bottom") +
  geom_errorbar(aes(ymin = mean - se, 
                    ymax = mean + se),
                width = .2,                    # Width of the error bars
                position = position_dodge(.9)) +
  # geom_signif(
  #   data = subset(qbs_sum, Year == 2022), # Subset data for Crop1
  #   comparisons = list(c("Conventional", "Conservation")),
  #   map_signif_level = TRUE,
  #   textsize = 4,
  #   tip_length = 0.001, 
  #   annotations = "NS.", 
  #   fontface = 'italic', 
  #   y_position = c(-2) # Adjust y-position if necessary
  # ) +
  # geom_signif(
  #   data = subset(qbs_sum, Year == 2023), # Subset data for Crop2
  #   comparisons = list(c("Conventional", "Conservation")),
  #   map_signif_level = TRUE,
  #   textsize = 4,
  #   tip_length = 0.001,
  #   annotations = "NS.",
  #   fontface = 'italic', 
  #   y_position = c(-2) # Adjust y-position if necessary
  # ) +
  # geom_signif(
  #   data = subset(qbs_sum, Year == 2024), # Subset data for Crop2
  #   comparisons = list(c("Conventional", "Conservation")),
  #   map_signif_level = TRUE,
  #   textsize = 4,
  #   tip_length = 0.001, 
  #   annotations = "NS.",
  #   fontface = 'italic', 
  #   y_position = c(-2) # Adjust y-position if necessary
  # ) +
  facet_wrap(~ Year, 
             ncol = 4, 
             scales = 'free_x')

fig2c




### 02 D - Hexapoda ####

dat <- as.data.frame(tax_ord_list[4])

names(dat) <- names(qbs_dat_summary)

fig2d <- ggplot(data = dat, 
                aes(x = Treatment, 
                    y = mean, 
                    fill = Treatment)) + 
  geom_bar(stat = "identity", 
           color = "black", 
           position = "dodge") + 
  labs(
    x = "Treatment",
    y = "Mean Hexapoda Abundance",
    subtitle = "Mean Hexapoda Abundance", 
    caption = "") +
  theme_bw() +
  scale_fill_manual(values = c("turquoise3", "tomato2"), 
                    name = "Treatment") +
  theme(strip.text.x = element_text(size = 12, 
                                    color = "black", 
                                    face = "bold.italic"), 
        axis.text.x=element_blank(),
        legend.position = "bottom") +
  geom_errorbar(aes(ymin = mean - se, 
                    ymax = mean + se),
                width = .2,                    # Width of the error bars
                position = position_dodge(.9)) +
  # geom_signif(
  #   data = subset(qbs_sum, Year == 2022), # Subset data for Crop1
  #   comparisons = list(c("Conventional", "Conservation")),
  #   map_signif_level = TRUE,
  #   textsize = 4,
  #   tip_length = 0.01, 
  #   annotations = "NS.", 
  #   fontface = 'italic', 
  #   y_position = c(500) # Adjust y-position if necessary
  # ) +
  # geom_signif(
  #   data = subset(qbs_sum, Year == 2023), # Subset data for Crop2
  #   comparisons = list(c("Conventional", "Conservation")),
  #   map_signif_level = TRUE,
  #   textsize = 4,
  #   tip_length = 0.01,
  #   annotations = "NS.",
  #   fontface = 'italic', 
  #   y_position = c(500) # Adjust y-position if necessary
  # ) +
  # geom_signif(
  #   data = subset(qbs_sum, Year == 2024), # Subset data for Crop2
  #   comparisons = list(c("Conventional", "Conservation")),
  #   map_signif_level = TRUE,
  #   textsize = 4,
  #   tip_length = 0.01, 
  #   annotations = "NS.",
  #   fontface = 'italic', 
  #   y_position = c(500) # Adjust y-position if necessary
  # ) +
  facet_wrap(~ Year, 
             ncol = 4, 
             scales = 'free_x')

fig2d



### 02 E total abundance ####


fig2e <- ggplot(data = total_abun_sum, 
       aes(x = Treatment, 
           y = mean, 
           fill = Treatment)) + 
  geom_bar(stat = "identity", 
           color = "black", 
           position = "dodge") + 
  labs(
    x = "Treatment",
    y = "Total abundance",
    subtitle = "Mean Total Abundance", 
    caption = "") +
  theme_bw() +
  scale_fill_manual(values = c("turquoise3", "tomato2"), 
                    name = "Treatment") +
  theme(strip.text.x = element_text(size = 12, 
                                    color = "black", 
                                    face = "bold.italic"),
        axis.text.x=element_blank(),
        legend.position = "bottom") +
  geom_errorbar(aes(ymin = mean - se, 
                    ymax = mean + se),
                width = .2,                    # Width of the error bars
                position = position_dodge(.9)) +
  # geom_signif(
  #   data = subset(qbs_sum, Year == 2022), # Subset data for Crop1
  #   comparisons = list(c("Conventional", "Conservation")),
  #   map_signif_level = TRUE,
  #   textsize = 4,
  #   tip_length = 0.01, 
  #   annotations = "NS.", 
  #   fontface = 'italic', 
  #   y_position = c(700) # Adjust y-position if necessary
  # ) +
  # geom_signif(
  #   data = subset(qbs_sum, Year == 2023), # Subset data for Crop2
  #   comparisons = list(c("Conventional", "Conservation")),
  #   map_signif_level = TRUE,
  #   textsize = 4,
  #   tip_length = 0.01,
  #   annotations = "NS.",
  #   fontface = 'italic', 
  #   y_position = c(700) # Adjust y-position if necessary
  # ) +
  # geom_signif(
  #   data = subset(qbs_sum, Year == 2024), # Subset data for Crop2
  #   comparisons = list(c("Conventional", "Conservation")),
  #   map_signif_level = TRUE,
  #   textsize = 4,
  #   tip_length = 0.01, 
  #   annotations = "NS.",
  #   fontface = 'italic', 
  #   y_position = c(700) # Adjust y-position if necessary
  # ) +
  facet_wrap(~ Year, 
             ncol = 4, 
             scales = 'free_x')


dir.create(path = "Plots/barplots/total_abundance/")

ggsave(filename = "total_abundance_barplot.png", 
       path = "Plots/barplots/total_abundance/", width = 8, height = 5)






### 02 joint fig ####



figure2 <- ggarrange(fig2a, 
                     fig2b,
                     fig2c,
                     fig2d,
                     fig2e,
                     ncol = 3, 
                     nrow = 2, 
                     # align = "v",
                     # vjust = 1, 
                     labels = c("A", "B", "C", "D", "E"), 
                     legend = "bottom", 
                     common.legend = TRUE,
                     widths = 1,
                     heights = 1) +
  theme(legend.text=element_text(size = 20))

figure2


# annotate_figure(figure2,
#                 bottom = text_grob("n = 25", 
#                                    color = "black",
#                                    hjust = 1, 
#                                    x = 1, 
#                                    face = "italic", 
#                                    size = 10))

ggsave(filename = "fig_2_plot.png", 
       path = "plots/figures/", 
       width = 10, 
       height = 8)







## 03 QBS-c ####

### 03 A Epigeic ####

dat <- as.data.frame(qbs_c_dat_list[1])

names(dat) <- names(qbs_c_dat_summary)


fig3a <- ggplot(data = dat, 
                aes(x = Treatment, 
                    y = mean, 
                    fill = Treatment)) + 
  geom_bar(stat = "identity", 
           color = "black", 
           position = "dodge") + 
  labs(
    x = "Treatment",
    y = "QBS-c Score",
    subtitle = "Mean Epigeic Collembola QBS-c Score", 
    caption = "") +
  theme_bw() +
  scale_fill_manual(values = c("turquoise3", "tomato2"), 
                    name = "Treatment") +
  theme(strip.text.x = element_text(size = 12, 
                                    color = "black", 
                                    face = "bold.italic"), 
        axis.text.x=element_blank(),
        legend.position = "bottom") +
  geom_errorbar(aes(ymin = mean - se, 
                    ymax = mean + se),
                width = .2,                    # Width of the error bars
                position = position_dodge(.9)) +
  # geom_signif(
  #   data = subset(qbs_sum, Year == 2022), # Subset data for Crop1
  #   comparisons = list(c("Conventional", "Conservation")),
  #   map_signif_level = TRUE,
  #   textsize = 4,
  #   tip_length = 0.01, 
  #   annotations = "NS.", 
  #   fontface = 'italic', 
  #   y_position = c(200) # Adjust y-position if necessary
  # ) +
  # geom_signif(
  #   data = subset(qbs_sum, Year == 2023), # Subset data for Crop2
  #   comparisons = list(c("Conventional", "Conservation")),
  #   map_signif_level = TRUE,
  #   textsize = 4,
  #   tip_length = 0.01,
  #   annotations = "NS.",
  #   fontface = 'italic', 
  #   y_position = c(200) # Adjust y-position if necessary
  # ) +
  # geom_signif(
  #   data = subset(qbs_sum, Year == 2024), # Subset data for Crop2
  #   comparisons = list(c("Conventional", "Conservation")),
  #   map_signif_level = TRUE,
  #   textsize = 4,
  #   tip_length = 0.01,
  #   annotations = "p = 0.01",
  #   fontface = 'italic', 
  #   y_position = c(200) # Adjust y-position if necessary
  # ) +
  facet_wrap(~ Year, 
             ncol = 4, 
             scales = 'free_x')

fig3a



### 03 B Hemiedaphic ####

dat <- as.data.frame(qbs_c_dat_list[2])

names(dat) <- names(qbs_c_dat_summary)


fig3b <- ggplot(data = dat, 
                aes(x = Treatment, 
                    y = mean, 
                    fill = Treatment)) + 
  geom_bar(stat = "identity", 
           color = "black", 
           position = "dodge") + 
  labs(
    x = "Treatment",
    y = "QBS-c Score",
    subtitle = "Mean Hemiedaphic Collembola QBS-c Score", 
    caption = "") +
  theme_bw() +
  scale_fill_manual(values = c("turquoise3", "tomato2"), 
                    name = "Treatment") +
  theme(strip.text.x = element_text(size = 12, 
                                    color = "black", 
                                    face = "bold.italic"), 
        axis.text.x=element_blank(),
        legend.position = "bottom") +
  geom_errorbar(aes(ymin = mean - se, 
                    ymax = mean + se),
                width = .2,                    # Width of the error bars
                position = position_dodge(.9)) +
  # geom_signif(
  #   data = subset(qbs_sum, Year == 2022), # Subset data for Crop1
  #   comparisons = list(c("Conventional", "Conservation")),
  #   map_signif_level = TRUE,
  #   textsize = 4,
  #   tip_length = 0.01, 
  #   annotations = "NS.", 
  #   fontface = 'italic', 
  #   y_position = c(1400) # Adjust y-position if necessary
  # ) +
  # geom_signif(
  #   data = subset(qbs_sum, Year == 2023), # Subset data for Crop2
  #   comparisons = list(c("Conventional", "Conservation")),
  #   map_signif_level = TRUE,
  #   textsize = 4,
  #   tip_length = 0.01,
  #   annotations = "NS.",
  #   fontface = 'italic', 
  #   y_position = c(1400) # Adjust y-position if necessary
  # ) +
  # geom_signif(
  #   data = subset(qbs_sum, Year == 2024), # Subset data for Crop2
  #   comparisons = list(c("Conventional", "Conservation")),
  #   map_signif_level = TRUE,
  #   textsize = 4,
  #   tip_length = 0.01,
  #   annotations = "NS.",
  #   fontface = 'italic', 
  #   y_position = c(1400) # Adjust y-position if necessary
  # ) +
  facet_wrap(~ Year, 
             ncol = 4, 
             scales = 'free_x')

fig3b



### 03 C Eudaphic ####

dat <- as.data.frame(qbs_c_dat_list[3])

names(dat) <- names(qbs_c_dat_summary)


fig3c <- ggplot(data = dat, 
                aes(x = Treatment, 
                    y = mean, 
                    fill = Treatment)) + 
  geom_bar(stat = "identity", 
           color = "black", 
           position = "dodge") + 
  labs(
    x = "Treatment",
    y = "QBS-c Score",
    subtitle = "Mean Eudaphic Collembola QBS-c Score", 
    caption = "") +
  theme_bw() +
  scale_fill_manual(values = c("turquoise3", "tomato2"), 
                    name = "Treatment") +
  theme(strip.text.x = element_text(size = 12, 
                                    color = "black", 
                                    face = "bold.italic"), 
        axis.text.x=element_blank(),
        legend.position = "bottom") +
  geom_errorbar(aes(ymin = mean - se, 
                    ymax = mean + se),
                width = .2,                    # Width of the error bars
                position = position_dodge(.9)) +
  # geom_signif(
  #   data = subset(qbs_sum, Year == 2022), # Subset data for Crop1
  #   comparisons = list(c("Conventional", "Conservation")),
  #   map_signif_level = TRUE,
  #   textsize = 4,
  #   tip_length = 0.01, 
  #   annotations = "NS.", 
  #   fontface = 'italic', 
  #   y_position = c(8000) # Adjust y-position if necessary
  # ) +
  # geom_signif(
  #   data = subset(qbs_sum, Year == 2023), # Subset data for Crop2
  #   comparisons = list(c("Conventional", "Conservation")),
  #   map_signif_level = TRUE,
  #   textsize = 4,
  #   tip_length = 0.01,
  #   annotations = "NS.",
  #   fontface = 'italic', 
  #   y_position = c(8000) # Adjust y-position if necessary
  # ) +
  # geom_signif(
  #   data = subset(qbs_sum, Year == 2024), # Subset data for Crop2
  #   comparisons = list(c("Conventional", "Conservation")),
  #   map_signif_level = TRUE,
  #   textsize = 4,
  #   tip_length = 0.01,
  #   annotations = "NS.",
  #   fontface = 'italic', 
  #   y_position = c(8000) # Adjust y-position if necessary
  # ) +
  facet_wrap(~ Year, 
             ncol = 4, 
             scales = 'free_x')

fig3c



### 03 D Hemiedaphic ####

dat <- as.data.frame(qbs_c_dat_list[4])

names(dat) <- names(qbs_c_dat_summary)


fig3d <- ggplot(data = dat, 
                aes(x = Treatment, 
                    y = mean, 
                    fill = Treatment)) + 
  geom_bar(stat = "identity", 
           color = "black", 
           position = "dodge") + 
  labs(
    x = "Treatment",
    y = "QBS-c Score",
    subtitle = "Mean QBS-c Score", 
    caption = "") +
  theme_bw() +
  scale_fill_manual(values = c("turquoise3", "tomato2"), 
                    name = "Treatment") +
  theme(strip.text.x = element_text(size = 12, 
                                    color = "black", 
                                    face = "bold.italic"), 
        axis.text.x=element_blank(),
        legend.position = "bottom") +
  geom_errorbar(aes(ymin = mean - se, 
                    ymax = mean + se),
                width = .2,                    # Width of the error bars
                position = position_dodge(.9)) +
  # geom_signif(
  #   data = subset(qbs_sum, Year == 2022), # Subset data for Crop1
  #   comparisons = list(c("Conventional", "Conservation")),
  #   map_signif_level = TRUE,
  #   textsize = 4,
  #   tip_length = 0.01, 
  #   annotations = "NS.", 
  #   fontface = 'italic', 
  #   y_position = c(8000) # Adjust y-position if necessary
  # ) +
  # geom_signif(
  #   data = subset(qbs_sum, Year == 2023), # Subset data for Crop2
  #   comparisons = list(c("Conventional", "Conservation")),
  #   map_signif_level = TRUE,
  #   textsize = 4,
  #   tip_length = 0.01,
  #   annotations = "NS.",
  #   fontface = 'italic', 
  #   y_position = c(8000) # Adjust y-position if necessary
  # ) +
  # geom_signif(
  #   data = subset(qbs_sum, Year == 2024), # Subset data for Crop2
  #   comparisons = list(c("Conventional", "Conservation")),
  #   map_signif_level = TRUE,
  #   textsize = 4,
  #   tip_length = 0.01,
  #   annotations = "NS.",
  #   fontface = 'italic', 
  #   y_position = c(8000) # Adjust y-position if necessary
  # ) +
  facet_wrap(~ Year, 
             ncol = 4, 
             scales = 'free_x')

fig3d


### 03 joint fig ####



figure3 <- ggarrange(fig3a, 
                     fig3b,
                     fig3c,
                     fig3d,
                     ncol = 2, 
                     nrow = 2, 
                     align = "v",
                     vjust = 1, 
                     labels = c("A", "B", "C", "D"), 
                     legend = "bottom", 
                     common.legend = TRUE,
                     widths = 1,
                     heights = 1) +
  theme(legend.text=element_text(size = 20))

figure3


# annotate_figure(figure3,
#                 bottom = text_grob("n = 25", 
#                                    color = "black",
#                                    hjust = 1, 
#                                    x = 1, 
#                                    face = "italic", 
#                                    size = 10))

ggsave(filename = "fig_3_plot.png", 
       path = "plots/figures/", 
       width = 10, 
       height = 6)






## ANY OTHER PLOTS ####


