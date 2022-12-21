library(plyr)
library(tidyverse)
library(ggplot2)
library(showtext)
library(ggnewscale)
library(ggrepel)

# create df
donut_df <- data.frame(
  category = sample(c("Fruit", "Vegetable", "Bread"), size = 80, replace = TRUE), 
  amount = sample(50:1000, size = 80, replace = FALSE), 
  month = sample(1:10, size = 80, replace = TRUE),
  item = "item"
)

donut_df <- donut_df %>% mutate(item = case_when(
  category == "Fruit" ~ sample(c("Star fruit", "Dragon fruit", "Passion fruit"), size = 80, replace = TRUE),
  category == "Vegetable" ~ sample(c("Sweet potato", "Lotus root", "Goya"), size = 80, replace = TRUE),
  category == "Bread" ~ sample(c("Semmel", "Krapfen", "Weckerl"), size = 80, replace = TRUE)
))


# prepare df for outside donut/donut1
donut1 <- donut_df %>%
  group_by(item) %>%
  dplyr::summarise(total = sum(amount)) %>%
  data.frame()

donut1 <- donut1 %>%
  mutate(fraction = total/sum(total)) %>%
  # so items show up according to alphabetical order of 'category' superset
  mutate(item2 = recode(item,  "Star fruit" = "y_Star fruit", "Dragon fruit" = "y_Dragon fruit", "Passion fruit" = "y_Passion fruit", "Sweet potato" = "z_Sweet potato", "Lotus root" = "z_Lotus root", "Goya" = "z_Goya" ))
donut1$item2 <- as.character(donut1$item2)
donut1 = donut1[order(donut1$item2), ]
donut1 <- donut1 %>%
  mutate(ymax = cumsum(donut1$fraction))
donut1 <- donut1 %>%
  mutate(ymin = c(0, head(donut1$ymax, n = -1))) #ymax of previous row

# calculate a first ymin/ymax in order to get label_pos; overwrite later to make gradient for plot
donut1 <- donut1 %>%
  mutate(label = paste0(donut1$item), 
         label_pos = (ymax + ymin)/2)  

# cut up ymin and ymax into "N" portions to create gradient 
N <- 100
donut1plot <- data.frame(
  total = rep(donut1$total, each = N),
  item2 = rep(donut1$item2, each = N),
  label = rep(donut1$label, each = N),
  label_pos = rep(donut1$label_pos, each = N),
  ymin = c(0,
           seq(0.00, 0.1121987, length = N), #get these values from donut1
           seq(0.1121987, 0.2348386, length = N),
           seq(0.2348386, 0.4020273, length = N),
           seq(0.4020273, 0.4665884, length = N),
           seq(0.4665884, 0.5726964, length = N),
           seq(0.5726964, 0.6860045, length = N),
           seq(0.6860045, 0.8196728, length = N),
           seq(0.8196728, 0.9159053, length = N),
           seq(0.9159053, 1.00, length = N)[-N]),
  ymax = c(seq(0.00, 0.1121987, length = N)[-1],
           seq(0.1121987, 0.2348386, length = N),
           seq(0.2348386, 0.4020273, length = N),
           seq(0.4020273, 0.4665884, length = N),
           seq(0.4665884, 0.5726964, length = N),
           seq(0.5726964, 0.6860045, length = N),
           seq(0.6860045, 0.8196728, length = N),
           seq(0.8196728, 0.9159053, length = N),
           seq(0.9159053, 1.00, length = N),
           1.00)) 
# split by item2 and create column 'alpha' for each subset
donut1plot <- plyr::ddply(donut1plot, .(item2), transform, 
                          alpha = c(0, seq(0.00, 1.00, length = N)[-N]))


# prepare df for inside donut/donut2
donut2 <- donut_df %>%
  group_by(category) %>%
  dplyr::summarise(total = sum(amount)) %>%
  data.frame()

donut2$category <- as.character(donut2$category)
donut2 <- with(donut2, donut2[order(category), ]) 

donut2 <- donut2 %>%
  mutate(fraction = total/sum(total))
donut2 <- donut2 %>%
  mutate(ymax = cumsum(donut2$fraction)) 
donut2 <- donut2 %>%
  mutate(ymin = c(0, head(donut2$ymax, n = -1)), #ymax of previous row
         label = paste0(donut2$category), 
         label_pos = (ymax + ymin)/2) 


# to use colorbrewer
# library(RColorBrewer)
# display.brewer.all(n = NULL, type = "qual", colorblindFriendly = TRUE)
# brewer.pal(n = 6, name = "Dark2")

# to get viridis #codes 
# library(scales)
# show_col(viridis_pal()(6))

# use same colours for items in same category
fill_scheme <- c("#CFE11CFF","#CFE11CFF","#CFE11CFF","#21908CFF", "#21908CFF", "#21908CFF", "#481B6DFF", "#481B6DFF", "#481B6DFF") #for donut1plot/ outer
fill_scheme2 <- c("#CFE11CFF", "#21908CFF", "#481B6DFF") #for donut2/inner

showtext_auto()
# theme_void()$plot.margin


# plot main donut
p1 = ggplot() +
  geom_rect(data = donut1plot, aes(fill = item2, 
                                   ymax = ymax, ymin = ymin,
                                   xmax = 4, xmin = 3, alpha = alpha)) +
  scale_fill_manual(values = fill_scheme) +
  guides(fill = "none") +
  new_scale_fill() + #start new colour scale
  geom_rect(data = donut2, aes(fill = category, ymax = ymax, ymin = ymin,
                               xmax = 3, xmin = 2)) +
  scale_fill_manual(values = fill_scheme2, labels = donut2$category) +
  theme(legend.position = NULL) +
  coord_polar(theta = "y") +
  xlim(c(0, 4)) +
  guides(alpha = "none") +
  geom_label_repel(data = donut1, x = 4, 
                   aes(y = label_pos, label = label),
                   size = 3, 
                   nudge_y = 1, # hor distance between labels reqd
                   direction = "x", point.size = NA, show.legend = FALSE) +
  theme_void() +
  theme(plot.margin = margin(t = 0, r = 0, b = 0, l = 2.5, unit = "cm"), 
        legend.title = element_blank())
p1

# clean df for facet plots
facet_data <- donut_df %>%
  group_by(month, category) %>%
  dplyr::summarise(total = sum(amount)) %>%
  data.frame()

facet_data$category <- as.character(facet_data$category)
facet_data <- with(facet_data, facet_data[order(month, category), ]) 
# get donut coordinates for each month
facet_data <- plyr::ddply(facet_data, .(month), transform, fraction = total/sum(total))
facet_data <- plyr::ddply(facet_data, .(month), transform, 
                          ymax = cumsum(fraction))
facet_data <- plyr::ddply(facet_data, .(month), transform, 
                          ymin = c(0, head(ymax, n = -1)))


# data for annotating: need to use geom_text
facet_text <- data.frame(
  month = 1:10)
facet_text <- facet_data %>%
  group_by(month) %>%
  dplyr::summarise(total = sum(total)) %>%
  round() %>%
  inner_join(facet_text, by = "month")
facet_text$total <- prettyNum(as.numeric(facet_text$total), 
                              big.mark = ".", decimal.mark = ",")

# facet labels
facet_labels <- c("1" = "Jan", "2" = "Feb", "3" = "Mar", 
                  "4" = "Apr", "5" = "May", "6" = "Jun", 
                  "7" = "Jul", "8" = "Aug", "9" = "Sep", "10" = "Oct")


p2 = ggplot() +
  geom_rect(data = facet_data, aes(fill = category, ymax = ymax, ymin = ymin,
                                   xmax = 4, xmin = 3)) +
  facet_wrap(~month, ncol = 5, labeller = as_labeller(facet_labels)) +
  scale_fill_manual(values = fill_scheme2) +
  coord_polar(theta = "y") +
  xlim(c(0, 4)) +
  guides(alpha = "none", fill = "none") +
  geom_text(data = facet_text, mapping = aes(x = 0, y = 0, label = total), 
            size = 3) + # annotate: x and y are 0 because polar
  theme_classic() + 
  theme(legend.title = element_blank(),
        panel.grid = element_blank(),
        strip.background = element_blank()) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(), 
        axis.line = element_blank(), 
        axis.title = element_blank())
p2

# combine plots
# vertical
plot_ver <- plot_grid(p1, p2, ncol = 1, rel_heights = c(1, 0.7))
plot_ver

# horizontal with shared legend at bottom
# first plot p1 without legend
combined_plot <- plot_grid(p1, p2, ncol = 2)
legend <- get_legend(
  p1 +
    theme(legend.direction = "horizontal")
)
plot_hor <- plot_grid(combined_plot, legend, ncol = 1, rel_heights = c(1, 0.1))
plot_hor

# main references
# https://stackoverflow.com/questions/13615562/ggplot-donut-chart
# https://stackoverflow.com/questions/43593378/add-color-gradient-to-geom-rect-in-r 
# https://ggrepel.slowkow.com/articles/examples.html
# https://stackoverflow.com/questions/11889625/annotating-text-on-individual-facet-in-ggplot2


# notes
# since ggplot doesn't allow two scale_fill_manual (e.g. here to add an outline colour for each donut segment), add a second colour scale using the second aes (col = type), especially for line geom 
# check out citation("ggnewscale")




