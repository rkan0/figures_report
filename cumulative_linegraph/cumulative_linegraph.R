library(tidyverse)
library(ggplot2)

# create df
question_data <- data.frame(
  date = seq(as.Date("2010-01-01"), as.Date("2010-06-01"), by = "days")
) %>%
  slice(rep(1:n(), each = 2)) 
# 1 = received, 2 = discarded
question_data$status <- sample(c(1, 2), replace = TRUE, 
                               size = nrow(question_data), prob = c(0.9, 0.1)) 
question_data$count <- sample(0:5, replace = TRUE, 
                              size = nrow(question_data), prob = c(0.5, (rep(0.1, 5))))

question_data<- question_data  %>%  
  mutate(count = na_if(count, "0")) %>%
  filter(!is.na(count))

question_data<- question_data %>%
  group_by(date, status) %>%
  summarise(count = sum(count)) %>% #so that there is only one rec/disc per date
  pivot_wider(names_from = status, values_from = count) %>%
  rename(received = 2, discarded = 3) 

question_data$cs_rec <- cumsum(coalesce(question_data$received, 0)) + question_data$received*0
question_data$cs_dis <- cumsum(coalesce(question_data$discarded, 0)) + question_data$discarded*0
max(question_data$cs_rec)
max(question_data$cs_dis)
question_data <- pivot_longer(question_data, cols = cs_rec:cs_dis, names_to = "cs_recdis", values_to = "count") 

# create labels for figure
label_text <- data.frame(
  x = c("2010-05-15", "2010-04-10"), 
  y = c(80, 380), 
  label = c("45 responses were discarded", "403 valid responses were \n received by the cut-off date")
)
label_text$x <- lubridate::ymd(label_text$x) 

limit <- c(as.Date("2010-01-01"), as.Date ("2010-06-10")) #x-axis cannot start earlier than first date in data

# plot
p <- ggplot(data = question_data, aes(x = date, y = count, group = cs_recdis)) +
  geom_line(data = question_data[!is.na(question_data$count),]) + 
  geom_point(aes(shape = cs_recdis), size = 1) +
  theme_classic() +
  scale_x_date(date_labels="%Y-%m", limits = limit, 
               expand = c(0, 0), name = "Date") + 
  scale_y_continuous(name = "Count (cumulative)") +
  #scale_shape_discrete(name = "", breaks = c("cs_rec", "cs_dis"), 
  #                        labels = c("Received", "Discarded")) +
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        legend.position = "none") +
  geom_text(data = label_text, aes(x = x, y = y, label = label),
            inherit.aes = FALSE)
p
