library(oefenwebDatabase)
library(ggplot2)
library(tidyverse)
library(RColorBrewer)
library(lintr)

# to put the plots together
suppressMessages(library("cowplot"))
# to prepare data
suppressMessages(library("dplyr"))
# make database connection
con <- connect()

#-------------- Log Data --------------
records <- suppressWarnings(DBI::dbGetQuery(con, "SELECT `user_agent_id`, `grade`, `created`,
                                            `user_id`, `response_in_milliseconds`, `domain_id`
                                            FROM `log_records`
                                            WHERE `created` >= '2018-11-09' AND `created` < '2018-12-09'"))
records <- rename(records, id = user_agent_id, time = created,
                  response = response_in_milliseconds)
save(records, file = "records.Rdata")
load("records.Rdata")
records <- subset(records, grade >= 1 & grade <= 8)

#-------------- Domain Data --------------
domain <- suppressWarnings(DBI::dbGetQuery(con, "SELECT `id` AS `domain_id`, `short_name`, `name`
                                           FROM `domains`"))
domain <- rename(domain, domain_name = name)
save(domain, file = "domain.Rdata")
load("domain.Rdata")

#-------------- Device Type --------------
devices <- suppressWarnings(DBI::dbGetQuery(con, "SELECT * FROM `user_agents`"))
devices <- rename(devices, device = string)
devices$device <- substr(devices$device, 14, 16)
devices$device <- as.character(gsub("Win", "Windows", devices$device))
devices$device <- as.character(gsub("iPa", "iPad", devices$device))
devices$device <- as.character(gsub("X11", "Linux", devices$device))
save(devices, file = "devices.Rdata")
load("devices.Rdata")

#-------------- Response Time & Non-Resopnse Time --------------
# calculate average response time & non_response time
response <- records %>%
  group_by(id, domain_id) %>%
  summarize(response_time = mean(response), non_response = min(response))

# join tables
response <- inner_join(response, domain)
data <- inner_join(response, devices)

# take basic math data
math_data <- subset(data, (domain_id >= 1) & (domain_id <= 4))

# rename the labels
math_data$domain_name <- gsub("optellen", "Addition", math_data$domain_name)
math_data$domain_name <- gsub("aftrekken", "Subtraction", math_data$domain_name)
math_data$domain_name <- gsub("vermenigvuldigen", "Multiplication", math_data$domain_name)
math_data$domain_name <- gsub("delen", "Division", math_data$domain_name)

# re-order the domains
math_data$domain_name <- factor(math_data$domain_name,
                                levels = c("Addition", "Subtraction", "Multiplication", "Division"))

# average response time by device & domain
plot1 <- ggplot(math_data, aes(x = factor(device), y = response_time,
                 fill = factor(device))) +
  stat_summary(fun.y = "mean", geom = "bar") +
  facet_wrap(vars(domain_name), nrow = 1) +
  theme_cowplot() +
  labs(title = "Average Response Time on Different Device Types",
       y = "Response Time \n (milliseconds)") +
  scale_fill_brewer(palette = "Set2") +
  coord_cartesian(ylim = c(5000, 9500)) +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        legend.position = c(.8, .8),
        plot.title = element_text(hjust = 0.5, face = "plain"),
        strip.text = element_text(size = 12),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 9))

# non_response time by device & domain
plot2 <- ggplot(math_data, aes(x = factor(device), y = non_response,
                 fill = factor(device))) +
  stat_summary(fun.y = "min", geom = "bar") +
  facet_wrap(vars(domain_name), nrow = 1) +
  theme_cowplot() +
  labs(title = "Non-Response Time on Different Device Types",
       y = "Non-Response Time \n (milliseconds)") +
  scale_fill_brewer(palette = "Set2") +
  coord_cartesian(ylim = c(0, 1000)) +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        legend.position = c(.1, .8),
        plot.title = element_text(hjust = 0.5, face = "plain"),
        strip.text = element_text(size = 12),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 9))

#-------------- Usage by Hour --------------
# daily time series
daily <- records %>%
  select(id, time) %>%
  mutate(date = substr(time, 1, 10), hour = substr(time, 12, 13)) %>%
  select(id, date, hour)

# combine with device type
daily <- inner_join(daily, devices)

by_hour <- daily %>%
  group_by(hour, date, device) %>%
  summarise(number = n())

plot3 <- ggplot(by_hour, aes(x = hour, y = number,
                             group = factor(device),
                             color = factor(device))) +
  stat_summary(fun.y = "mean", geom = "line", size = 2) +
  theme_minimal() +
  labs(title = "Device Usage per Hour",
       x = "Hour",
       y = "Average Number of Items Played") +
  scale_color_brewer(palette = "Set2") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 9),
        legend.position = c(.1, .8))

#-------------- Device by Grade --------------
grade_data <- inner_join(records, devices) %>%
  select(user_id, grade, device)
grade_data <- unique(grade_data)

grade_count <- grade_data %>%
  group_by(grade, device) %>%
  summarize(count = n())

plot4 <- ggplot(grade_count, aes(x = grade, y = count, fill = factor(device))) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(title = "Device Usage by Grade",
       x = "Grade",
       y = "Number of Devices") +
  scale_fill_brewer(palette = "Set2") +
  scale_x_continuous(breaks = seq(1, 9, 1),
                     labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = c(.1, .8),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 9))

#-------------- Dashboard --------------
left_plot <- plot_grid(plot4, plot3, ncol = 1)
right_plot <- plot_grid(plot1, plot2, ncol = 1)
all_plot <- plot_grid(left_plot, right_plot)
title <- ggdraw() + draw_label("User Behaviors on Different Device Types",
                               fontface = "bold", size = 16)
final_plot <- plot_grid(title, all_plot, ncol = 1, rel_heights = c(0.1, 1))

# save file
ggsave(filename = "final.pdf", final_plot,
       width = 16, height = 9, device = "pdf")