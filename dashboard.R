library("oefenwebDatabase")
library("ggplot2")
library("tidyverse")
library("RColorBrewer")
library("extrafont")

# to put the plots together
suppressMessages(library("cowplot"))
# to prepare data
suppressMessages(library("dplyr"))
# make database connection
con <- connect()

# Part 1
# get the log_records from 2018-12-08 to 2018-12-09
records <- suppressWarnings(DBI::dbGetQuery(con, "SELECT `user_agent_id`, AVG(`response_in_milliseconds`)
                                         FROM `log_records`
                                         WHERE `created` >= '2018-12-08' AND `created` < '2018-12-09'
                                         GROUP BY `user_agent_id`"))
names(records)[1] <- "id"

# get the user_agents table
device <- suppressWarnings(DBI::dbGetQuery(con, "SELECT * FROM `user_agents`"))

# join 2 tables
data <- inner_join(records, device)

# remove Mozilla/5.0 from the strings
for (i in 1:nrow(data)){
  data[i, 3] <- substring(data[i, 3], 14)
}

# get the device type from the strings
data[, 3] <- sapply(strsplit(data[, 3], ";"), "[", 1)
data[, 3] <- sapply(strsplit(data[, 3], " "), "[", 1)

# rename the columns
names(data)[2] <- "avg_response"
names(data)[3] <- "device"

# rename "x11" to "Linux"
data$device <- gsub("X11", "Linux", data$device)

# bar plot
ggplot(data, aes(x = factor(device), y = avg_response, fill = factor(device) ) ) +
  stat_summary(fun.y = "mean", geom = "bar") +
  theme_classic() +
  labs(title = "Response Time on Different Device Types",
       subtitle = "from 2018-12-08 to 2018-12-09",
       x = "Device Type", y = "Average Response Time \n (milliseconds)") +
  scale_y_continuous(breaks = seq(0, 10000, 500)) +
  scale_fill_brewer(palette = "Pastel2") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none")
  


# Part 2
userData <- suppressWarnings(DBI::dbGetQuery(con,
                                             "SELECT `users`.*,
                                             `app_prop`.`coin_count`,
                                             `app_prop`.`difficulty`,
                                             `count_tab`.`modified_count`
                                             FROM `users`
                                             INNER JOIN `user_application_properties` AS `app_prop`
                                             ON `app_prop`.`user_id` = `users`.`id`
                                             LEFT JOIN (
                                             SELECT `user_id`, SUM(`modified_count`) AS `modified_count`
                                             FROM `user_domain_ratings`
                                             GROUP BY `user_id`) AS `count_tab`
                                             ON `users`.`id` = `count_tab`.`user_id`
                                             -- select only players (no teachers etc.)
                                             WHERE `users`.`role_id` = 1"))

userData <- userData %>%
  filter(coin_count < as.numeric(quantile(userData$coin_count, prob = 0.9))
         & !is.na(modified_count)
         & grade %in% c(3:9)
         & coin_count > 0)

userData$difficulty <- factor(userData$difficulty)

facetname <- c(`0` = "Easy", `1` = "Medium", `2` = "Hard*")
ann_text <- data.frame(x = 20000, y = 200, label = "*On average the \n most coins", difficulty = 2)

plot1 <- ggplot(userData, aes(coin_count, fill = factor(difficulty))) +
  geom_histogram(color = "black") +
  scale_fill_brewer(palette = "Blues") +
  facet_grid(~difficulty, labeller = as_labeller(facetname)) +
  ggtitle("Coins vs. Difficulty Setting") +
  xlab("Coins earned") + ylab("Freqency") +
  theme_cowplot() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust = .5)) +
  geom_text(data = ann_text, aes(x = x, y = y, label = label))

plot2 <- ggplot(userData, aes(x = grade, y = coin_count / 1000, fill = factor(grade))) +
  ylab("Coins earned \n in thousand") +
  geom_boxplot( ) +
  scale_fill_brewer(palette = "PuRd") +
  theme_minimal( ) +
  scale_x_continuous(breaks = seq(3, 9, 1),
                     labels = c("Grade 3", "Grade 4", "Grade 5", "Grade 6", "Grade 7", "Grade 8", "Grade 9")) +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = .5))

fit <- lm(data = userData, formula = coin_count ~ modified_count)
adjR2 <- summary(fit)$adj.r.squared # nolint
intercept <- fit$coef[[1]]
slope <- fit$coef[[2]]
pValue <- summary(fit)$coef[2, 4]

plot3 <- ggplot(data = userData, aes(x = modified_count, y = coin_count)) +
  scale_x_continuous(trans = "log2", limits = c(10, max(userData$modified_count))) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "lm", colour = "red", size = 0.5) +
  theme_bw() +
  labs(title = "Relation between Coins and Number of Items Played",
       subtitle = paste("Adj R2 = ", round(adjR2, 2), "Intercept = ",
                        round(intercept, 2), "Slope =", round(slope, 2), "P >", round(pValue, 2)),
       x = "Number of items played",
       y = "Coins earned") +
  scale_y_continuous(limit = c(0, 40000),
                     labels = c("0K", "10K", "20K", "30K", "40K"))

upper_plot <- plot_grid(plot1, plot2)
final_plot <- plot_grid(upper_plot, plot3, ncol = 1, rel_heights = c(1, 2))
ggsave("part2.pdf", final_plot)
