install.packages('devtools')
devtools::install_github('Oefenweb/r-database')
oefenwebTools::lintrProfile("myfile.R")
devtools::install_github('Oefenweb/r-tools')
con = oefenwebDatabase::connect()
my_data_domains <- DBI::dbGetQuery(con, "SELECT * FROM domains")
my_data_domains <- suppressWarnings(DBI::dbGetQuery(con, "SELECT * FROM domains"))
my_data_user <- suppressWarnings(DBI::dbGetQuery(con, 
                                 "SELECT user_id, users.gender, q_score, domain_id, grade
                                 FROM user_domain_ratings
                                 LEFT JOIN users ON user_domain_ratings.user_id = users.id
                                 WHERE user_domain_ratings.created > '2018-01-01' 
                                 AND grade BETWEEN 2 AND 7"))

install.packages("tidyverse")
library(tidyverse)
# select addition and multiplication
clean_data <- my_data_user %>% 
  subset(domain_id %in% c(1,3))
# make factors
clean_data$grade <- as.factor(clean_data$grade)
clean_data$domain_id <- as.factor(clean_data$domain_id)
# omit missing data:
clean_data <- na.omit(clean_data)

# ggplot(data = clean_data, 
#        aes(x = grade, 
#            y  = q_score, 
#            colour = gender)) +
#   geom_point()

# original plot
p_out <- ggplot(data = clean_data,
                aes(x = grade, y  = q_score)) +
  geom_boxplot(aes(fill = domain_id, linetype = gender)) +
  xlab("Grade") +
  ylab("Quantile-Scores")

ggsave("original.pdf", plot = p_out)

# comparing females v.s. males
p_out <- ggplot(data = clean_data, 
                aes(x = grade, y  = q_score)) +
  geom_boxplot(aes(fill = gender)) +
  facet_grid(~domain_id) +
  xlab("Grade") + 
  ylab("Quantile-Scores")

ggsave("mod.pdf", plot = p_out)

# comparing domain_id
p_out <- ggplot(data = clean_data, 
                aes(x = grade, y  = q_score)) +
  geom_boxplot(aes(fill = domain_id)) +
  facet_grid(~gender) +
  xlab("Grade") + 
  ylab("Quantile-Scores")

ggsave("mod2.pdf", plot = p_out)
