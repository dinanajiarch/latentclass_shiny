library(tidyverse)
library(dplyr)
library(here)



data <-
  readModels(
    "C:/Users/Dina Arch/Box/ESM 244/Shiny/example/lsay_pr3_3step_maineffects no interaction  2 nostart.out",
    what = c("sampstat", "parameters", "class_counts")
  )


# Extract Means
means <- as.data.frame(data[["sampstat"]][["means"]])

# Extract dummy codes
cov_codes <-
  as.data.frame(data[["sampstat"]][["univariate.sample.statistics"]]) %>%
  select(Minimum, Maximum) %>% 
  t() %>% 
  as.data.frame()


# Extract names of Covariates
cov_names <- names(means)

# Relabel
first_cov <- cov_names[1]
non_first_cov <- paste("non", cov_names[1])
sec_cov <- cov_names[2]
non_sec_cov <- paste("non", cov_names[2])

# Extract logits
logits <-
  as.data.frame(data[["parameters"]][["unstandardized"]]) %>%
  filter(paramHeader != "Means") %>%
  filter(paramHeader != "New.Additional.Parameters") %>%
  select(paramHeader, param, est)

# Extract intercept
intercept <- logits %>%
  filter(paramHeader == "Intercepts")

# Extract logits
beta <- logits %>%
  filter(paramHeader != "Intercepts")


# Create class size in percentages (%)
c_size <-
  as.data.frame(data[["class_counts"]][["modelEstimated"]][["proportion"]])
colnames(c_size) <- paste0("cs")
c_size <- c_size %>%
  mutate(cs = round(cs * 100, 2))


### Convert logits to probabilities

# First, how many covariates do they have?
cov_no <- cov_names %>%
  length()

# Second, how many classes do they have?
class_no <-
  as.data.frame(data[["class_counts"]][["modelEstimated"]][["class"]]) %>%
  nrow()

# Prep data for analysis
# COV#1: FEMALE
beta_2cov_1 <- logits %>%
  filter(param == cov_names[1]) %>%
  select(est) %>%
  rename(beta1 = est)


# COV#2: URM
beta_2cov_2 <- logits %>%
  filter(param == cov_names[2]) %>%
  select(est) %>%
  rename(beta2 = est)

#Intercept
int <- intercept %>%
  select(est) %>%
  rename(int = est)


#Mean 1: URM
#Mean 2: FEMALE
rename_means <- means %>%
  rename(mean1 = cov_names[1],
         mean2 = cov_names[2])

df <-
  cbind(int, beta_2cov_1, beta_2cov_2, rename_means) %>%
  as.data.frame()


# Reference Class
ref_class <- class_no - 1

# Create function for two covariates
cov <- function(int, beta1, mean1, beta2, mean2) {
  exp(int + (beta1) * (mean1) + (beta2) * (mean2))
}

## COVARIATE 1

#Covariate 1, 0
cov1_0 <- NULL
for (i in 1:ref_class) {
  x <- cov(df[i, 1], df[i, 2], cov_codes[1,1], df[i, 3], df$mean2)
  cov1_0 <- rbind(cov1_0, x)
  
}

cov1_0 <- as.data.frame(cov1_0) %>%
  select(1) %>%
  remove_rownames() %>%
  rename(COV1_0 = V1)
cov1_0 <- cov1_0  %>%
  add_row(COV1_0 = 1)


# Convert to probabilities


prob0 <- cov1_0 %>%
  mutate(prob0 = COV1_0 / sum(COV1_0)) %>% 
  select(prob0)

colnames(prob0) <- glue("{non_first_cov}")


#Covariate 1, 1
cov1_1 <- NULL
for (i in 1:ref_class) {
  x <- cov(df[i, 1], df[i, 2], cov_codes[2,1], df[i, 3], df$mean2)
  cov1_1 <- rbind(cov1_1, x)
  
}

cov1_1 <- as.data.frame(cov1_1) %>%
  select(1) %>%
  remove_rownames() %>%
  rename(COV1_1 = V1)
cov1_1 <- cov1_1  %>%
  add_row(COV1_1 = 1)


# Convert to probabilities
prob1 <- cov1_1 %>%
  mutate(prob1 = COV1_1 / sum(COV1_1)) %>% 
  select(prob1)
  colnames(prob1) <- glue("{first_cov}")

# Add class labels
cov1_prob <- cbind(prob0, prob1) %>%
  add_column(class = paste0("Class ", 1:class_no, glue(" ({c_size[1:class_no,]}%)")))

#Plot dataframe

plot_df <- cov1_prob %>% 
  pivot_longer(!class, names_to = "cov1", values_to = "Prob")

# Plot

plot_df %>%
  ggplot(aes(x= class, y = Prob, fill = cov1)) +
  geom_bar(stat = "identity", position=position_dodge(), colour="black") +
  geom_text(aes(label = round(Prob,2)), vjust=-1, size=3.5,
            position = position_dodge(0.9))+
  scale_fill_grey(start = .4, end = .8) +
  theme_classic() +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    text = element_text(size = 12)
  ) +
  labs(title = "Binary Covariate Plot", x = "", y = "Probabilities") 


## COVARIATE 2

#Covariate 2, 0
cov2_0 <- NULL
for (i in 1:ref_class) {
  x <- cov(df[i, 1], df[i, 2], df$mean1, df[i, 3], cov_codes[1,1])
  cov2_0 <- rbind(cov2_0, x)
  
}

cov2_0 <- as.data.frame(cov2_0) %>%
  select(1) %>%
  remove_rownames() %>%
  rename(COV2_0 = V1)
cov2_0 <- cov2_0  %>%
  add_row(COV2_0 = 1)


# Convert to probabilities


prob0 <- cov2_0 %>%
  mutate(prob0 = COV2_0 / sum(COV2_0)) %>% 
  select(prob0)

colnames(prob0) <- glue("{non_sec_cov}")


#Covariate 1, 1
cov2_1 <- NULL
for (i in 1:ref_class) {
  x <- cov(df[i, 1], df[i, 2], df$mean1, df[i, 3], cov_codes[2,1])
  cov2_1 <- rbind(cov2_1, x)
  
}

cov2_1 <- as.data.frame(cov2_1) %>%
  select(1) %>%
  remove_rownames() %>%
  rename(COV2_1 = V1)
cov2_1 <- cov2_1  %>%
  add_row(COV2_1 = 1)


# Convert to probabilities
prob1 <- cov2_1 %>%
  mutate(prob1 = COV2_1 / sum(COV2_1)) %>% 
  select(prob1)
colnames(prob1) <- glue("{sec_cov}")

# Add class labels
cov2_prob <- cbind(prob0, prob1) %>%
  add_column(class = paste0("Class ", 1:class_no, glue(" ({c_size[1:class_no,]}%)")))

#Plot dataframe

plot_df <- cov2_prob %>% 
  pivot_longer(!class, names_to = "cov2", values_to = "Prob")

# Plot 

plot_df %>%
  ggplot(aes(x= class, y = Prob, fill = cov2)) +
  geom_bar(stat = "identity", position=position_dodge(), colour="black") +
  geom_text(aes(label = round(Prob,2)), vjust=-1, size=3.5,
            position = position_dodge(0.9))+
  scale_fill_grey(start = .4, end = .8) +
  theme_classic() +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    text = element_text(size = 12)
  ) +
  labs(title = "Binary Covariate Plot", x = "", y = "Probabilities") 

