library(tidyverse)
library(keras)
library(lime)
library(rsample)
library(recipes)
library(yardstick)
library(corrr)

#Import Data
churn_data_raw <- read.csv("Scripts-personal/WA_Fn-UseC_-Telco-Customer-Churn.csv")
churn_data_raw%>%glimpse()
churn_data_tbl <- churn_data_raw %>%
  select(-customerID) 

churn_data_tbl<-na.omit(object =churn_data_tbl )
churn_data_tbl<-churn_data_tbl %>% select(Churn, everything())

set.seed(100)
train_test_split <- sample(c(rep(0, 0.8* nrow(churn_data_tbl)), rep(1, 0.2 * nrow(churn_data_tbl))))

train_test_split
data_train <- churn_data_tbl[train_test_split == 0, ]  
data_test <- churn_data_tbl[train_test_split == 1, ]  
hist(churn_data_tbl$tenure)
churn_data_tbl %>% ggplot(aes(x = tenure)) + 
  geom_histogram(bins = 6, color = "white", fill =  "#2DC6D6") +
  labs(
    title = "Tenure Counts With Six Bins",
    x     = "tenure (month)"
  )
hist(churn_data_tbl$TotalCharges)
hist(log(churn_data_tbl$TotalCharges))
data_train %>%
  select(Churn, TotalCharges) %>%
  mutate(
    Churn = Churn %>% as.factor() %>% as.numeric(),
    LogTotalCharges = log(TotalCharges)
  ) %>%
  correlate() %>%
  focus(Churn) %>%
  fashion()
churn_data_tbl %>% 
  pivot_longer(cols      = c(Contract, InternetService, MultipleLines, PaymentMethod), 
               names_to  = "feature", 
               values_to = "category") %>% 
  ggplot(aes(category)) +
  geom_bar(fill = "#2DC6D6") +
  facet_wrap(~ feature, scales = "free") +
  labs(
    title = "Features with multiple categories: Need to be one-hot encoded"
  ) +
  theme(axis.text.x = element_text(angle = 25, 
                                   hjust = 1))


#Create Recipe
rec_obj <- recipe(Churn ~ ., data = data_train) %>%
  step_rm(Churn) %>% 
  step_discretize(tenure, options = list(cuts = 6)) %>%
  step_log(TotalCharges) %>%
  step_dummy(all_nominal(), -all_outcomes(), one_hot = T) %>%
  step_center(all_predictors(), -all_outcomes()) %>%
  step_scale(all_predictors(), -all_outcomes()) %>%
  prep(data = data_train)

x_train_tbl <- bake( rec_obj , new_data = data_train)
x_test_tbl  <- bake( rec_obj , new_data = data_test)

y_train_vec <- ifelse( ) #figure this out
y_test_vec  <- ifelse( ) #figure this out

model_keras <- keras_model_sequential()

model_keras %>% 
  # First hidden layer
  layer_dense(
    units              = 16, 
    kernel_initializer = "uniform", 
    activation         = "relu", 
    input_shape        = ncol(x_train_tbl)) %>% 
  # Dropout to prevent overfitting
  layer_dropout(rate = 0.1) %>%
  # Second hidden layer
  layer_dense(
    units              = 16, 
    kernel_initializer = "uniform", 
    activation         = "relu") %>% 
  # Dropout to prevent overfitting
  layer_dropout(rate = 0.1) %>%
  # Output layer
  layer_dense(
    units              = 1, 
    kernel_initializer = "uniform", 
    activation         = "sigmoid") %>% 
  # Compile ANN
  compile(
    optimizer = 'adam',
    loss      = 'binary_crossentropy',
    metrics   = c('accuracy')
  )
model_keras

fit_keras <- fit(
  x_train_tbl,
  y_train_vec,  
  epochs              = 35 , 
  batch_size              = 50 , 
  validation_split = 0.3  
  
  
)

fit_keras

plot(fit_keras) +
  labs(title = "Deep Learning Training Results") +
  theme(legend.position  = "bottom", 
        strip.placement  = "inside",
        strip.background = element_rect(fill = "#grey"))


yhat_keras_class_vec <- predict_classes(object = model_keras, x = as.matrix(x_test_tbl)) %>%
  as.vector()

# Predicted Class Probability
yhat_keras_prob_vec  <- predict_proba(object = model_keras, x = as.matrix(x_test_tbl)) %>%
  as.vector()

estimates_keras_tbl <- tibble(
  truth      = as.factor(y_test_vec) %>% fct_recode(yes = "1", no = "0"),
  estimate   = as.factor(yhat_keras_class_vec) %>% fct_recode(yes = "1", no = "0"),
  class_prob = yhat_keras_prob_vec
)

