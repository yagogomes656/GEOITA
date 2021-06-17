library(caret)
library(foreach)
library(readr)
library(dplyr, warn.conflicts = FALSE)

combinations <-
  expand.grid(seed = 1,    #Sets the algorithm, metric, dataset and case study combinations
              algorithm = c("rf", "bridge", "brnn", "svmRadial", "knn", "treebag", "kknn", "lm"),
              dataset = c("meyer", "dq"), #c("meyer", "dq", "all"),
              metric = c("RMSE", "Rsquared"),
              case = 1:3,
              stringsAsFactors = FALSE)

full_data <- #Full data, excluding examples used in case study
  read_delim("data.csv", delim = ",", col_types = cols(.default = col_double())) %>%
  select(-c(N_pile, Dq, Mey)) %>%
  { .[-c(120, 121, 122), ] }

case_data <- #Data for case study
  read_delim("data.csv", delim = ",", col_types = cols(.default = col_double())) %>%
  select(-c(N_pile, Dq, Mey)) %>%
  { .[120:122, ] }

best <- function(x, metric) if (metric == "RMSE") min(x) else max(x)


results <- foreach(r = 1:nrow(combinations),
                   .combine = rbind,
                   .errorhandling = "stop") %do%
{
  with(combinations[r, ], {

    print(combinations[r, ])

    data <- if (dataset != "all") {
      suffix <- if (dataset == "meyer") "Dq" else "Mey"
      full_data %>% select(!ends_with(suffix))
    } else {
      full_data
    }

    test_data <- if (dataset != "all") {
      suffix <- if (dataset == "meyer") "Dq" else "Mey"
      case_data %>% select(!ends_with(suffix))
    } else {
      case_data
    }

    set.seed(seed)
    #Model training with 'leave one out cross validation' data split
    model <- train(Qu ~ ., data = data,
                   trControl = trainControl(method = "LOOCV",
                                            allowParallel = FALSE),
                   method = algorithm, metric = metric)
    print(model$pred)
    #Predicted values for the case study
    ypredicted <- predict(model, newdata = test_data[case, ]) %>% unlist()

    set.seed(seed)
    #Improved model, considering case study examples in the training dataset
    model_impr <- train(Qu ~ ., data = rbind(data, test_data[-case, ]),
                   trControl = trainControl(method = "LOOCV",
                                            allowParallel = FALSE),
                   method = algorithm, metric = metric)
   
    #New predictions considering the improved model applied to the case study
    ypred_impr <- predict(model_impr, newdata = test_data[case, ]) %>% unlist()
    
    #Results organized
    as_tibble(combinations[r, ]) %>%
      mutate(value = best(model$results[[metric]], metric),
             first_prediction = ypredicted,
             second_prediction = ypred_impr,
             first_error = abs(ypredicted - test_data$Qu[case]) / test_data$Qu[case],
             second_error = abs(ypred_impr - test_data$Qu[case]) / test_data$Qu[case])
  })
}

write_csv(results, "results.csv")
