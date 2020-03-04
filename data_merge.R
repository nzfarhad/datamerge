
library(dplyr)
library(tidyr)
library(readxl)

# load results file (Column names: dependent.var,	dependent.var.value,	independent.var, independent.var.value,	numbers,	is_ranked)
# is_ranked column value: Yes for variables that needs to be ranked, otherwise empty

# Rank value (top 3)
rank_value <- 3

results <- read_excel("input/results_all_indicators_Province_disagg.xlsx")


# pivot wide 1
results_datamerge <- pivot_wider(results, 
                                 id_cols = c("independent.var","independent.var.value"),
                                 values_from = "numbers",
                                 names_from = c("dependent.var", "dependent.var.value") )

# pivot wide 2 - one row 
# remove is_ranked column if it exists in results
results2 <- results %>% select(-c(is_ranked))
results_datamerge_one_row <- pivot_wider(results2, 
                                 values_from = "numbers",
                                 names_from = c("dependent.var", "dependent.var.value", "independent.var", "independent.var.value" ))

# Output
write.csv(results_datamerge, "output/data_merge.csv", row.names = F)
write.csv(results_datamerge_one_row, "output/data_merge_single_row.csv", row.names = F)


#######################################################################################
# Rank numbers

# is_ranked column to lowercase
results$is_ranked <- tolower(results$is_ranked)

results_ranked <- results %>% 
  group_by(independent.var.value, dependent.var) %>% 
  mutate(
    rank = rank(-numbers, na.last = T, ties.method= "first"),
    rank = case_when(
      rank > rank_value | is_ranked !="yes" ~ NA_real_,
      TRUE ~ as.double(rank)  
    )
  ) %>% arrange(dependent.var, rank) %>% 
  filter((is_ranked == "yes") & !is.na(rank)) 


# Ranked data merge
ranked_data_merge <- pivot_wider(results_ranked, 
                                      id_cols = c("independent.var","independent.var.value"),
                                      values_from = "numbers",
                                      names_from = c("dependent.var","rank") )


# names 
ranked_data_merge_names <- pivot_wider(results_ranked, 
                                               id_cols = c("independent.var","independent.var.value"),
                                               values_from = "dependent.var.value",
                                               names_from = c("dependent.var","rank"))
names(ranked_data_merge_names) <- paste0(names(ranked_data_merge_names),"_","op_name")

# Join values and names
ranked_data_merge_final <- ranked_data_merge %>%  
  full_join(ranked_data_merge_names, by = c("independent.var.value" = "independent.var.value_op_name")) %>% 
  select(-c(independent.var_op_name))


# Ranked output
write.csv(ranked_data_merge_final, "output/ranked_datamerge.csv", row.names = F)

