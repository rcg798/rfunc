###############
#
# FUNCTION
#
###############

# function has 4 inputs:
# x is dfA
# y is dfB
# merge_by is the list of fields you are using to merge
# example formats: 
# if one field common to both dfs: c('field')
# if field name differs across dfs: c('field_name1'='field_name2')
# if multiple fields: c('field1', 'field2')
# merge_type is the type of merge: inner, left, right, full

merge_and_tell = function(x, y, merge_by, merge_type) {
  
  # load dependencies 
  library(dplyr)
  
  # if an inner join
  if(merge_type=='inner') {
    merged_results <- x %>%
      mutate(is_x=1) %>% # the is_x and is_y will allow me to create the '_merge' column
      inner_join((y%>%mutate(is_y=1)), by=merge_by) 
  }
  # if a left join
  if(merge_type=='left') {
    merged_results <- x %>%
      mutate(is_x=1) %>% # the is_x and is_y will allow me to create the '_merge' column
      left_join((y%>%mutate(is_y=1)), by=merge_by) 
  }
  # if a right join
  if(merge_type=='right') {
    merged_results <- x %>%
      mutate(is_x=1) %>% # the is_x and is_y will allow me to create the '_merge' column
      right_join((y%>%mutate(is_y=1)), by=merge_by) 
  }
  # if a full outer join
  if(merge_type=='full') {
    merged_results <- x %>%
      mutate(is_x=1) %>% # the is_x and is_y will allow me to create the '_merge' column
      full_join((y%>%mutate(is_y=1)), by=merge_by) 
  }
  
  # df output
  merged_results <- merged_results %>%
    mutate('_merge'=ifelse(!is.na(is_x)&!is.na(is_y), 1, 
                           ifelse(!is.na(is_x)&is.na(is_y), 2, 3))) 
  
  # diagnostic output
  diagnostics <- merged_results %>%
    summarise(rows_in_x_and_y=sum(`_merge`==1), # num rows from dfA AND dfB
              rows_in_x_not_y=sum(`_merge`==2), # num rows from dfA BUT NOT dfB
              rows_in_y_not_x=sum(`_merge`==3)) # num rows from dfB BUT NOT dfA
  
  merged_results <- merged_results %>% select(-is_x, -is_y) # we don't need these columns anymore
  
  final_list <- list(merged_results, diagnostics) # make a list to return
  
  return (final_list)
}

###############
#
# EXAMPLES
#
###############

# to load your own dfs from csvs, please use the following format
# df <- read.csv("csv_name.csv")
# documentation on usage: https://stat.ethz.ch/R-manual/R-devel/library/utils/html/read.table.html

# the following example does a full join of 2 built-in R datasets, beaver1 and beaver2
merge_and_tell(beaver1, beaver2, c('day'), 'full')