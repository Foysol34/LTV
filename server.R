library(httpuv)
library(shiny)
library(dplyr)
library(plyr)

options(browser = "C:/Program Files/Google/Chrome/Application/chrome.exe")
options(shiny.maxRequestSize=30*1024^2) 

generate_order_size_distribution <- function(cleaned_data){
  
  unique_email_addresses <- cleaned_data$Email[!duplicated(cleaned_data$Email)]
  
  utility_vector <- vector()
  generated_df <- data.frame()
  for(i in unique_email_addresses){
    
    partial_df <- cleaned_data[cleaned_data$Email==i,]
    utility_vector <- vector()
    total_order_value <- 0
    # In the following loop we want to construct a vector corresponding to one row of the final output dataframe
    for(j in 1:nrow(partial_df)){
      if(nrow(partial_df) == 1){
        # comment
        utility_df <- partial_df[j,c("Email","Paid.at","Total","Financial.Status", "Made.Order")]
        total_order_value <- total_order_value + utility_df$Total
        colnames(utility_df) <- c("Email","Paid.at.1","Total.1","Financial.Status.1", "Made.Order.1")
        utility_vector <- c(utility_vector, utility_df)
        break
      }
      if(j==1){
        # Grab the email address on the first entry 
        utility_df <- partial_df[j,c("Email","Paid.at","Total","Financial.Status", "Made.Order")]
        total_order_value <- total_order_value + utility_df$Total
        colnames(utility_df) <- c("Email","Paid.at.1","Total.1","Financial.Status.1", "Made.Order.1")
        utility_vector <- c(utility_vector, utility_df)
        next
      }
      list_to_add <- partial_df[j,c("Paid.at","Total", "Financial.Status", "Made.Order")]
      total_order_value <- total_order_value + list_to_add$Total
      colnames(list_to_add)<-c(paste("Paid.at",j,sep = "."), paste("Total",j,sep = "."), paste("Financial.Status",j,sep = "."), paste("Made.Order",j,sep = "."))
      utility_vector <- c(utility_vector,list_to_add)
      days_diff <- unlist(utility_vector[paste("Paid.at",j,sep = ".")])-unlist(utility_vector[paste("Paid.at",j-1,sep = ".")])
      names(days_diff)<-paste("days.diff",j,sep = ".")
      utility_vector <- c(utility_vector, days_diff)
    }
    
    total_order_count <- j 
    names(total_order_count)<-"Total.order.count"
    names(total_order_value)<-"Total.order.value"
    utility_vector <- c(utility_vector, total_order_count, total_order_value)
    # Append the transposed vector to the overall dataframe
    generated_df <- rbind.fill(generated_df, as.data.frame(utility_vector))
    
    generated_df <- generated_df %>%
      select(Email,Total.order.count, Total.order.value, everything())
  }
  return(generated_df)
}

server <- function(input, output) {
  
  state <- reactiveValues(display_df=NULL)
  
  
  
  # UI Datatable
  output$contents <- renderDataTable({
    
    inFile <- input$orders_export
    
    if (is.null(inFile))
      return(NULL)
    
    # Read in the initial orders file csv
    complete_df <- data.frame(read.csv2(inFile$datapath, sep = ","), stringsAsFactors = FALSE)
    
    # Convert to required data types and format. Add in an extra 'Made.Order' column 
    complete_df$Email <- as.character(complete_df$Email)
    complete_df$Financial.Status <- as.character(complete_df$Financial.Status)
    complete_df$Total <- as.numeric(as.character(complete_df$Total))
    complete_df$Total[is.na(complete_df$Total)] <- 0
    complete_df$Paid.at <- as.Date(substr(complete_df$Paid.at,0,10))
    complete_df <- complete_df[order(complete_df$Paid.at, decreasing = FALSE),]
    complete_df$Made.Order <- rep(1, nrow(complete_df))
    
    # Extract only the relevant rows and columns from the complete dataset
    working_df <- complete_df %>%
      select(Email, Financial.Status, Paid.at, Total, Made.Order) %>%
      filter(Financial.Status == "paid" | Financial.Status == "refunded" | Financial.Status == "partially_refunded") %>%
      filter(Total > 0) %>% # Filtering out any zero 'Total's
      filter(!is.na(Paid.at)) # Filtering out entries without valid 'Paid.at' dates (e.g. blank fields)
    
    state$display_df <- generate_order_size_distribution(working_df)
    
  })
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {"OrderSizeDistributionOverview.csv"},
    content = function(file) {
      write.table(state$display_df, file, na = "", sep = ",", row.names = FALSE)
    }
  )
  
  }
