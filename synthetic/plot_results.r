
library(ggplot2)
library(reshape2) # For melt function


plot_slice <- function(tensor, k=1,xlab,ylab,yes=FALSE) {
  df_long <- data.frame()
  # Extract a slice from the 2nd dimension
  # Convert each slice (along the second dimension) of the tensor to long format
  for (i in 1:dim(tensor)[k]) {
    if (k==1){
      ts=tensor[i, ,]
    }else if (k==2){
      ts=tensor[,i ,]
    }else{
      ts=tensor[, ,i ]
    }
    slice_df <- as.data.frame(ts) %>%
      mutate(X = row_number(), Slice = i) %>%
      pivot_longer(cols = -c(X, Slice), names_to = "Y", values_to = "Value") %>%
      mutate(Y = as.numeric(gsub("V", "", Y))) # Convert Y to numeric
    
    df_long <- rbind(df_long, slice_df) # Bind each slice's data frame to the main data frame
  }
  
  # Adjust factors for plotting
  df_long$Slice <- factor(df_long$Slice)
  
  if (yes==TRUE){
    df_long$X=as.factor(df_long$X)
    df_long$Y=as.factor(df_long$Y)
  }
  # Plotting
  g=ggplot(df_long, aes(x = X, y =Y, fill = Value)) +
    geom_tile() +  # Create the heatmap
    scale_fill_viridis_c(limits=c(0,1),option="H",trans="sqrt") +  # Use a color scale that's visually appealing for heatmaps
    facet_wrap(~Slice, ncol = 10) +  # Facet by slice, adjust ncol for desired layout
    labs(x = xlab, y = ylab, fill = "") +
    theme_minimal() +  # Use a minimal theme
    theme(axis.text.x = element_text(angle = 45, hjust = 1), # Improvehttp://127.0.0.1:18569/graphics/5001c0ec-4c59-4d8d-8e0d-a42a99b7917e.png x-axis label readability
          #strip.background = element_rect(fill = "lightblue"), # Customize facet label background
          strip.text = element_text(face = "bold"))  # Bold facet labels
  
  
  print(g)
}


heatmap_matrix <- function(matrix_data,xlab,ylab){
  # Convert matrix to data frame in long format
  df <- as.data.frame(matrix_data) %>%
    rownames_to_column("Row") %>%
    pivot_longer(cols = -Row, names_to = "Column", values_to = "Value") %>%
    mutate(Row = as.numeric(Row), Column = as.numeric(gsub("V", "", Column))) # Convert Row and Column to numeric
  
  # Create heatmap
  g=ggplot(df, aes(x = as.factor(Column), y =( Row), fill = Value)) +
    geom_tile() +
    scale_fill_viridis_c(limits=c(0,1),option="H",trans="sqrt") +  # Use a color scale that's visually appealing for heatmaps
    # scale_fill_viridis_c() +  # Us
    labs(x = xlab, y = ylab, fill = "") +
    #scale_y_continuous(breaks=seq(0,max(as.numeric(df$Row)),by=5))+
    theme_minimal() +  # Use a minimal theme
    theme(#axis.text.x = element_text(angle = 45, hjust = 1), # Improve x-axis label readability
          strip.background = element_rect(fill = "lightblue"), # Customize facet label background
          strip.text = element_text(face = "bold"))  # Bold facet labels
  print(g)
}

heatmap_matrix2<- function(matrix_data,xlab,ylab){
  # Convert matrix to data frame in long format
  df <- as.data.frame(matrix_data) %>%
    rownames_to_column("Row") %>%
    pivot_longer(cols = -Row, names_to = "Column", values_to = "Value") %>%
    mutate(Row = as.numeric(Row), Column = as.numeric(gsub("V", "", Column))) # Convert Row and Column to numeric
  
  # Create heatmap
  g=ggplot(df, aes(x = as.factor(Column), y =( Row), fill = Value)) +
    geom_tile() +
    scale_fill_viridis_c(limits=c(0,1),option="H",trans="sqrt") +  # Use a color scale that's visually appealing for heatmaps
    # scale_fill_viridis_c() +  # Us
    labs(x = xlab, y = ylab, fill = "") +
    scale_y_continuous(breaks=seq(0,max(as.numeric(df$Row)),by=5))+
    theme_minimal() +  # Use a minimal theme
    theme(#axis.text.x = element_text(angle = 45, hjust = 1), # Improve x-axis label readability
      strip.background = element_rect(fill = "lightblue"), # Customize facet label background
      strip.text = element_text(face = "bold"))  # Bold facet labels
  
  
  print(g)
}