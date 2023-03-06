

# this chunk of codes works the same way the "Session => Set Working Directory => To Source File Location" does
get_backto_WD = function() {
  {
    library(rstudioapi)
    # Get the path of the active file
    active_file <- getActiveDocumentContext()$path
    file_path <- rstudioapi::getSourceEditorContext()$path
    if (is.null(file_path)) {
      file_path <- active_file
    }

    # Set the working directory to the parent directory
    setwd(file.path(file_path, ".."))
  }
}


# This chunk of codes is to check if there is any repetitive locations (i.e., same lat and long)
test_repetitive_locs = function(data) {

  data = data

  for (year in unique(data$year)) {

    X1 = length(unique(data$positionStr[data$year==year]))
    X2 = dim(data[data$year==year,])[1]

    difference = try(X2 - X1, silent = TRUE)

    if (difference != 0) {
      print(paste("Difference = ", difference, sep = ""))
    } else {
      print("Difference = 0")
    }
  }
}


make_effortdens_rt = function(data, Tri_Area = TriList$Tri_Area) {

  data = data
  df1 = data.frame("year" = data$year,
                   "r_i" = data$r_i,
                   "effort" = data$effort)
  # compute cross-tabulated object
  df2 = as.data.frame.matrix(xtabs(effort ~ r_i + year, data = df1))

  Tri_Area = Tri_Area
  # create a data frame of NA with ncol=number of years & nrow=number of triangles
  {
    df3 = as.data.frame(matrix(ncol = length(unique(df1$year)),
                               nrow = length(unique(Tri_Area)))
    )
    rownames(df3) = c(1:length(unique(Tri_Area)))
    colnames(df3) = sort(unique(df1$year))
  }

  # replace NA by corresponding values
  for (i in 1:nrow(df3)) {
    row_name <- rownames(df3)[i]
    if (row_name %in% rownames(df2)) {
      row_index <- which(rownames(df2) == row_name)
      df3[i, ] <- df2[row_index, ]
    } else {
      df3[i, ] <- 0
    }
  }

  df4 = df3

  df5 = data.frame("r_i" = c(1:length(TriList$Tri_Area)),
                   "Tri_Area" = TriList$Tri_Area)

  for (col in names(df4)) {
    df4[, col] <- df4[, col] / df5[match(rownames(df4), df5$r_i), "Tri_Area"]
  }

  effortdens_rt = df4

  return(effortdens_rt)

}










# ------------------ TEST CODE ------------------ #
# year_test = c(2001:2002,2003,2003,2004:2006,2006,2006,2007,2007:2010,2010,2010)
# r_i_test = c(4,5,5,6,1:2,3,3,3,4,6:8,8,9,9)
# value_test = c(1.5:16.5)

# A = data.frame("year" = year_test,
#                "r_i" = r_i_test,
#                "value1" = value_test,
#                "value2" = value_test*2
# )

# # Compute cross-tabulated object
# xt <- xtabs(value1 ~ r_i + year, data = A)
# # Convert cross-tabulated object to data frame
# B1 <- as.data.frame.matrix(xt)

# B2 <- as.data.frame.matrix(xtabs(value1 ~ r_i + year, data = A))
# B2 <- replace(B, is.na(B), 0)

# C = data.frame("r1_i" = sort(unique(r_i)),
#                "bottom" = c(2:10))

# # Loop over columns of B and divide each value by corresponding value in C
# for (col in names(B2)[-1]) {
#   B2[, col] <- B2[, col] / C[match(rownames(B2), C$r_i), "bottom"]
# }



