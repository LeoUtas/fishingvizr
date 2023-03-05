

make_effortdens_rt = function(data, TriList = Trilist) {

  data = data
  # to construct a matrix of sum for year and triangle
  {
    mat = aggregate(amount ~ year + r_i, data = data, FUN = sum)
    matrix1 = matrix(mat$amount, nrow = length(unique(data$r_i)), ncol = length(unique(data$year)))
    rownames(matrix1) = sort(unique(data$r_i))
    colnames(matrix1) = sort(unique(data$year))
  }

  Trilist = Trilist
  {
    matrix2 = matrix(TriList$Tri_Area, nrow = length(TriList$Tri_Area), ncol = 1)
    rownames(matrix2) = c(1:length(TriList$Tri_Area))
    colnames(matrix2) = 1
  }

  # to divide the sum of effort in each triangle by the triangle area
  {
    matrix3 = matrix(nrow = nrow(matrix1), ncol = ncol(matrix1))
    for (i in 1:ncol(matrix1)) {
      row_indices = match(rownames(matrix1), rownames(matrix2))
      matrix3[,i] = matrix1[,i] / matrix2[row_indices,1]
    }
    rownames(matrix3) = sort(unique(data$r_i))
    colnames(matrix3) = sort(unique(data$year))
  }

  # to make a matrix of fishing effort in the triangles
  {
    matrix4 = matrix(ncol = length(unique(data$year)),
                     nrow = length(unique(TriList$Tri_Area))
    )
    rownames(matrix4) = c(1:length(unique(TriList$Tri_Area)))
    colnames(matrix4) = sort(unique(data$year))
  }

  for (i in 1:nrow(matrix4)) {
    row_name <- rownames(matrix4)[i]
    if (row_name %in% rownames(matrix3)) {
      row_index <- which(rownames(matrix3) == row_name)
      matrix4[i, ] <- matrix3[row_index, ]
    } else {
      matrix4[i, ] <- 0
    }
  }
  effortdens_rt = matrix4

  return(effortdens_rt)

}



