




# --------------------- APPLY THORSON'S CODE FOR SURVEY DATA --------------------- #
make_tri_mesh = function (convex, cutoff, lok_center, data) {

  convex = convex
  cutoff = cutoff
  lok_center = lok_center
  data = data # take survey data as input

  loc = cbind(data$long, data$lat)
  loc_k <- stats::kmeans(surveyloc_org, centers = lok_center)$centers
  MeshList <- MovementTools::Make_Movement_Mesh(loc_orig = loc_k, Cutoff = cutoff)
  TriList <- MovementTools::TriList_Fn(mesh = MeshList$mesh_domain)
  r_i <- MovementTools::Loc2Tri_Fn(locmat=loc, TriList=TriList)
  # add the r_i to the data df
  data$r_i = r_i

  output = list(data, TriList)

  return(output)

}


make_effort_r_i = function (data, TriList) {

  data = data
  TriList = TriList

  loc = cbind(data$long, data$lat)
  error_count = 0
  tri_found = rep(NA,dim(loc)[1])

  { # WARNING: VERY LONG TIME RUN
    start_time = Sys.time()
    for (i in 1:dim(loc)[1]) {
      loc_i = loc[i,]
      loc_i = matrix(loc_i, nrow = 1, ncol = 2)
      error_r = try((loc_found_i = MovementTools::Loc2Tri_Fn(locmat=loc_i, TriList=TriList)),
                    silent=TRUE)

      if ('try-error' %in% class(error_r)) {
        error_count = error_count + 1
        next
      }
      tri_found[i] = loc_found_i
    }
    end_time = Sys.time()
  }

  r_i = tri_found
  # add the r_i to the data df
  data$r_i = r_i
  # remove the rows containing NA r_i
  updated_data = data[complete.cases(data),]
  output = list(updated_data, error_count)

  return(output)

}


