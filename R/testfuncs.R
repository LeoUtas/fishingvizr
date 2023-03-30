




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
  data$r_i = r_i

  output = list(data, TriList)

  return(output)

}


make_effort_df = function (data) {

  data = data

  loc = cbind(data$long, data$lat)


}




# --------------------- APPLY THORSON'S CODE FOR EFFORT DATA --------------------#
effortdensloc_org = cbind(effort3LN_df$long, effort3LN_df$lat)
error_count = 0
tri_found = rep(NA,dim(effortdensloc_org)[1])

{ # WARNING: VERY LONG TIME RUN > 45 minutes, typically
  start_time = Sys.time()
  for (i in 1:dim(effortdensloc_org)[1]) {
    loc_i = effortdensloc_org[i,]
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

effortdensr_s = tri_found
effort3LN_df2 = effort3LN_df
effort3LN_df2$r_i = effortdensr_s


# remove the rows containing NA r_i
effort3LN_df2 = effort3LN_df2[complete.cases(effort3LN_df2),]
