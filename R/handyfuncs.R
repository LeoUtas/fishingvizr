# ---------- SOME HANDY FUNCTIONS FOR MY PHD WORK ----------- #


# ---------- FUNCTION 1 ----------- #
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


# ---------- FUNCTION 2 ----------- #
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


# ---------- FUNCTION 3 ----------- #
# to make effortdens_rt matrix
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
                               nrow = length(Tri_Area))
    )
    rownames(df3) = c(1:length(Tri_Area))
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


# ---------- FUNCTION 4 ----------- #
# to fit the model
fit_the_model = function(tmb_data, tmb_pars) {

  dyn.load("mesh_model_v5m_test_Qcoef_orgFt_Tweedie")

  rname = c("ln_u_gt", "Omegainput_g"
  )

  map = list(ln_p_par2 = factor(NA))

  obj <- MakeADFun(tmb_data, tmb_pars,
                   random = rname,
                   map = map,
                   DLL = "mesh_model_v5m_test_Qcoef_orgFt_Tweedie",
                   random.start = expression(last.par[random]),
                   inner.control = list(maxit = 10000,trace = F)
  )

  system.time(
    opt<-nlminb(obj$par,obj$fn,obj$gr,
                control = list(trace=1,iter.max=10000,eval.max=10000,sing.tol=1e-20))
  )

  for (i in c(1:20)) {

    if (opt$convergence != 0) {

      system.time(
        opt<-nlminb(opt$par,obj$fn,obj$gr,#lower=lower,upper=upper,
                    control = list(trace=1,iter.max=10000,eval.max=10000,sing.tol=1e-20))
      )

    }

  }

  opt_mes = opt$convergence
  opt_obj = opt$objective
  parList = obj$env$parList()

  result = list(opt_mes, opt_obj, tmb_pars$ln_p_par2, parList)

  return(result)

}


# ---------- FUNCTION 5 ----------- #
# create a list of polygons
make_polygons = function (data, n_loop) {

  n_loop = n_loop # n_loop = the number of triangles
  V_ls = data # data are TriList$V0, TriList$V2, TriList$V2

  mat_ls = list()
  poly_ls = list()
  for (i in 1:n_loop) {

    mat_ls[[i]] =
      as.matrix(
        rbind(
          V_ls[[1]][i,],
          V_ls[[2]][i,],
          V_ls[[3]][i,],
          V_ls[[1]][i,]
        )
      )

    poly_ls[[i]] <- st_polygon(list(mat_ls[[i]]))
  }
  crs <- st_crs("+proj=longlat +datum=WGS84")
  poly_sf <- st_sf(geometry = st_sfc(poly_ls), crs = crs)

  return(poly_sf)

}


# ---------- FUNCTION 6 ----------- #
make_tri_maps = function (data, n_loop, effort_data, n_breaks, selected_region,
                          legend_title = "Effort",
                          plot_title = "Title",
                          texture
                          ) {

  data = data # data are TriList$V0, TriList$V2, TriList$V2
  n_loop = n_loop # n_loop = the number of triangles

  poly_sf = make_polygons(data = data, n_loop = n_loop)

  effortdens_rt = effort_data # take in the effort data in a matrix form
  n_breaks = n_breaks # the number of breaks for legend scale
  selected_region = selected_region # use the FAO code
  legend_title = legend_title
  plot_title = plot_title
  texture = texture

  # arrange inputs for the loop creating triangle plots
  {
    limits = range(effortdens_rt)
    breaks = pretty(c(0,max(effortdens_rt)), n = n_breaks)
    poly_sf_ls <- list()
    filtered_poly_sf_ls = list()
    Tri_plot_ls = list()
    Output_ls = list()

    # run to select a mapping region
    selected_map_region = subset(shp_source, F_CODE %in% selected_region)
  }

  # Loop to create each poly_sf object, append it to the list, and generate triangle plots
  for (i in 1:ncol(effortdens_rt)) {
    # append to the list
    poly_sf_ls[[i]] <- poly_sf # e.g., poly_sf_ls[[1]] = poly_sf_ls in the year 2012
    # add the effort attribute to the sf object for year i
    poly_sf_ls[[i]]$effort = effortdens_rt[,i]
    # filter the sf object to use the data within the selected map only
    filtered_poly_sf_ls[[i]] = suppressWarnings(st_intersection(poly_sf_ls[[i]], selected_map_region))

    # plot the triangles of efforts
    Tri_plot_ls[[i]] = ggplot() +

      geom_sf(data = (filtered_poly_sf_ls[[i]]),
              alpha = .8,
              aes(fill=effort),
              color = "white") +

      scale_fill_gradientn(colors = texture,
                           limits = limits,
                           breaks = breaks) +

      labs(fill = paste(legend_title, "\n")) +

      ggtitle(paste(plot_title,
                    colnames(effortdens_rt)[i], sep = " ")) +

      theme(panel.grid = element_blank(),
            plot.background = element_rect(fill = "white"),
            panel.background = element_rect(fill = "white"),
            panel.border = element_rect(color = "darkgrey", fill = NA, linewidth = 1),
            #legend.position = c(0.2, 0.4)

            legend.direction = "horizontal",
            legend.position = "bottom",
            legend.box = "horizontal",
            legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
            legend.text.align = 0,
            legend.title.align = 0
      )

    # return a list of filtered_poly_sf_ls and Tri_plot_ls
    Output_ls[[i]] = list(filtered_poly_sf_ls[[i]], Tri_plot_ls[[i]])
  }

  return(Output_ls)

}


# --------------------- FUNCTION 7 - APPLY THORSON'S CODE FOR SURVEY DATA --------------------- #
make_tri_mesh = function (convex, cutoff, lok_center, data) {

  convex = convex
  cutoff = cutoff
  lok_center = lok_center
  data = data # take survey data as input

  loc = cbind(data$long, data$lat)
  loc_k <- stats::kmeans(loc, centers = lok_center)$centers
  MeshList <- MovementTools::Make_Movement_Mesh(loc_orig = loc_k, Cutoff = cutoff)
  TriList <- MovementTools::TriList_Fn(mesh = MeshList$mesh_domain)
  r_i <- MovementTools::Loc2Tri_Fn(locmat=loc, TriList=TriList)
  # add the r_i to the data df
  data$r_i = r_i

  output = list(data, TriList)

  return(output)

}


# --------------------- FUNCTION 8 - APPLY THORSON'S CODE FOR EFFORT DATA --------------------- #
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









# ------------------ TEST CODE FOR EFFORTDENS_RT ------------------ #
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



