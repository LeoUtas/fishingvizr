# --------------------- SOME HANDY FUNCTIONS --------------------- #


# --------------------- FUNCTION get_backto_WD() --------------------- #
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


check_path = function(path, folder_name) {

  path = path
  folder_name = folder_name

  # Check if the gear_type folder already exists
  if (!dir.exists(file.path(path, folder_name))) {
    # Create the 3D folder if it doesn't exist
    dir.create(file.path(path, folder_name))
    cat(crayon::cyan(paste("created ", folder_name, " folder ", " in ", path, "\n", sep = "")))
    
  } else {
    cat(crayon::cyan(paste(folder_name, " folder already exists ", " in ", path, "\n", sep = "")))
  }
}


# --------------------- FUNCTION handle_dimensions() --------------------- #
# handle width and height when using ggsave()
handle_dimensions = function(plot) {
  
  plot = plot
  g <- ggplot_build(plot = plot)
  x_range = abs(g$layout$panel_params[[1]]$x_range[2] - g$layout$panel_params[[1]]$x_range[1])
  y_range = abs(g$layout$panel_params[[1]]$y_range[2] - g$layout$panel_params[[1]]$y_range[1])
  ratio = y_range / x_range
  width = height * ratio
  
  return(ratio)
  
}

# --------------------- FUNCTION test_repetitive_locs() --------------------- #
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


# --------------------- FUNCTION make_effortdens_rt() --------------------- #
# to make effortdens_rt matrix
make_effortdens_rt = function(data, Tri_Area) {

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

  df5 = data.frame("r_i" = c(1:length(Tri_Area)),
                   "Tri_Area" = Tri_Area)

  for (col in names(df4)) {
    df4[, col] <- df4[, col] / df5[match(rownames(df4), df5$r_i), "Tri_Area"]
  }

  effortdens_rt = df4

  return(effortdens_rt)

}


# --------------------- FUNCTION fit_the_model() --------------------- #
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


# --------------------- FUNCTION make_sumdata() --------------------- #
make_sumdata = function (data) {

  # get the data ready to use
  {
    data = data %>% arrange(year, lat, long)
    sumdata = data %>% group_by(positionStr, year) %>%
      summarise(
        N = n(),
        effort = sum(effort),
        .groups = "drop" 
      )
    
    sumdata$lat <- sapply(strsplit(sumdata$positionStr, "-"), function(x) as.numeric(x[1]))
    sumdata$long <- sapply(strsplit(sumdata$positionStr, "-"), function(x) (as.numeric(x[2])*-1))
    sumdata = as.data.frame(sumdata %>% dplyr::select(-positionStr) %>% arrange(year, lat, long))
  }
  return(sumdata)

}


# --------------------- FUNCTION make_polygon() --------------------- #
# create a list of polygons
make_polygon = function (data, n_loop, crs = 4326) {

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
  crs = crs
  poly_sf <- st_sf(geometry = st_sfc(poly_ls), crs = crs)
  poly_sf$name = "triangle"

  return(poly_sf)

}


# --------------------- FUNCTION make_tri_mesh() - APPLY THORSON'S CODE --------------------- #
make_tri_mesh = function (convex, cutoff, lok_center, data) {

  convex = convex
  cutoff = cutoff
  lok_center = lok_center
  data = data # take survey data as input (recommended for research)

  loc = cbind(data$long, data$lat)

  # this chunk to force kmeans running until there is no warning
  test <- TRUE
  while(test == TRUE) {
    loc_k <- tryCatch(
      stats::kmeans(loc, centers = lok_center)$centers,
      warning = function(w) {
        # print("Warning is True")
        return(TRUE)
      }
    )
  
    if(is.logical(loc_k)) {
      test <- loc_k
    } else {
      test <- FALSE
    }
  }
  
  MeshList <- MovementTools::Make_Movement_Mesh(loc_orig = loc_k, Cutoff = cutoff)
  TriList <- MovementTools::TriList_Fn(mesh = MeshList$mesh_domain)
  n_r = length(TriList$Tri_Area)
  r_i <- MovementTools::Loc2Tri_Fn(locmat=loc, TriList=TriList)
  # add the r_i to the data df
  data$r_i = r_i

  output = list(data, TriList)

  return(output)

}


# --------------------- FUNCTION make_effort_r_i() - APPLY THORSON'S CODE --------------------- #
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


# ---------- FUNCTION make_tri_map() ----------- #
make_tri_map = function (data, n_loop, effort_data, n_breaks, selected_region, gear_type,
                         legend_title = "Effort",
                         plot_title = "Title",
                         texture,
                         pooled_scale = FALSE
                         ) {

  data = data # data are TriList$V0, TriList$V2, TriList$V2
  n_loop = n_loop # n_loop = the number of triangles

  # create polygons to use in plotting
  poly_sf = make_polygon(data = data, n_loop = n_loop)  

  effortdens_rt = effort_data # take in the effort data in a matrix form
  n_breaks = n_breaks # the number of breaks for legend scale
  selected_region = selected_region # use the FAO code
  legend_title = legend_title
  plot_title = plot_title
  texture = texture

  # arrange inputs for the loop creating triangle plots
  {
    poly_sf_ls <- list()
    filtered_poly_sf_ls = list()
    Tri_plot_ls = list()
    Output_ls = list()
    pooled_scale = pooled_scale

    # run to select a mapping region
    selected_map_region = subset(shp_source, F_CODE %in% selected_region)
    selected_map_region = selected_map_region %>% dplyr::select(-c(F_SUBDIVIS,F_SUBUNIT))
  }

  # Loop to create each poly_sf object, append it to the list, and generate triangle plots
  for (i in 1:ncol(effortdens_rt)) {
    # append to the list
    poly_sf_ls[[i]] <- poly_sf # e.g., poly_sf_ls[[1]] = poly_sf_ls in the year 2012
    # add the effort attribute to the sf object for year i
    poly_sf_ls[[i]]$effort = effortdens_rt[,i]
    # add the year attribute to the sf object for year i
    poly_sf_ls[[i]]$year = rep(colnames(effortdens_rt)[i], length(effortdens_rt[,i])) 
    st_make_valid(poly_sf_ls[[1]])
    # filter the sf object to use the data within the selected map only
    filtered_poly_sf_ls[[i]] = suppressWarnings(st_intersection(poly_sf_ls[[i]], selected_map_region))

    # prepare for the choice of legend scale
    limits1 = range(effortdens_rt)
    breaks1 = pretty(c(0,max(effortdens_rt)), n = n_breaks)

    limits2 = range(effortdens_rt[,i])
    breaks2 = pretty(c(0,max(effortdens_rt[,i])), n = n_breaks)

    if (pooled_scale == TRUE) {
      limits = limits1
      breaks = breaks1
    } else {
       limits = limits2
       breaks = breaks2
    }

    # plot the triangles of efforts
    Tri_plot_ls[[i]] = ggplot() +

      geom_sf(data = (filtered_poly_sf_ls[[i]]),
              alpha = .8,
              aes(fill=effort),
              color = "transparent") +

      scale_fill_gradientn(colors = texture,
                           limits = limits,
                           breaks = breaks) +

      labs(fill = paste(legend_title, "\n")) +

      ggtitle(paste(plot_title, " ",
                    colnames(effortdens_rt)[i],
                    " | Gear type: ", gear_type,
                     sep = " ")) +

      theme(panel.grid = element_blank(),
            plot.background = element_rect(fill = "white"),
            panel.background = element_rect(fill = "white"),
            panel.border = element_rect(color = "darkgrey", fill = NA, linewidth = 1),
            #legend.position = c(0.2, 0.4)

            legend.direction = "vertical",
            legend.position = "right",
            legend.box = "horizontal",
            legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
            legend.text.align = 0,
            legend.title.align = 0
      )

    # return a list of filtered_poly_sf_ls and Tri_plot_ls
    Output_ls[[i]] = list(filtered_poly_sf_ls[[i]], Tri_plot_ls[[i]])
  }

  return(Tri_plot_ls)

}


# --------------------- FUNCTION make_3dplot() APPLY rayshader package --------------------- #
render_3dplot = function (range, Tri_plot_ls2, 
                          lightcolor, 
                          lightdirection = 315,
                          lightaltitude = c(20, 80),
                          lightintensity = c(600, 100),
                          samples = 550,
                          width = 6000, height = 6000,
                          path = "viz/3D/", folder_name,
                          gear_type
                          ) {

  range = range
  Tri_plot_ls2 = Tri_plot_ls2
  lightcolor = lightcolor
  lightdirection = lightdirection
  lightaltitude = lightaltitude
  lightintensity = lightintensity
  samples = samples
  width = width
  height = height
  folder_name = gear_type

  path = path
  folder_name = folder_name  
  
  check_path(path = "viz", folder_name = "3D")
  check_path(path = path, folder_name = folder_name)

  # Create plot2 for plot_gg()
  plot2 = list()
  for (i in 1:length(range)) {
    plot2[[i]] = Tri_plot_ls2[[i]] +
      
      labs(title = NULL) +
      theme(
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "none",
        panel.border = element_blank()
      )
    
    # Create a 3D plot
    rgl::close3d()
    plot_gg(
      plot2[[i]],
      solid = FALSE,
      col = "transparent",
      #sunangle = 315,
      background = color[1],
      multicore = TRUE,
      theta = -45,
      #zoom = 0.55, 
      phi = 35
      
    )
    render_snapshot()

  # Render the high quality images
    outfile <- paste(path, folder_name,"/3Dplot_", gear_type, "_", range[i],  "_.png", sep = "")

    {
      start_time <- Sys.time()
      cat(crayon::cyan(start_time), "\n")
      if (!file.exists(outfile)) {
        png::writePNG(matrix(1), target = outfile)
      }
      
      render_highquality(
        filename = outfile,
        
        interactive = FALSE,
        lightdirection = 315, #280 220
        lightaltitude = lightaltitude,
        lightcolor = c(lightcolor[2], "white"),
        lightintensity = lightintensity,
        samples = samples,
        width = width,
        height = height
      )
      end_time <- Sys.time()
      diff <- end_time - start_time
      cat(crayon::cyan(diff), "\n")
    }

 }
 rgl::close3d()

 cat(crayon::cyan(paste("3D plots were successfully rendered and saved to 3D folder", sep = "")))

}









# # ---------- FUNCTION make_landmark() ----------- #
# make_landmark_polygon = function (coords, name = NULL, crs = 4326, polygon = FALSE) {

#   lat = coords[1]
#   long = coords[2]
#   crs = crs
#   name = name

#   if (polygon == FALSE) {

#     point1_sf = st_point(x = coords)
#     point1_sf = st_sf(geometry = st_sfc(point1_sf), crs = crs)
#     point1_sf$name = stringr::str_to_title(name)

#   } else {

#   # Create a data frame with A1
#   point0 <- data.frame(lat = lat, long = long)
#   # Convert the data frame to an sf point object with WGS84 CRS
#   point0_sf <- st_as_sf(point0, coords = c("long", "lat"), crs = crs)
#   # Create a buffer of 500 meters (1 km diameter) around point0
#   point1_sf <- st_buffer(point0_sf, dist = 500)
#   # Extract the coordinates of the polygon vertices
#   point1_coords <- st_coordinates(point1_sf)
#   # Create a data frame with the polygon vertices
#   point1_df <- data.frame(long = point1_coords[,1], lat = point1_coords[,2])
#   # Add the first vertex at the end to close the polygon
#   point1_df <- rbind(point1_df, point1_df[1,])
#   # Create an sf polygon object with name attribute
#   point1_sf <- st_polygon(list(as.matrix(point1_df)))
#   # Create a sf object
#   point1_sf = st_sf(geometry = st_sfc(point1_sf), crs = crs)
#   # add a name attribute to the sf object
#   point1_sf$name = stringr::str_to_title(name)

#   }

#   return(point1_sf)
# }


# # --------------------- FUNCTION make_2dplot() --------------------- #
# render_2dplot = function (landmark = NULL,
#                           landmark_text_size = 9,
#                           text_color,
#                           width, height, 
#                           range,
#                           Tri_plot_ls) {

#   # Create plot1 as side plot of the visualization
#   {
#     plot1 = list()
#     plot1.1 = list()
#     legend = list()
#     text_color = text_color
#     width = width 
#     height = height
#     range = range
#   }

#   # Check if the 2D folder already exists
#   if (!dir.exists(file.path("viz", "2D"))) {
#     # Create the 2D folder if it doesn't exist
#     dir.create(file.path("viz", "2D"))
#     cat(crayon::cyan(paste("created 2D folder \n", sep = "")))
    
#   } else {
#     cat(crayon::cyan(paste("2D folder already exists \n", sep = "")))
#   }

#   {
#     Tri_plot_ls = Tri_plot_ls
#     landmark_text_size = landmark_text_size
#     landmark = landmark      
#   }  

#   for (i in 1:length(range)) {
    
#     plot1[[i]] = Tri_plot_ls[[i]] + 
      
#       geom_point(data = landmark, aes(x = lat,
#                                       y = long),
#                 size = 1.5,
#                 color = text_color
#                 ) +
      
#       geom_text(data = landmark, aes(x = lat, 
#                                     y = long, 
#                                     label = name),
#                 size = landmark_text_size,
#                 color = text_color,
#                 nudge_x = 0.01, 
#                 nudge_y = .28) +
      
#       labs(title = NULL)
    
#     ggsave(paste("viz/2D/", folder_name, "/2Dplot_", gear_type, "_", range[i], "_.png", sep = ""), 
#           plot = plot1[[i]], 
#           width = width, 
#           height = height, 
#           dpi = 300,
#           bg = "white")
#   }

#   return(plot1)

# }


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



