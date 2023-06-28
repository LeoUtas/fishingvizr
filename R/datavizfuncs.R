

# ---------- LOAD REQUIRED LIBRARIES ----------- #
#' @importFrom sf read_sf st_coordinates
#' @importFrom ggplot2 ggplot geom_sf


# ------------ make_map_background() ----------- #
#' Creates a map background
#'#'
#' @param selected_map_region is required to provide selected regions for the map background.
#' The current version accepts any of region codes in the FAO Major Fishing Area 21
#' (i.e., the Northwest Atlantic).
#' The region codes include c("0A", "0B", "1A", "1B", "1C", "1D", "1E", "1F", "2G", "2H",
#' "2J", "3K", "3L", "3M", "3N", "3O", "3Pn", "3Ps", "4R", "4S", "4T", "4Vn", "4Vs", "4W",
#' "4X", "5Y", "5Ze", "5Zw", "6A", "6B", "6C", "6D", "6E", "6F", "6G", "6H."
#'
#' @param geom_sf_color is assigned to be "grey66" by default, and alterable
#' @param geom_sf_fill is assigned to be "#009EFF" by default, and alterable
#' @param label_size is assigned to be 5 by default, and alterable
#' @param alpha is assigned to be 0.2 by default, and alterable
#' @param axis_text_size is assigned to be 12 by default, and alterable
#' @param axis_color is assigned to be ""grey" by default, and alterable
#' @param axis_line_size is assigned to be 0.5 by default, and alterable
#' @param title_text_size is assigned to be 16 by default, and alterable
#' @param panel_color is assigned to be "grey" by default, and alterable
#' @param panel_fill is assigned to be "white" by default, and alterable
#' @param legend_title_size is assigned to be 12 by default, and alterable
#' @param legend_text_size is assigned to be 12 by default, and alterable
#' @param ...
#'
#' @return Returns a map background object
#' # remove export
#'
#' @examples
#' make_map_background(selected_map_region = c("3K", "3L", "3N", "3M", "3O", "3Ps", "3Pn"))
#'
#'
make_map_background = function(selected_map_region,
                               geom_sf_color = "grey66",
                               geom_sf_fill = "#009EFF",
                               label_size = 5,
                               alpha = .2,
                               axis_text_size = 12,
                               axis_color = "grey",
                               axis_line_size = .5,
                               title_text_size = 16,
                               panel_color = "grey",
                               panel_fill = "white",
                               legend_title_size = 12,
                               legend_text_size = 12,
                               ...){

  args = list(...)

  region = make_selected_region(selected_map_region)

  { # make a map background
    bubmap_temp <-
      ggplot() +
      geom_sf(data = region$geometry, color = geom_sf_color, fill = geom_sf_fill, alpha = alpha) +

      theme(axis.text = element_text(size = axis_text_size),
                     axis.title = element_blank(),
                     plot.title = element_text(size = title_text_size, face = "bold", hjust = 0),
                     axis.line = element_line(size = axis_line_size, color = axis_color),
                     panel.grid.major = element_blank(),
                     panel.background = element_rect(fill = panel_fill, color = panel_color, size = axis_line_size),
                     legend.title  = element_text(size = legend_title_size),
                     legend.text = element_text(size = legend_text_size),
                     legend.position = "right",
                     legend.background = element_rect(fill = "transparent")
      )
  }

  return(bubmap_temp)
}


# ------------ make_bubmap() ----------- #
#' This is a main function to create a list of bubble maps for fishing effort
#' data requested from the Global Fishing Watch API
#'
#' @param data is required to plug in the output of the function get_rasters()
#'
#' @param selected_map_region is required to provide selected regions for the map background.
#'
#' The current version accepts any of region codes in the FAO Major Fishing Area 21
#'
#' (i.e., the Northwest Atlantic). The region codes include c("0A", "0B", "1A", "1B", "1C",
#'
#' "1D", "1E", "1F", "2G", "2H", "2J", "3K", "3L", "3M", "3N", "3O", "3Pn", "3Ps", "4R",
#'
#' "4S", "4T", "4Vn", "4Vs", "4W", "4X", "5Y", "5Ze", "5Zw", "6A", "6B", "6C", "6D", "6E",
#'
#' "6F", "6G", "6H."
#'
#' @param begin is required to provide the beginning year
#' @param end is required to provide the end year
#' @param alpha is assigned to be 0.3 by default, and alterable
#' @param over_zero_color is assigned to be "#002E94" by default, and alterable,
#' presenting data points of effort > 0
#' @param over_zero_fill is assigned to be "#002E94" by default, and alterable,
#' presenting data points of effort > 0
#' @param zero_color is assigned to be "#1A1A40" by default, and alterable,
#' presenting data points of effort = 0
#' @param zero_fill is assigned to be "#1A1A40" by default, and alterable,
#' presenting data points of effort = 0
#' @param legend_title presents the legend title, taking string inputs
#' @param plot_tilte is assigned to be "Fishing effort map" by default,
#' and alterable, taking string inputs
#' @param zeronote is assigned to be "Weight = 0" by default, and alterable,
#' presenting the legend title for data points of effort = 0
#'
#' @return
#' Returns a list of bubble maps presenting fishing effort for different years
#' @export
#'
#' @examples
#' effort_map = make_bubmap(data = effort_ls,
#'                          selected_map_region = c("3K", "3L", "3N", "3M", "3O", "3Ps", "3Pn"),
#'                          begin = begin, end = end,
#'                          legend_title = "Fishing effort\n (hour)",
#'                          zeronote = "")
#'
#'
make_bubmap = function(data, selected_map_region, begin, end,
                       alpha = .6,
                       over_zero_color = "#002E94",
                       over_zero_fill = "#002E94",
                       zero_color = "#1A1A40",
                       zero_fill = "#1A1A40",
                       legend_title = "effort",
                       plot_tilte = "Fishing effort map",
                       zeronote = "N = 0") {

  bubmap_temp = make_map_background(selected_map_region = selected_map_region)

  bubmap = list()
  year = c(begin:end)

  begin_i = begin/year[1]
  end_i = year[length(year)] - year[1] + 1

  for (i in begin_i:end_i) {

    bubmap[[i]] <- bubmap_temp +
      geom_point(data = data[[i]][data[[i]]$effort > 0,],
                 aes(x = long, y = lat, size = effort),
                 alpha = alpha, color = over_zero_color, fill = over_zero_fill) +

      geom_point(data = data[[i]][data[[i]]$effort == 0,],
                 aes(x = long, y = lat, shape = ifelse(.data$effort == 0, !!zeronote, "over_zero")) ,
                 color = zero_color, fill = zero_fill) +
      scale_shape_manual(name="", values=setNames(c(4, 21), c(zeronote, "over_zero")), limits=c(zeronote)) +

      scale_size_continuous(paste(legend_title)) +

      labs(title = paste(year[i], "-", plot_tilte, sep = " "))

  }
  return(bubmap)

}


# ------------ print_map() ----------- #
#' Creates a series of .jpg files for the list of fishing effort maps
#'
#' @param effort_map takes the output of the function make_bubmap()
#'
#' @return
#' Generates a series of .jpg files for the list of fishing effort maps,
#' and save the files in a viz folder
#' @export
#'
#' @examples
#' print_effort_map(effort_map = effort_map)
#'
print_map = function(map_input, title) {

  title = title

  for (i in 1:length(effort_map)) {
    ggsave(filename = paste(title, "_map_", i, ".jpg", sep = ""),
           plot = effort_map[[i]],
           height = 22, width = 22, units = "cm",
           path = "viz/" )
  }
}
