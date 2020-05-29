
# hp ----------------------------------------------------------------------

sw <- function(n, alpha = 1, begin = 0, end = 1, direction = 1, option = 'Always', house = NULL) {

  if(!is.null(house)) option <- house

  #convert to lowercase
  option <- tolower(option)
  #remove spaces
  option <- gsub(" ", "", option, fixed = TRUE)
  #remove underscore
  option <- gsub("\\_", "", option, fixed = FALSE)

  #check to make sure start and end of palette between 0 and 1
  if (begin < 0 | begin > 1 | end < 0 | end > 1) {
    stop("begin and end must be in [0,1]")
  }

  #check to make sure direction is either 1 or -1
  if (abs(direction) != 1) {
    stop("direction must be 1 or -1")
  }

  #if the direction is reversed, switch begin and end values
  if (direction == -1) {
    tmp <- begin
    begin <- end
    end <- tmp
  }

  #set the column names of sw.map
  colnames(sw.map) <- c("R", "G", "B", "option")

  #filter for just the values of the option selected
  map <- sw.map[sw.map$option == option, ]

  #don't we already have these as hex codes?? why do we need the back and forth
  map_cols <- grDevices::rgb(map$R, map$G, map$B, maxColorValue = 255)

  #this is doing some sort of magic idk
  #somethin to do with colors
  #interpolating into a useable palette, perhaps?
  fn_cols <- grDevices::colorRamp(map_cols, space = "Lab", interpolate = "spline")
  cols <- fn_cols(seq(begin, end, length.out = n)) / 255
  grDevices::rgb(cols[, 1], cols[, 2], cols[, 3], alpha = alpha)
}



# hp_pal ------------------------------------------------------------------

#this creates a discrete color palette
sw_pal <- function(alpha = 1, begin = 0, end = 1, direction = 1, option = 'Always', house = NULL) {

  if(!is.null(house)) option <- house
  option <- tolower(option)
  option <- gsub(" ", "", option, fixed = TRUE)
  option <- gsub("\\_", "", option, fixed = FALSE)

  function(n) {
    hp(n, alpha, begin, end, direction, option)
  }
}



# scale_color_hp ----------------------------------------------------------

scale_color_sw <- function(option = 'anewhope', ..., alpha = 1, begin = 0, end = 1, direction = 1,
                           discrete = FALSE, house = NULL) {

  #if(!is.null(house)) option <- house
  option <- tolower(option)
  option <- gsub(" ", "", option, fixed = TRUE)
  option <- gsub("\\_", "", option, fixed = FALSE)

  if (discrete) {
    discrete_scale("colour", "hp", sw_pal(alpha, begin, end, direction, option), ...)
  } else {
    scale_color_gradientn(colours = sw(256, alpha, begin, end, direction, option), ...)
  }
}
