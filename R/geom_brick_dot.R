#' Tile charts as bricks
#'
#' `geom_circle`, except cicles look like LEGO 1x1 tiles
#'
#' Currently all 'ggplot2' extensions are for internal use only.
#'
#' @inheritParams ggplot2::geom_rect
#' @param label Character string to include as embossed text inside brick knobs. Maximum 6 characters.
#' @param label_scale Scale text size of label as a percentage.
#' @param simplified_threshold Maximum number of knobs on the plot before embossed label is suppressed.
#' @family Graphs
#' @keywords internal
#'
geom_brick_dots <- function(mapping = NULL, data = NULL,
                            stat = "identity", position = "identity",
                            ...,
                            label = "", simplified_threshold = 24*24, label_scale = 1,
                            linejoin = "mitre",
                            na.rm = FALSE,
                            show.legend = NA,
                            inherit.aes = TRUE) {
  layer_brick_dots <- ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomBrickDots,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      linejoin = linejoin,
      label = label,
      label_scale = label_scale,
      na.rm = na.rm,
      simplified_threshold = simplified_threshold,
      ...
    )
  )

  return(layer_brick_dots)
}

#' GeomBrick
#'
#' ggproto for brickr geoms
#'
#' @rdname brickr-dots-ggproto
#' @format NULL
#' @usage NULL
GeomBrickDots <- ggplot2::ggproto("GeomBrickDots", ggplot2::Geom,
                              default_aes = ggplot2::aes(colour = "#333333", fill = "#B40000", size = 0.5, linetype = 1,
                                                         alpha = NA, label = "brickr", label_scale = 1,
                                                         angle = 0, family = "", fontface = 1, lineheight = 1.2),

                              required_aes = c("x", "y"),

                              setup_data = function(data, params) {
                                #This data manipulation happens BEFORE splitting the data into the PANEL
                                return(data)
                              },

                              draw_panel = function(self, data, panel_params, coord, linejoin = "mitre",
                                                    simplified_threshold = 24*24, label_scale = 1,
                                                    use_bricks = use_bricks) {

                                #This happens to EACH panel
                                if (!coord$is_linear()) {
                                  stop("geom_brick_rect must be used with linear coordinates")
                                } else {

                                  #Brick border ----

                                  points_to_rects <- function(data){
                                    #Probably super frowned upon to use dplyr inside of a ggproto, but this simplifies the data that can be fed into geom
                                    data$Level <- as.numeric(data$PANEL)

                                    if(is.null(data$fill)){
                                      data$Lego_name <- "#B40000"
                                      data$Lego_color <- "#B40000"
                                    }else{
                                      data$Lego_name <- data$fill
                                      data$Lego_color <- data$fill
                                    }

                                    dat <- data$Img_lego
                                    dat <- transform(dat,
                                                     PANEL = Level, Level = NULL,
                                                     fill = Lego_color)

                                    # print(dat)
                                    return(dat)
                                  }

                                  # coords_rect <- coord$transform(data %>% points_to_rects, panel_params) %>%
                                  #   dplyr::mutate(size = data$size[1], linetype = data$linetype[1],
                                  #                 colour = data$colour[1], alpha = data$alpha[1])
                                  #
                                  # gm_brick <- grid::rectGrob(
                                  #   coords_rect$xmin, coords_rect$ymax,
                                  #   width = coords_rect$xmax - coords_rect$xmin,
                                  #   height = coords_rect$ymax - coords_rect$ymin,
                                  #   default.units = "native",
                                  #   just = c("left", "top"),
                                  #   gp = grid::gpar(
                                  #     col = ggplot2::alpha(coords_rect$colour, 0.2),
                                  #     fill = ggplot2::alpha(coords_rect$fill, coords_rect$alpha),
                                  #     lwd = coords_rect$size * ggplot2::.pt,
                                  #     lty = coords_rect$linetype,
                                  #     linejoin = linejoin,
                                  #     # `lineend` is a workaround for Windows and intentionally kept unexposed
                                  #     # as an argument. (c.f. https://github.com/tidyverse/ggplot2/issues/3037#issuecomment-457504667)
                                  #     lineend = if (identical(linejoin, "round")) "round" else "square"
                                  #   )
                                  # )

                                  # Knob ----

                                  coords <- coord$transform(data, panel_params)

                                  # test_coord <<- coords

                                  x_size <- stats::median(abs(diff(coords$x))[abs(diff(coords$x))>0], na.rm=TRUE)
                                  y_size <- stats::median(abs(diff(coords$y))[abs(diff(coords$y))>0], na.rm=TRUE)

                                  diameter <- max(x_size, y_size, na.rm=TRUE)

                                  #Nudge the shadow down and right by a 1/4 knob radius
                                  coords_nudge <- ggplot2::transform_position(coords,
                                                                              function(x) x + x_size*(29/32)*(1/2)*(1/4),
                                                                              function(y) y - y_size*(29/32)*(1/2)*(1/4))

                                  #Outline and text for dark colors
                                  color_lum <- as.data.frame(t(col2rgb(coords$fill)/255))
                                  coords$color_intensity <- 0.299*color_lum$red + 0.587*color_lum$green + 0.114*color_lum$blue

                                  coords$text_alpha <- ifelse(coords$color_intensity <= brickr:::thres_brick_lum(), 0.3, 0.3)
                                  coords$text_col <- ifelse(coords$color_intensity <= brickr:::thres_brick_lum(), "#CCCCCC", "#333333")

                                  coords$dot_sides_col <- ifelse(coords$color_intensity <= brickr:::thres_brick_lum(),
                                                                 colorspace::lighten(coords$fill, 0.5),
                                                                 colorspace::darken(coords$fill, 0.5))

                                  coords$dot_sides_fill <- ifelse(coords$color_intensity <= brickr:::thres_brick_lum(),
                                                                 colorspace::lighten(coords$fill, 0.3),
                                                                 colorspace::darken(coords$fill, 0.3))

                                  gm_knob_shadow <- grid::circleGrob(
                                    coords_nudge$x,
                                    coords_nudge$y,
                                    r= diameter*(29/32)*(1/2),
                                    default.units = "native",
                                    gp = grid::gpar(
                                      col = ggplot2::alpha(coords$text_col, coords$text_alpha),
                                      fill = ggplot2::alpha(coords$dot_sides_fill, coords$alpha), #ggplot2::alpha("#333333", 0.2),
                                      size = coords$size * ggplot2::.pt,
                                      lty = coords$linetype
                                    )
                                  )

                                  gm_knob_base <- grid::circleGrob(
                                    coords$x, coords$y,
                                    r= diameter*(29/32)*(1/2),
                                    default.units = "native",
                                    gp = grid::gpar(
                                      # col = ggplot2::alpha("#333333", 0.2),
                                      col = ggplot2::alpha(coords$text_col, coords$text_alpha),
                                      fill = ggplot2::alpha(coords$fill, coords$alpha),
                                      size = coords$size * ggplot2::.pt,
                                      lty = coords$linetype
                                    )
                                  )

                                  # #Text ----
                                  # #Don't draw if mosaic is larger than threshold size
                                  # n <- nrow(data)
                                  # if (n > simplified_threshold | data$label[1] == "") {
                                  #   gm_knob_text <- grid::nullGrob()
                                  # } else {
                                  #   lab <- data$label
                                  #   if(any(nchar(lab) > 6)){
                                  #     warning("aes `label` is too long and will be truncated. Please limit to 6 characters or less.")
                                  #     lab <- substr(lab, 1, 6)
                                  #   }
                                  #
                                  #   label_num <- nchar(lab)[1]
                                  #
                                  #   #Get view port size for initial text drawing...
                                  #   vp_width = grid::convertWidth(ggplot2::unit(1, "snpc"), "mm", valueOnly=TRUE)
                                  #   fs <- scales::rescale(vp_width, to=c(20, 7), from=c(120, 20))
                                  #
                                  #   gm_knob_text <- grid::textGrob(
                                  #     lab,
                                  #     coords$x, coords$y,
                                  #     default.units = "native",
                                  #     hjust = data$hjust, vjust = data$vjust,
                                  #     rot = data$angle,
                                  #     gp = grid::gpar(
                                  #       col = ggplot2::alpha(coords$text_col, coords$text_alpha),
                                  #       fontsize = fs,
                                  #       cex = label_scale * (3/8) * 0.5 * (1.5) * ((100/n)^(1/2)), #100 bricks is optimal size for labels by default?
                                  #       fontfamily = data$family,
                                  #       fontface = "bold",
                                  #       lineheight = data$lineheight
                                  #     )
                                  #   )
                                  # }

                                  # Combine ----
                                  ggplot2:::ggname("geom_brick_dots",
                                                   grid::grobTree(#gm_brick,
                                                                  gm_knob_shadow, gm_knob_base
                                                                  #, gm_knob_text
                                                   ))
                                }
                              },

                              draw_key = ggplot2::draw_key_polygon
)
