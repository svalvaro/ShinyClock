library(shiny)
library(tidyverse)


# Define UI for application that draws a histogram
ui <- fluidPage(

           plotOutput("clock")

)

# Define server logic required to draw a histogram
server <- function(input, output) {

    # create function geom_segment_straight

    geom_segment_straight <- function(...) {
        layer <- geom_segment(...)
        new_layer <- ggproto(NULL, layer)
        old_geom <- new_layer$geom
        geom <- ggproto(
            NULL, old_geom,
            draw_panel = function(data, panel_params, coord,
                                  arrow = NULL, arrow.fill = NULL,
                                  lineend = "butt", linejoin = "round",
                                  na.rm = FALSE) {
                data <- ggplot2:::remove_missing(
                    data, na.rm = na.rm, c("x", "y", "xend", "yend",
                                           "linetype", "size", "shape")
                )
                if (ggplot2:::empty(data)) {
                    return(zeroGrob())
                }
                coords <- coord$transform(data, panel_params)
                # xend and yend need to be transformed separately, as coord doesn't understand
                ends <- transform(data, x = xend, y = yend)
                ends <- coord$transform(ends, panel_params)

                arrow.fill <- if (!is.null(arrow.fill)) arrow.fill else coords$colour
                return(grid::segmentsGrob(
                    coords$x, coords$y, ends$x, ends$y,
                    default.units = "native", gp = grid::gpar(
                        col = alpha(coords$colour, coords$alpha),
                        fill = alpha(arrow.fill, coords$alpha),
                        lwd = coords$size * .pt,
                        lty = coords$linetype,
                        lineend = lineend,
                        linejoin = linejoin
                    ),
                    arrow = arrow
                ))

            }
        )
        new_layer$geom <- geom
        return(new_layer)
    }






     autoInvalidate <- reactiveTimer(intervalMs = 1000) # 1s


    output$clock <- renderPlot({

        autoInvalidate()

        seconds_current <- as.numeric(format(Sys.time(), "%S"))

        mins_current <- as.numeric(format(Sys.time(), "%M"))

        hours_current <- as.numeric(format(Sys.time(), "%H"))

        if (hours_current>12) {

            hours_current <- hours_current-12
        }


        hours_current <- (hours_current*60)/12
        hours_current <- hours_current + (mins_current/60)*5




        minutes <- data_frame(x = 0:60, y = 1)


        hours <- filter(minutes, x %% 5 == 0)


        timer <- data_frame( x = 0, y = 2)


        p <- ggplot() +
            geom_point(data = minutes, aes(x = x, y = y)) +
            geom_point(data = hours, aes(x = x, y = y,
                                         color = factor(x, levels = c(60, seq(5, 55, 5)))),
                       size = 5, show.legend = FALSE, shape = 18) +
            coord_polar() +
            expand_limits(y = c(0, 1)) +
            scale_x_continuous(breaks = seq(15, 60, 15), labels = c(3, 6, 9, 12)) +
            scale_y_continuous(breaks = seq(0, 1, 0.25)) +
            scale_color_discrete() +
            theme_grey() +
            theme(
                axis.title = element_blank(),
                axis.ticks = element_blank(),
                axis.text.x = element_text(size = 20),
                axis.text.y = element_blank(),
                panel.grid.major = element_line(size = 2),
                panel.grid.minor = element_line(size = 2)
            )+ #seconds
            geom_segment_straight(
                aes(
                    x = 0,
                    y = 0,
                    xend = seconds_current,
                    yend = 0.9
                ),
                arrow = arrow(length = unit(0.2, "cm")),
                col = "red",
                size = 1
            )+
            #mins
            geom_segment_straight(
                aes(
                    x = 0,
                    y = 0,
                    xend = mins_current,
                    yend = 0.8
                ),
                arrow = arrow(length = unit(0.2, "cm")),
                col = "blue",
                size = 2
            )+
            # hours

            geom_segment_straight(
                aes(
                    x = 0,
                    y = 0,
                    xend = hours_current,
                    yend = 0.6
                ),
                arrow = arrow(length = unit(0.2, "cm")),
                col = "black",
                size = 2.5
            )



        return(p)
   })
}

# Run the application
shinyApp(ui = ui, server = server)
