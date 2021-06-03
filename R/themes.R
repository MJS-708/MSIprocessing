#############################################
################# Themes ####################
#############################################

#' ggplot theme general
#' @author Koundinya Desiraju \url{https://rpubs.com/Koundy/71792}
#' @param base_size	Set the text size.
#' @param base_family Set the font type.
#' 
#' @import ggplot2 ggthemes
#' 
#' @examples
#' # Use on ggplot object
#' theme_Publication()
#' 
#' @export
theme_Publication <- function(base_size=14, base_family="helvetica") {
  (theme_foundation(base_size=base_size, base_family=base_family)
    + theme(plot.title = element_text(face = "bold",
                                      size = rel(1.2), hjust = 0.5),
            text = element_text(),
            panel.background = element_rect(colour = NA),
            plot.background = element_rect(colour = NA),
            panel.border = element_rect(colour = NA),
            axis.title = element_text(face = "bold",size = rel(1)),
            axis.title.y = element_text(angle=90,vjust =2),
            axis.title.x = element_text(vjust = -0.2),
            axis.text = element_text(),
            axis.line = element_line(colour="black"),
            axis.ticks = element_line(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.key = element_rect(colour = NA),
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.key.size= unit(0.2, "cm"),
            legend.margin = unit(0, "cm"),
            legend.title = element_text(face="italic"),
            plot.margin=unit(c(10,5,5,5),"mm"),
            strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
            strip.text = element_text(face="bold")
    ))
}

#' ggplot theme fill
#' @author Koundinya Desiraju \url{https://rpubs.com/Koundy/71792}
#' @param ...	Additional arguments.
#' 
#' @import ggplot2 scales
#' 
#' @examples
#' # Use on ggplot object
#' scale_fill_Publication()
#' 
#' @export
scale_fill_Publication <- function(...){
  discrete_scale("fill","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
}

#' ggplot theme colour
#' @author Koundinya Desiraju \url{https://rpubs.com/Koundy/71792}
#' @param ...	Additional arguments.
#' 
#' @import ggplot2 scales
#' 
#' @examples
#' # Use on ggplot object
#' scale_colour_Publication()
#' 
#' @export
scale_colour_Publication <- function(...){
  discrete_scale("colour","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
}
