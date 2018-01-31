ICPlot <- function(IC, Lag, Caption, Relative, Bins) {
  library(ggplot2)
  
  IC.Lim <- c(max(abs(IC[, Lag + 1]), na.rm = T), max(abs(IC[, Lag + 2]), na.rm = T))
  
  IC.Plot <- ggplot(IC, aes_string(paste0("Lag", Lag), paste0("Lag", Lag + 1))) +
    geom_point(color = "black") +
    geom_smooth(color = "red", method = "lm", formula = y ~ x - 1, se = F) +
    geom_vline(xintercept = 0, linetype = 2) +
    geom_hline(yintercept = 0, linetype = 2) +
    geom_text(aes(IC.Lim[1] * 4 / 5, IC.Lim[2] * 9 / 10, label = Caption),
              size = rel(Relative)) +
    xlim(-IC.Lim[1], IC.Lim[1]) +
    ylim(-IC.Lim[2], IC.Lim[2]) +
    theme_bw() +
    theme(panel.grid = element_blank())
  
  Hist.Top <- ggplot(IC, aes_string(paste0("Lag", Lag))) +
    geom_histogram(color = "black", fill = "gray", bins = Bins) +
    xlim(-IC.Lim[1], IC.Lim[1]) +
    theme_bw() +
    theme(panel.grid = element_blank()) +
    theme(panel.border = element_blank()) +
    theme(axis.title.y = element_text(color = "white")) +
    theme(axis.title.x = element_blank()) +
    theme(axis.text = element_blank()) +
    theme(axis.ticks = element_blank())
  
  Hist.Right <- ggplot(IC, aes_string(paste0("Lag", Lag + 1))) +
    geom_histogram(color = "black", fill = "gray", bins = Bins) +
    xlim(-IC.Lim[2], IC.Lim[2]) +
    theme_bw() +
    theme(panel.grid = element_blank()) +
    theme(panel.border = element_blank()) +
    theme(axis.title.x = element_text(color = "white")) +
    theme(axis.title.y = element_blank()) +
    theme(axis.text = element_blank()) +
    theme(axis.ticks = element_blank()) +
    coord_flip()
  
  Combinition <- list(IC.Plot = IC.Plot, Hist.Top = Hist.Top, Hist.Right = Hist.Right)
  
  return(Combinition)
}