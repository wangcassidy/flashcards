draw_card <- function(text, title="") {
  card <- data.frame(text = text)
  
  if (nchar(text) < 20) {
    ggplot(card) +
      geom_rect(aes(xmin = 1, xmax = 2, ymin = 1, ymax = 2),
                color = "black", fill = "white") +
      geom_text(aes(x = 1.5, y = 1.5, label = text),
                size = 19) +
      theme_classic() +
      theme(axis.line  = element_blank(),
            axis.ticks = element_blank(),
            axis.text  = element_blank(),
            axis.title = element_blank(),
            plot.title = element_text(hjust = 0.5, size=22)) +
      ggtitle(title)
  } else {
    ggplot(card) +
      aes(label = text, xmin = 0.5, xmax = 4.5, ymin = 1,
          ymax = 4) +
      geom_rect(aes(xmin=0, xmax=5, ymin=0, ymax=5), color = "black", fill = "white") +
      geom_fit_text(reflow=TRUE, grow=TRUE) +
      theme_classic() +
      theme(axis.line  = element_blank(),
            axis.ticks = element_blank(),
            axis.text  = element_blank(),
            axis.title = element_blank(),
            plot.title = element_text(hjust = 0.5, size=22)) +
      ggtitle(title)
  }
}
