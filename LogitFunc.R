# On the ggplot example.
# This example was taken form an example at R-bloggers. Is to have a plot inset into 
# other plot ggplot
library(ggplot2)

# generate fake data
set.seed(42)
n <- 1000
x <- (runif(n) * 10) - 5 
y <- (exp(x) / (1 + exp(x)))
df <- data.frame(x = x, y = y)

# overall plot
p1 <- ggplot(df, aes(x, y)) +
    geom_point(alpha = 0.3) +
    geom_smooth(se = FALSE) +
    theme_bw()
p1

# zoomed in plot
p2 <- ggplot(df, aes(x, y)) +
    geom_point(alpha = 0.3) +
    geom_smooth(se = FALSE) +
    scale_x_continuous(limits = c(1, 4)) +
    scale_y_continuous(limits = c(.75, 1.0)) +
    theme_bw()
p2

# plot with inset
p1 + annotation_custom(ggplotGrob(p2), xmin = -4, xmax = -2, 
                       ymin = 0.6, ymax = 0.9)
