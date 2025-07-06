#Install the required packages
#install.packages(c('hexbin', 'RColorBrewer', 'scales', 'ggplot2', 'microbenchmark'))

library(RColorBrewer)
library(ggplot2)
library(scales)
library(KernSmooth)
library(microbenchmark)

data(diamonds)

mydiamonds <- data.frame(lprice = log(diamonds$price, 2),
                            lcarat = log(diamonds$carat, 2))

# calculate lm
mylm <- lm(lprice ~ lcarat, data = mydiamonds)

# calculate NW kernel
myks <- ksmooth(x = mydiamonds$lcarat, y = mydiamonds$lprice,
                    kernel = 'normal', n.points = 200)

myls1 <- loess(mydiamonds$lprice ~ mydiamonds$lcarat)

myls2 <- loess(mydiamonds$lprice ~ mydiamonds$lcarat,
                        control =
                        loess.control(surface = "interpolate",
                                    statistics = "approximate",
                                    trace.hat = "approximate", cell = 0.2))

tim1 <- microbenchmark(ksmooth(x = mydiamonds$lcarat, y = mydiamonds$lprice,
                    kernel = 'normal', n.points = 200), times = 100L)

tim2 <- microbenchmark(loess(mydiamonds$lprice ~ mydiamonds$lcarat), times = 25L)

tim3 <- microbenchmark(loess(mydiamonds$lprice ~ mydiamonds$lcarat,
                        control =
                        loess.control(surface = "interpolate",
                                    statistics = "approximate",
                                    trace.hat = "approximate", cell = 0.2)), times = 100L)

# calculate alternate smoother (also NW kernel, but using different defaults)
h <- dpill(x = mydiamonds$lcarat, y = mydiamonds$lprice)
fit <- locpoly(x = mydiamonds$lcarat, y = mydiamonds$lprice, bandwidth = h)

# prepare loess smoother for plotting
ord <- order(myls2$x)
ldf <- data.frame(x = myls2$x[ord], y = myls2$fitted[ord])

my_colors1 = brewer.pal(7, 'Blues')

d <- ggplot(mydiamonds, aes(lcarat, lprice))
d + geom_hex(binwidth = c(.15, .15)) +
    scale_fill_continuous(low = my_colors1[2], high = my_colors1[7]) +
    labs(x = "Log2 Carat" , y = "Log2 Price", title = "Diamonds Dataset") +
    ylim(8.2, 16) +
    geom_line(data = data.frame(myks), aes(x = x, y = y), col = 'green', lwd = 1.1) +
    geom_line(data = ldf, aes(x = x, y = y), col = 'darkorange', lwd = 1.1)
    #geom_line(data = data.frame(fit), aes(x = x, y = y), col = 'black', lwd = 1.1)
    #uncomment the above line to activate alternate smoother
