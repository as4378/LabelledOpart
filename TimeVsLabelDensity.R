library(data.table)
library(directlabels)
library(ggplot2)
library(penaltyLearning)
library(dplyr)
library(data.table)
library(microbenchmark)



timing_list <- list()
start <- 0
end <- 0

# set density here, density means -> 1 label per density number of points
density <- 50

for(i in seq(100,80680,10)){
  print(i)
  size <- i
  signal <- rnorm(size, mean=i)

  start <- density
  end <- size - density

  labels <- data.frame("starts" <- seq(start, end, density), "ends" <- seq(start + 5, end + 5, density))
  names(labels) <- c("starts", "ends")
  labels$breaks <- 1

  timing <- microbenchmark(
    "labelled_opart"={
      LabelledOpart::labelled_opart_gaussian(signal, labels, 5)
    },
    "fpop"={
      if(requireNamespace("fpop", quietly = TRUE))
        fpop::Fpop(signal, 5)
    },
    "opart"={
      opart::opart_gaussian(signal, 5)
    },
    times=3)

  timing_list[[paste(i)]] <- data.table("labels", size, timing)
}
timing.dt2 <- do.call(rbind, timing_list)

# read data computed using above code
# comment this line if running above code for fresh results
timing.dt2 <- read.csv("vignettes/TimeVsLabelDensity.csv")


timing.dt2$time <- timing.dt2$time / (10^(9))
timing.dt2$V1 <- "size(label density = 1 per 20)"

# read data for 5 labels
timing.dt <- read.csv("vignettes/TimingVsLabels.csv")

# read data for fixed length data set and increasing labels
timing.dt1 <- read.csv("vignettes/TimingList.csv")
timing.dt1$time <- timing.dt1$time / (10^(9))
timing.dt1$V1 <- "size"

lab.df <- data.frame(seconds <- c(1,60),
                     times <- c("1 second", "1 minute"))

comb_data <- rbind(timing.dt2, timing.dt1, timing.dt)
comb_data$expr <- as.character(comb_data$expr)
comb_data$expr <- ifelse(comb_data$expr == "labelled_opart", "labelled\nopart", comb_data$expr)

lab.df1 <- lab.df
lab.df$V1 <- "labels"
lab.df1$V1 <- "size"

lab.df2 <- lab.df
lab.df2$V1 <- "size(label density = 1 per 20)" # change these labels according to set density

png("file.png", width=12, height=7, units="in", res=200)

plt <- ggplot(data = comb_data, aes(x = size, y = time, col = expr)) +
  geom_point() +
  geom_smooth() +
  geom_hline(aes(yintercept=(seconds)),
             data=lab.df,
             color="grey")+
  geom_hline(aes(yintercept=(seconds)),
             data=lab.df1,
             color="grey")+
  geom_hline(aes(yintercept=(seconds)),
             data=lab.df2,
             color="grey")+
  geom_text(aes(1, (seconds), label=times),
            data=lab.df,
            size=2,
            color="black",
            vjust=-0.5)+
  geom_text(aes(15, 1000, label="datasize=100000"),
            data=lab.df,
            size=3,
            color="black",
            vjust=-0.5)+
  geom_text(aes(4000, 1000, label="No. of labels = 5"),
            data=lab.df1,
            size=3,
            color="black",
            vjust=-0.5)+
  geom_text(aes(4000, 1000, label="size(label density = 1 per 20)"),
            data=lab.df2,
            size=3,
            color="black"
  )+
  facet_grid(. ~ V1, scales="free")+
  scale_x_log10("") + scale_y_log10("time(s)")

directlabels::direct.label(plt, "last.polygons")
dev.off()

