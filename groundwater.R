# Creating groundwater simulation
clean.groundwater <- rnorm(10000, mean = 25, sd = 5)
contam.groundwater <- rnorm(10000, mean = 35, sd = 5)

# Taking 100,000 samples from each groundwater population
clean.meansample <- replicate(100000, mean(sample(clean.groundwater, size = 5, replace = FALSE)), simplify = "vector")
clean.maxsample <- replicate(100000, max(sample(clean.groundwater, size = 5, replace = FALSE)), simplify = "vector")
contam.meansample <- replicate(100000, mean(sample(contam.groundwater, size = 5, replace = FALSE)), simplify = "vector")
contam.maxsample <- replicate(100000, max(sample(contam.groundwater, size = 5, replace = FALSE)), simplify = "vector")

# Mean and standard deviation of each sample strategy by groundwater type
clean.meansample.mean <- mean(clean.meansample)
clean.meansample.sd <- sd(clean.meansample)
contam.meansample.mean <- mean(contam.meansample)
contam.meansample.sd <- sd(contam.meansample)
clean.maxsample.mean <- mean(clean.maxsample)
clean.maxsample.sd <- sd(clean.maxsample)
contam.maxsample.mean <- mean(contam.maxsample)
contam.maxsample.sd <- sd(contam.maxsample)

# Plotting maxsamples distributions
clean.meansample.dist <- dnorm(x, mean = clean.meansample.mean, sd = clean.meansample.sd)
contam.meansample.dist <- dnorm(x, mean = contam.meansample.mean, sd = contam.meansample.sd)
plot(x, contam.meansample.dist, type="l", xlab = "Critical Value", ylab = "Density")
lines(x, clean.meansample.dist)
text(17, 0.1, "clean")
text(46, 0.1, "contaminated")
title("Distribution of Sample Statistics (Mean)", sub = "Figure 1")

clean.maxsample.dist <- dnorm(x, mean = clean.maxsample.mean, sd = clean.maxsample.sd)
contam.maxsample.dist <- dnorm(x, mean = contam.maxsample.mean, sd = contam.maxsample.sd)
plot(x, contam.maxsample.dist, type="l", xlab = "Critical Value", ylab = "Density")
lines(x, clean.maxsample.dist)
text(24, 0.08, "clean")
text(52, 0.08, "contaminated")
title("Distribution of Sample Statistics (Max)", sub = "Figure 2")

# Functions to return sum of samples that reject H0
clean.fun <- function(x){
  y <- ifelse(clean.meansample > x, 1, 0)
  z <- sum(y)
  z
}

contam.fun <- function(x){
  y <- ifelse(contam.meansample > x, 1, 0)
  z <- sum(y)
  z
}

clean.fun2 <- function(x){
  y <- ifelse(clean.maxsample > x, 1, 0)
  z <- sum(y)
  z
}

contam.fun2 <- function(x){
  y <- ifelse(contam.maxsample > x, 1, 0)
  z <- sum(y)
  z
}

# Creating data frames for sequence of rejection regions
clean.seq <- seq(from = 15, to = 45, by = 0.5)

clean.seq.vec <- sapply(clean.seq, clean.fun)
clean.results.df <- data.frame(clean.seq, clean.seq.vec)
clean.results.df$Percent <- clean.results.df$clean.seq.vec / 100000

contam.seq <- seq(from = 15, to = 45, by = 0.5)
contam.seq.vec <- sapply(contam.seq, contam.fun)
contam.results.df <- data.frame(contam.seq, contam.seq.vec)
contam.results.df$Percent <- contam.results.df$contam.seq.vec / 100000

RejReg <- clean.seq
CleanReg <- clean.seq.vec
CleanRegPct <- clean.results.df$Percent
ContamReg <- contam.seq.vec
ContamRegPct <- contam.results.df$Percent
combined <- data.frame(RejReg, CleanReg, CleanRegPct, ContamReg, ContamRegPct)

clean.seq2 <- seq(from = 20, to = 50, by = 0.5)
RejReg2 <- clean.seq2
CleanReg2 <- sapply(clean.seq2, clean.fun2)
CleanRegPct2 <- CleanReg2 / 100000
ContamReg2 <- sapply(clean.seq2, contam.fun2)
ContamRegPct2 <- ContamReg2 / 100000
combined2 <- data.frame(RejReg2, CleanReg2, CleanRegPct2, ContamReg2, ContamRegPct2)

# Plot of percentage of samples in rejection region
plot(clean.seq, contam.results.df$Percent, type="l", ylim = 0:1, xlab = "Critical Value", ylab = "Percentage")
lines(clean.seq, clean.results.df$Percent)
title("% of Sample Statistics above Critical Value (Mean)", sub = "Figure 3")
text(38.5, 0.6, "contaminated")
text(22.5, 0.6, "clean")

plot(clean.seq2, combined2$ContamRegPct2, type = "l", ylim = 0:1, xlab = "Critical Value", ylab = "Percentage")
lines(clean.seq2, combined2$CleanRegPct2)
title("% of Sample Statistics above Critical Value (Max)", sub = "Figure 4")
text(44, 0.6, "contaminated")
text(27.5, 0.6, "clean")

# Plot of differences between percentage of clean and contaminated samples in rejection region
diff.seq <- combined$ContamRegPct - combined$CleanRegPct
plot(clean.seq, diff.seq, type = "l", xlab = "Critical Value", ylab = "Difference")
title("Difference of Sample Statistics above Critical Value (Mean)", sub = "Figure 5", cex.main = 0.9)
max(diff.seq)
clean.seq[diff.seq == max(diff.seq)]
diff.df <- data.frame(clean.seq, combined$ContamRegPct, combined$CleanRegPct, diff.seq, clean.seq2, combined2$ContamRegPct2, combined2$CleanRegPct2, diff.seq2)

diff.seq2 <- combined2$ContamRegPct2 - combined2$CleanRegPct2
plot(clean.seq2, diff.seq2, type = "l", xlab = "Critical Value", ylab = "Difference")
title("Difference of Sample Statistics above Critical Value (Max)", sub = "Figure 6", cex.main = 0.9)
max(diff.seq2)
clean.seq2[diff.seq2 == max(diff.seq2)]

# EXTRA CODE
# Plotting groundwater distributions
x <- seq(0, 60, length = 100)
clean <- dnorm(x, mean = 25, sd = 5)
contam <- dnorm(x, mean = 35, sd = 5)
plot(x, clean, type = "l", ylab = "dnorm(x)")
title(main = "Comparison of Clean vs. Contaminated Groundwater Distributions", cex.main = 0.95)
lines(x, contam)
text(15, 0.06, "clean")
text(48, 0.06, "contaminated")

# Plotting theoretical sample mean distributions
clean.five <- dnorm(x, mean = 25, sd = 5 / sqrt(5))
contam.five <- dnorm(x, mean = 35, sd = 5 / sqrt(5))
plot(x, clean.five, type="l", ylab = "dnorm(x)")
lines(x, contam.five)
