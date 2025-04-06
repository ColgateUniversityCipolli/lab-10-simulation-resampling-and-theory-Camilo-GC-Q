library(ggplot2)
library(tidyverse)
library(patchwork)

# 1 Basic Simulation

true.prob = 0.39
n1 = 1004
n2 = 2008
simulations = 10000

poll1 = rbinom(simulations, n1, true.prob) / n1

poll2 = rbinom(simulations, n2, true.prob) / n2

plot_simulation = function(polls, title){
  df = data.frame(polls)
  ggplot(df, aes(x = polls)) +
    geom_histogram(aes(y = after_stat(density)), bins = 50) + 
    geom_density(color = "red") +
    labs(title = title,
         x = "Sample Proportion",
         y = "Density") + 
    theme_minimal()
}

plot1 = plot_simulation(poll1, "Sampling Distribution (n = 1000)")
plot2 = plot_simulation(poll2, "Sampling Distribution (n = 2000)")

(plot1 | plot2)


range1 = quantile(poll1, c(0.025, 0.975))
range1
range2 = quantile(poll2, c(0.025, 0.975))
range2

margin.error1 = (range1[2] - range1[1]) / 2
margin.error1
margin.error2 = (range2[2] - range2[1]) / 2
margin.error2


# 2 Resampling

n = 1004
satisfied = round(n * 0.39)

og.sample = tibble(
  id = 1:n,
  measurement = c(rep(1,satisfied), rep(0, n - satisfied))
)

og.sample |>
  summarize(mean = mean(measurement))

resample.means = numeric(10000)

for (i in 1:10000){
  resample.id = sample(1:n, size = n, replace = TRUE)
  resample.means[i] = mean(og.sample$measurement[resample.id])
}

resample.df = tibble(p.hat = resample.means)

ggplot(resample.df, aes(x = p.hat)) + 
  geom_histogram(aes(y = after_stat(density)), bins = 15) +
  geom_density(color = "red") +
  labs(title = "Resampled Sample Proportions (Gallup Survey)",
       x = "Sample Proportion",
       y = "Density") +
  theme_minimal()

range.95 = quantile(resample.means, c(0.025, 0.975))
range.95
margin.error = (range.95[2] - range.95[1]) / 2
margin.error


# 3 Simulation over n and p

n.values = seq(100, 3000, by = 10)
p.values = seq(0.01, 0.99, by = 0.01)

results = expand.grid(n = n.values, p = p.values) |>
  mutate(n = as.numeric(n), p = as.numeric(p))

set.seed(123)

for (i in 1:nrow(results)){
  n = results$n[i]
  for (j in 1:ncol(results)){
    p = results$p[j]
    
    simus = rbinom(10000, n, p) / n
    range_95 = quantile(simus, c(0.025, 0.975))
    results$margin_error[i] = (range_95[2] - range_95[1]) / 2
  }
}
ggplot(results, aes(x = p, y = n, fill = margin_error)) +
  geom_tile() +
  labs(title = "Margin of Error as a function of n and p",
       x = "Proportion (p)",
       y = "Sample Size (n)",
       fill = "Margin of Error") +
  theme_minimal()+
  scale_fill_viridis_c()


# 4 

nvals = seq(100, 2000, by = 10)
pvals = seq(0.01, 0.99, by = 0.01)
z = 1.96
grid = expand.grid(n = nvals, p = pvals) |>
  mutate(n = as.numeric(n), p = as.numeric(p))

grid = grid |>
  mutate(
    wilson = (z * ((sqrt(n*p*(1-p) + (z^2 / 4)))/(n + z^2)))
    )

ggplot(grid, aes(x = p, y = n, fill = wilson)) + 
  geom_tile() +
  scale_fill_viridis_c(option = "plasma") +
  labs(
    title = "Wilson Margin of Error as a function of n and p",
    x = "Proportion (p)",
    y = "Sample Size (n)"
  ) + 
  theme_minimal()



