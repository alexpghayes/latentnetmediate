
# row 1 of theta_tc describes what happens to block 1 of C
theta_tc <- rbind(
  c(-1, 1, 0, 0, 0), # block one jumps to block two,
  c(0, 0, 0, 0, 0),
  c(0, 1, -1, 0, 0), # block three jumps to block two
  c(0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0)
)


model <- model_informative_controls(n = 50, theta_tc = theta_tc)

graph <- sample_tidygraph(model)

netmediate(graph, y ~ trt + C1 + C2 + C3 + C4 + C5, rank = 5)


plot_intervention(model)

model1(n = 50, theta_tc = theta_tc) |>
  plot_intervention(post_multiply = TRUE)


model1(n = 50) |>
  plot_intervention()

