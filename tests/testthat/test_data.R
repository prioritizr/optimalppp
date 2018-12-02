context("data")

test_that("sim_project_data", {
  # data.frame properties
  data(sim_project_data)
  expect_is(sim_project_data, c("data.frame", "tbl_df"))
  expect_equal(ncol(sim_project_data), 6)
  expect_equal(nrow(sim_project_data), 6)
  # name column
  expect_is(sim_action_data$name, "character")
  expect_equal(anyDuplicated(sim_action_data$name), 0)
  expect_true(assertthat::noNA(sim_action_data$cost))
  # species persistence probability columns
  for (s in paste0("S", seq_len(5))) {
    expect_is(sim_project_data[[s]], "numeric")
    expect_true(all(sim_project_data[[s]] >= 0))
    expect_true(all(sim_project_data[[s]] <= 1))
    expect_true(assertthat::noNA(sim_project_data[[s]]))
  }
})

test_that("sim_action_data", {
  # data.frame properties
  data(sim_action_data)
  expect_is(sim_action_data, c("data.frame", "tbl_df"))
  expect_equal(ncol(sim_action_data), 5)
  expect_equal(nrow(sim_action_data), 6)
  # name column
  expect_is(sim_action_data$name, "character")
  expect_equal(anyDuplicated(sim_action_data$name), 0)
  expect_true(assertthat::noNA(sim_action_data$cost))
  # cost column
  expect_is(sim_action_data$cost, "numeric")
  expect_true(all(sim_action_data$cost >= 0))
  expect_true(assertthat::noNA(sim_action_data$cost))
  # success column
  expect_is(sim_action_data$success, "numeric")
  expect_true(all(sim_action_data$success >= 0))
  expect_true(all(sim_action_data$success <= 1))
  expect_true(assertthat::noNA(sim_action_data$success))
  # locked in column
  expect_is(sim_action_data$locked_in, "logical")
  expect_true(sum(sim_action_data$locked_in) == 1)
  expect_true(assertthat::noNA(sim_action_data$locked_in))
  # locked out column
  expect_is(sim_action_data$locked_out, "logical")
  expect_true(sum(sim_action_data$locked_out) == 1)
  expect_true(assertthat::noNA(sim_action_data$locked_out))
  expect_equal(max(sim_action_data$locked_in + sim_action_data$locked_out), 1)
})

test_that("sim_organization_data", {
  # data.frame properties
  data(sim_organization_data)
  expect_is(sim_organization_data, c("data.frame", "tbl_df"))
  expect_equal(ncol(sim_organization_data), 7)
  expect_equal(nrow(sim_organization_data), 6)
  # name column
  expect_is(sim_organization_data$name, "character")
  expect_equal(anyDuplicated(sim_organization_data$name), 0)
  # species persistence probability columns
  for (s in c(paste0("S", seq_len(5), "_action"), "baseline_action")) {
    expect_is(sim_organization_data[[s]], "logical")
    expect_true(assertthat::noNA(sim_organization_data[[s]]))
  }
})

test_that("sim_tree", {
  data(sim_tree)
  expect_is(sim_tree, "phylo")
  expect_is(suppressMessages(ape::checkValidPhylo(sim_tree)), "NULL")
  expect_equal(sort(sim_tree$tip.label), sort(paste0("S", seq_len(5))))
})
