test_that("check_args works", {
  sim_m_p <- simcausal::DAG.empty() %>%
    simcausal::add.nodes(
      simcausal::node("P0init", # initial value for the age of first participation
        distr = "truncnorm::rtruncnorm",
        a = a_P0init, # 5.5
        b = b_P0init, # 17.7
        mean = mean_P0init, # 0
        sd = sd_P0init # 11
      )
    ) %>%
    simcausal::add.nodes(
      simcausal::node("P0", # final value for the age of first participation
        distr = "rconst",
        const = round(P0init, 0)
      )
    ) %>%
    simcausal::add.nodes(
      simcausal::node("lop", # length of participation (in years)
        distr = "rdiscunif",
        min = lop_min,
        max = lop_max
      )
    ) %>%
    simcausal::add.nodes(
      simcausal::node("Plast", # age of last participation
        distr = "rconst",
        const = min(P0 + lop - 1, 24) # -1 as lop is # of part., not follow-ups
      )
    ) %>%
    simcausal::add.nodes(
      simcausal::node("P", # Participation
        t = 12:24,
        distr = "rconst",
        const = as.numeric(t >= P0 & t <= Plast)
      )
    )

  expect_equal(
    check_args(dag = sim_m_p, args = NULL),
    c("a_P0init", "b_P0init", "mean_P0init", "sd_P0init", "lop_min", "lop_max")
  )
  expect_equal(
    check_args(dag = sim_m_p, args = list(a_P0init = 1)),
    c("b_P0init", "mean_P0init", "sd_P0init", "lop_min", "lop_max")
  )
  expect_equal(
    check_args(dag = sim_m_p, args = list(a_P0init = 1, lop_min = 1)),
    c("b_P0init", "mean_P0init", "sd_P0init", "lop_max")
  )
  expect_equal(
    check_args(dag = sim_m_p, args = list(
      a_P0init = 1, b_P0init = 1, mean_P0init = 1,
      sd_P0init = 1, lop_min = 1, lop_max = 1
    )),
    character(0)
  )
})

test_that("run_sim works", {
  sim_m_p <- simcausal::DAG.empty() %>%
    simcausal::add.nodes(
      simcausal::node("P0init", # initial value for the age of first participation
        distr = "truncnorm::rtruncnorm",
        a = a_P0init, # 5.5
        b = b_P0init, # 17.7
        mean = mean_P0init, # 0
        sd = sd_P0init # 11
      )
    ) %>%
    simcausal::add.nodes(
      simcausal::node("P0", # final value for the age of first participation
        distr = "rconst",
        const = round(P0init, 0)
      )
    ) %>%
    simcausal::add.nodes(
      simcausal::node("lop", # length of participation (in years)
        distr = "rdiscunif",
        min = lop_min,
        max = lop_max
      )
    ) %>%
    simcausal::add.nodes(
      simcausal::node("Plast", # age of last participation
        distr = "rconst",
        const = min(P0 + lop - 1, 24) # -1 as lop is # of part., not follow-ups
      )
    ) %>%
    simcausal::add.nodes(
      simcausal::node("P", # Participation
        t = 12:24,
        distr = "rconst",
        const = as.numeric(t >= P0 & t <= Plast)
      )
    )

  sim_m_a_v1 <- simcausal::DAG.empty() %>%
    simcausal::add.nodes(
      simcausal::node("asthma",
        t = 6,
        distr = "rconst",
        const = ifelse(P0 > 6, NA, 1)
      )
    ) %>%
    simcausal::add.nodes(
      simcausal::node("asthma",
        t = 7:24,
        "rbern",
        prob = ifelse(P0 > t,
          NA,
          ifelse(P0 == t,
            1,
            ifelse(asthma[t - 1] == 1,
              1 - p_remission,
              p_relapse
            )
          )
        )
      )
    )

  x <- run_sim(
    dags = list(sim_m_p, sim_m_a_v1),
    args <- list(
      mean_P0init = 0,
      sd_P0init = 11,
      a_P0init = 5.5,
      b_P0init = 17.7,
      lop_min = 1,
      lop_max = 11,
      p_remission = 0.2,
      p_relapse = 0.1
    ),
    n = 5
  )

  cols <- c(
    "ID", "P0init", "P0", "lop", "Plast", "asthma_6", "asthma_7", "asthma_8",
    "asthma_9", "asthma_10", "asthma_11", "P_12", "asthma_12", "P_13",
    "asthma_13", "P_14", "asthma_14", "P_15", "asthma_15", "P_16", "asthma_16",
    "P_17", "asthma_17", "P_18", "asthma_18", "P_19", "asthma_19", "P_20",
    "asthma_20", "P_21", "asthma_21", "P_22", "asthma_22", "P_23", "asthma_23",
    "P_24", "asthma_24"
  )

  expect_equal(colnames(x), cols)
  expect_equal(nrow(x), 5)

  x1 <- run_sim(
    dags = list(sim_m_p, sim_m_a_v1),
    args <- list(
      mean_P0init = 0,
      sd_P0init = 11,
      a_P0init = 5.5,
      b_P0init = 17.7,
      lop_min = 1,
      lop_max = 11,
      p_remission = 0.2,
      p_relapse = 0.1
    ),
    n = 5,
    seed = 655065
  )

  x2 <- run_sim(
    dags = list(sim_m_p, sim_m_a_v1),
    args <- list(
      mean_P0init = 0,
      sd_P0init = 11,
      a_P0init = 5.5,
      b_P0init = 17.7,
      lop_min = 1,
      lop_max = 11,
      p_remission = 0.2,
      p_relapse = 0.1
    ),
    n = 5,
    seed = 655065
  )

  expect_equal(x1, x2)
})

test_that("run_sim and check_args works for dags without any args", {
  simtest <- simcausal::DAG.empty() %>%
    simcausal::add.nodes(
      simcausal::node("x",
        distr = "rnorm",
        mean = 0,
        sd = 14
      )
    )

  expect_equal(check_args(simtest, NULL), character(0))

  x <- run_sim(dags = list(simtest), n = 5)
  expect_true(is.data.frame(x))
  expect_equal(nrow(x), 5)
  expect_equal(colnames(x), c("ID", "x"))
})
