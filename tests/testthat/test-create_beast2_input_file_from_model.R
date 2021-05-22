test_that("use", {

  output_filename <- get_beautier_tempfilename()
  expect_false(file.exists(output_filename))

  expect_silent(
    create_beast2_input_file_from_model(
      get_fasta_filename(),
      output_filename
    )
  )
  expect_true(file.exists(output_filename))
  file.remove(output_filename)
})

test_that("abuse", {


  # input_filenames
  expect_error(
    create_beast2_input_file_from_model(
      input_filename = "nonexisting", # Error
      output_filename = "irrelevant"
    ),
    "'input_filename' not found"
  )

  # 2 site_model
  expect_error(
    create_beast2_input_file_from_model(
      input_filename = get_fasta_filename(),
      output_filename = "irrelevant",
      inference_model = "nonsense"
    ),
    "'inference_model' must be an inference model"
  )
})

test_that("cannot create CBS with less than 6 taxa", {
  # Tested by 'check_file_and_model_agree'
})

test_that("Can specify lower, upper and value in normal distr", {
  # See https://github.com/ropensci/beautier/issues/121
  inference_model <- create_inference_model(
    tree_prior = create_ccp_tree_prior(
      pop_size_distr = create_normal_distr(
        lower = "12.0",
        upper = "345.0",
        value = "100.0"
      )
    ),
    beauti_options = create_beauti_options_v2_6()
  )

  text <- create_beast2_input_from_model(
    get_beautier_path("anthus_nd2_sub.fas"),
    inference_model
  )

  expected <- paste0(
    "            ",
    "<parameter id=\"popSize.t:anthus_nd2_sub\" ",
      "spec=\"parameter.RealParameter\" lower=\"12.0\" name=\"stateNode\" ",
      "upper=\"345.0\">",
      "100.0",
    "</parameter>"
  )
  created <- stringr::str_subset(text, "popSize.t:anthus_nd2_sub.*stateNode")
  expect_equal(expected, created)
})

test_that("clockRate.c ID added twice", {

  # From https://github.com/ropensci/beautier/issues/127

  # See the duplicated input in lines 1 and 3 below:
  # <parameter id="clockRate.c:THAILAND_TEST.clust_1.dated" name="stateNode">0.0000001</parameter>                       # nolint indeed a long line
  #         <parameter id="popSize.t:THAILAND_TEST.clust_1.dated" lower="0" name="stateNode" upper="200">100</parameter> # nolint indeed a long line
  #         <parameter id="clockRate.c:THAILAND_TEST.clust_1.dated" name="stateNode">1.0</parameter>                     # nolint indeed a long line

  # clock_model
  clock_rate <- 0.0000001
  clock_model <- create_strict_clock_model(
    clock_rate_param = create_clock_rate_param(value = clock_rate),
    clock_rate_distr = create_log_normal_distr(
      value = clock_rate,
      m = 1,
      s = 1.25
    )
  )

  # MRCA PRIOR
  mrca_prior <- create_mrca_prior(
    is_monophyletic = TRUE,
    mrca_distr = create_laplace_distr(mu = 1990)
  )


  inference_model <- create_inference_model(
    clock_model = clock_model,
    mrca_prior = mrca_prior,
    tipdates_filename = get_beautier_path("THAILAND_TEST.clust_1.dated.txt")
  )


  text <- create_beast2_input_from_model(
    input_filename = get_beautier_path("THAILAND_TEST.clust_1.dated.fa"),
    inference_model = inference_model
  )

  # One sloppy match
  matches <- stringr::str_subset(
    string = text,
    pattern = "<parameter id=\"clockRate.c:THAILAND_TEST.clust_1.dated\" name=\"stateNode\">" # nolint indeed long
  )
  expect_equal(1, length(matches))
  # Must be the exact correct match
  matches <- stringr::str_subset(
    string = text,
    pattern = "<parameter id=\"clockRate.c:THAILAND_TEST.clust_1.dated\" name=\"stateNode\">0.0000001</parameter>" # nolint indeed long
  )
  expect_equal(1, length(matches))

})

test_that("ClockPrior.c ID added twice", {

  # From https://github.com/ropensci/beautier/issues/128

  # <distribution id="posterior" spec="util.CompoundDistribution">
  #     <distribution id="prior" spec="util.CompoundDistribution">
  #         <distribution id="YuleModel.t:THAILAND_TEST.clust_1.dated" spec="beast.evolution.speciation.YuleModel" birthDiffRate="@birthRate.t:THAILAND_TEST.clust_1.dated" tree="@Tree.t:THAILAND_TEST.clust_1.dated"/> # nolint long line indeed
  #         # ...
  #         <prior id="ClockPrior.c:THAILAND_TEST.clust_1.dated" name="distribution" x="@clockRate.c:THAILAND_TEST.clust_1.dated"> # nolint long line indeed
  #             <Uniform id="Uniform.150" name="distr" upper="Infinity"/>
  #         </prior>
  #         # ...
  #         <prior id="ClockPrior.c:THAILAND_TEST.clust_1.dated" name="distribution" x="@clockRate.c:THAILAND_TEST.clust_1.dated"> # nolint long line indeed
  #             <LogNormal id="LogNormalDistributionModel.0" name="distr">
  #                 <parameter id="RealParameter.0" estimate="false" name="M">1</parameter> # nolint long line indeed
  #                 <parameter id="RealParameter.1" estimate="false" lower="0" name="S" upper="Infinity">1.25</parameter> # nolint long line indeed
  #             </LogNormal>
  #         </prior>
  #     </distribution>
  # </distribution>

  # clock_model
  clock_rate <- 0.0000001
  clock_model <- create_strict_clock_model(
    clock_rate_param = create_clock_rate_param(value = clock_rate),
    clock_rate_distr = create_log_normal_distr(
      value = clock_rate,
      m = 1,
      s = 1.25
    )
  )

  # MRCA PRIOR
  mrca_prior <- create_mrca_prior(
    is_monophyletic = TRUE,
    mrca_distr = create_laplace_distr(mu = 1990)
  )


  inference_model <- create_inference_model(
    clock_model = clock_model,
    mrca_prior = mrca_prior,
    tipdates_filename = get_beautier_path("THAILAND_TEST.clust_1.dated.txt")
  )


  text <- create_beast2_input_from_model(
    input_filename = get_beautier_path("THAILAND_TEST.clust_1.dated.fa"),
    inference_model = inference_model
  )
  # One sloppy match
  matches <- stringr::str_subset(
    string = text,
    pattern = "<prior id=\"ClockPrior.c:THAILAND_TEST.clust_1.dated\" name=\"distribution\" x=\"@clockRate.c:THAILAND_TEST.clust_1.dated\">" # nolint indeed long
  )
  expect_equal(1, length(matches))
  # Must be the exact correct match
  matches <- stringr::str_subset(
    string = text,
    pattern = "<prior id=\"ClockPrior.c:THAILAND_TEST.clust_1.dated\" name=\"distribution\" x=\"@clockRate.c:THAILAND_TEST.clust_1.dated\">" # nolint indeed long
  )
  expect_equal(1, length(matches))
})

test_that("ID missing", {

  # https://github.com/ropensci/babette/issues/96
  gamma_site_distr <- create_log_normal_distr(
    value = 0.25,
    lower = 0.0,
    upper = 1.0,
    m = 1,
    s = 1.25
  )
  gamma_site_model <- create_gamma_site_model(
    gamma_cat_count = 4,
    gamma_shape_prior_distr = gamma_site_distr
  )
  # The error was that 'gamma_site_model' was assigned to the ID,
  # the first argument of 'create_gtr_site_model'
  site_model <- create_gtr_site_model(
    gamma_site_model = gamma_site_model
  )

  inference_model <- create_inference_model(
    site_model = site_model
  )

  text <- create_beast2_input_from_model(
    input_filename = get_beautier_path("THAILAND_TEST.clust_1.dated.fa"),
    inference_model = inference_model
  )

  expect_error(
    create_gtr_site_model(
      id = gamma_site_model
    ),
    "'id' must be NA \\(recommended\\) or an ID"
  )
})
