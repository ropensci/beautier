pkgname <- "beautier"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "beautier-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('beautier')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("bd_tree_prior_to_xml_prior_distr")
### * bd_tree_prior_to_xml_prior_distr

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: bd_tree_prior_to_xml_prior_distr
### Title: Creates the tree prior section in the prior section of the prior
###   section of the distribution section of a BEAST2 XML parameter file
###   for a Birth-Death tree prior
### Aliases: bd_tree_prior_to_xml_prior_distr

### ** Examples

 # <distribution id="posterior" spec="util.CompoundDistribution">
 #     <distribution id="prior" spec="util.CompoundDistribution">
 #       HERE, where the ID of the distribution is 'prior'
 #     </distribution>
 #     <distribution id="likelihood" ...>
 #     </distribution>
 # </distribution>



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("bd_tree_prior_to_xml_prior_distr", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("cbs_tree_prior_to_xml_prior_distr")
### * cbs_tree_prior_to_xml_prior_distr

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: cbs_tree_prior_to_xml_prior_distr
### Title: Creates the tree prior section in the prior section of the prior
###   section of the distribution section of a BEAST2 XML parameter file
###   for a Birth-Death tree prior
### Aliases: cbs_tree_prior_to_xml_prior_distr

### ** Examples

 # <distribution id="posterior" spec="util.CompoundDistribution">
 #     <distribution id="prior" spec="util.CompoundDistribution">
 #       HERE, where the ID of the distribution is 'prior'
 #     </distribution>
 #     <distribution id="likelihood" ...>
 #     </distribution>
 # </distribution>



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("cbs_tree_prior_to_xml_prior_distr", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ccp_tree_prior_to_xml_prior_distr")
### * ccp_tree_prior_to_xml_prior_distr

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ccp_tree_prior_to_xml_prior_distr
### Title: Creates the tree prior section in the prior section of the prior
###   section of the distribution section of a BEAST2 XML parameter file
###   for a Coalescent Constant Population tree prior
### Aliases: ccp_tree_prior_to_xml_prior_distr

### ** Examples

 # <distribution id="posterior" spec="util.CompoundDistribution">
 #     <distribution id="prior" spec="util.CompoundDistribution">
 #       HERE, where the ID of the distribution is 'prior'
 #     </distribution>
 #     <distribution id="likelihood" ...>
 #     </distribution>
 # </distribution>



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ccp_tree_prior_to_xml_prior_distr", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("cep_tree_prior_to_xml_prior_distr")
### * cep_tree_prior_to_xml_prior_distr

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: cep_tree_prior_to_xml_prior_distr
### Title: Creates the tree prior section in the prior section of the prior
###   section of the distribution section of a BEAST2 XML parameter file
###   for a Coalescent Exponential Population tree prior
### Aliases: cep_tree_prior_to_xml_prior_distr

### ** Examples

 # <distribution id="posterior" spec="util.CompoundDistribution">
 #     <distribution id="prior" spec="util.CompoundDistribution">
 #       HERE, where the ID of the distribution is 'prior'
 #     </distribution>
 #     <distribution id="likelihood" ...>
 #     </distribution>
 # </distribution>



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("cep_tree_prior_to_xml_prior_distr", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("clock_model_to_xml_lh_distr")
### * clock_model_to_xml_lh_distr

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: clock_model_to_xml_lh_distr
### Title: Converts a clock model to the 'branchRateModel' section of the
###   XML as text
### Aliases: clock_model_to_xml_lh_distr

### ** Examples

 # <distribution id="posterior" spec="util.CompoundDistribution">
 #     <distribution id="prior" spec="util.CompoundDistribution">
 #     </distribution>
 #     <distribution id="likelihood" ...>
 #       HERE, where the ID of the distribution is 'likelihood'
 #     </distribution>
 # </distribution>



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("clock_model_to_xml_lh_distr", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("clock_model_to_xml_prior_distr")
### * clock_model_to_xml_prior_distr

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: clock_model_to_xml_prior_distr
### Title: Converts a clock model to the 'prior' section of the XML as text
### Aliases: clock_model_to_xml_prior_distr

### ** Examples

 # <distribution id="posterior" spec="util.CompoundDistribution">
 #     <distribution id="prior" spec="util.CompoundDistribution">
 #       HERE, where the ID of the distribution is 'prior'
 #     </distribution>
 #     <distribution id="likelihood" ...>
 #     </distribution>
 # </distribution>



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("clock_model_to_xml_prior_distr", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("clock_model_to_xml_tracelog")
### * clock_model_to_xml_tracelog

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: clock_model_to_xml_tracelog
### Title: Creates the clock model's XML for the tracelog section
### Aliases: clock_model_to_xml_tracelog

### ** Examples

# <logger id="tracelog" ...>
#'   # Here
# </logger>



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("clock_model_to_xml_tracelog", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("clock_models_to_xml_prior_distr")
### * clock_models_to_xml_prior_distr

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: clock_models_to_xml_prior_distr
### Title: Represent the clock models as XML
### Aliases: clock_models_to_xml_prior_distr

### ** Examples

 # <distribution id="posterior" spec="util.CompoundDistribution">
 #     <distribution id="prior" spec="util.CompoundDistribution">
 #       HERE, where the ID of the distribution is 'prior'
 #     </distribution>
 #     <distribution id="likelihood" ...>
 #     </distribution>
 # </distribution>



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("clock_models_to_xml_prior_distr", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("clock_models_to_xml_tracelog")
### * clock_models_to_xml_tracelog

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: clock_models_to_xml_tracelog
### Title: Creates the clock models' XML for the tracelog section
### Aliases: clock_models_to_xml_tracelog

### ** Examples

# <logger id="tracelog" ...>
#'   # Here
# </logger>



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("clock_models_to_xml_tracelog", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("create_alpha_param")
### * create_alpha_param

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: create_alpha_param
### Title: Create a parameter called alpha
### Aliases: create_alpha_param create_param_alpha

### ** Examples

  # Create the parameter
  alpha_param <- create_alpha_param()

  # Use the parameter in a distribution
  beta_distr <- create_beta_distr(
    alpha = alpha_param
  )

  # Use the distribution to create a BEAST2 input file
  create_beast2_input_file(
    input_filenames = get_fasta_filename(),
    "create_alpha_param.xml",
    tree_priors = create_yule_tree_prior(
      birth_rate_distr = beta_distr
    )
  )
  testit::assert(file.exists("create_alpha_param.xml"))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("create_alpha_param", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("create_bd_tree_prior")
### * create_bd_tree_prior

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: create_bd_tree_prior
### Title: Create a Birth-Death tree prior
### Aliases: create_bd_tree_prior create_tree_prior_bd

### ** Examples

  bd_tree_prior <- create_bd_tree_prior()

  create_beast2_input_file(
    input_filenames = get_fasta_filename(),
    "create_bd_tree_prior.xml",
    tree_priors = bd_tree_prior
  )
  testit::assert(file.exists("create_bd_tree_prior.xml"))

  bd_tree_prior_exp <- create_bd_tree_prior(
    birth_rate_distr = create_exp_distr()
  )

  create_beast2_input_file(
    input_filenames = get_fasta_filename(),
    "create_bd_tree_prior_exp.xml",
    tree_priors = bd_tree_prior_exp
  )
  testit::assert(file.exists("create_bd_tree_prior_exp.xml"))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("create_bd_tree_prior", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("create_beast2_input")
### * create_beast2_input

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: create_beast2_input
### Title: Create a BEAST2 XML input text
### Aliases: create_beast2_input

### ** Examples

  create_beast2_input_file(
    input_filenames = get_fasta_filename(),
    "my_beast.xml"
  )



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("create_beast2_input", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("create_beast2_input_1_12")
### * create_beast2_input_1_12

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: create_beast2_input_1_12
### Title: Create a BEAST2 XML input text, interface of v1.12
### Aliases: create_beast2_input_1_12

### ** Examples

  create_beast2_input_file_1_12(
    input_filenames = get_fasta_filename(),
    "my_beast.xml"
  )



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("create_beast2_input_1_12", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("create_beast2_input_distr")
### * create_beast2_input_distr

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: create_beast2_input_distr
### Title: Creates the distribution section of a BEAST2 XML parameter file.
### Aliases: create_beast2_input_distr

### ** Examples

 # <distribution id="posterior" spec="util.CompoundDistribution">
 #     <distribution id="prior" spec="util.CompoundDistribution">
 #       HERE, where the ID of the distribution is 'prior'
 #     </distribution>
 #     <distribution id="likelihood" ...>
 #     </distribution>
 # </distribution>



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("create_beast2_input_distr", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("create_beast2_input_distr_lh")
### * create_beast2_input_distr_lh

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: create_beast2_input_distr_lh
### Title: Creates the likelihood section in the distribution section of a
###   BEAST2 XML parameter file
### Aliases: create_beast2_input_distr_lh

### ** Examples

 # <distribution id="posterior" spec="util.CompoundDistribution">
 #     <distribution id="prior" spec="util.CompoundDistribution">
 #     </distribution>
 #     <distribution id="likelihood" ...>
 #       HERE, where the ID of the distribution is 'likelihood'
 #     </distribution>
 # </distribution>



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("create_beast2_input_distr_lh", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("create_beast2_input_distr_prior")
### * create_beast2_input_distr_prior

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: create_beast2_input_distr_prior
### Title: Creates the prior section in the distribution section of a
###   BEAST2 XML parameter file
### Aliases: create_beast2_input_distr_prior

### ** Examples

 # <distribution id="posterior" spec="util.CompoundDistribution">
 #     <distribution id="prior" spec="util.CompoundDistribution">
 #       HERE, where the ID of the distribution is 'prior'
 #     </distribution>
 #     <distribution id="likelihood" ...>
 #     </distribution>
 # </distribution>



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("create_beast2_input_distr_prior", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("create_beast2_input_file")
### * create_beast2_input_file

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: create_beast2_input_file
### Title: Create a BEAST2 input file
### Aliases: create_beast2_input_file

### ** Examples

  # The file created by beautier, a BEAST2 input file
  output_filename <- "create_beast2_input_file.xml"

  # Birth-Death tree prior, crown age is estimated
  create_beast2_input_file(
    get_fasta_filename(),
    output_filename
  )
  testthat::expect_true(file.exists(output_filename))

  # The file created by beautier, a BEAST2 input file
  output_filename_fixed <- "create_beast2_input_file_fixed.xml"

  # Birth-Death tree prior, crown age is fixed at 15 time units
  create_beast2_input_file(
    get_fasta_filename(),
    output_filename_fixed,
    posterior_crown_age = 15
  )
  testthat::expect_true(file.exists(output_filename_fixed))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("create_beast2_input_file", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("create_beast2_input_file_1_12")
### * create_beast2_input_file_1_12

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: create_beast2_input_file_1_12
### Title: Create a BEAST2 input file, interface of v1.12. This interface
###   is obsoleted, use 'create_beast2_input_file' instead
### Aliases: create_beast2_input_file_1_12

### ** Examples

  # The file created by beautier, a BEAST2 input file
  output_filename_fixed <- "create_beast2_input_file_1_12.xml"

  # Birth-Death tree prior, crown age is fixed at 15 time units
  create_beast2_input_file_1_12(
    input_filenames = get_fasta_filename(),
    output_filename = output_filename_fixed,
    fixed_crown_ages = TRUE,
    initial_phylogenies = beautier::fasta_to_phylo(
      fasta_filename = get_fasta_filename(),
      crown_age = 15)
  )
  testthat::expect_true(file.exists(output_filename_fixed))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("create_beast2_input_file_1_12", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("create_beta_distr")
### * create_beta_distr

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: create_beta_distr
### Title: Create a beta distribution
### Aliases: create_beta_distr create_distr_beta

### ** Examples

  beta_distr <- create_beta_distr()

  create_beast2_input_file(
    input_filenames = get_fasta_filename(),
    "create_beta_distr.xml",
    tree_priors = create_yule_tree_prior(
      birth_rate_distr = beta_distr
    )
  )
  testit::assert(file.exists("create_beta_distr.xml"))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("create_beta_distr", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("create_beta_param")
### * create_beta_param

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: create_beta_param
### Title: Create a parameter called beta
### Aliases: create_beta_param create_param_beta

### ** Examples

  # Create the parameter
  beta_param <- create_beta_param()

  # Use the parameter in a distribution
  gamma_distr <- create_gamma_distr(
    beta = beta_param
  )

  # Use the distribution to create a BEAST2 input file
  create_beast2_input_file(
    input_filenames = get_fasta_filename(),
    "create_beta_param.xml",
    tree_priors = create_yule_tree_prior(
      birth_rate_distr = gamma_distr
    )
  )
  testit::assert(file.exists("create_beta_param.xml"))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("create_beta_param", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("create_cbs_tree_prior")
### * create_cbs_tree_prior

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: create_cbs_tree_prior
### Title: Create a Coalescent Bayesian Skyline tree prior
### Aliases: create_cbs_tree_prior create_tree_prior_cbs

### ** Examples

  cbs_tree_prior <- create_cbs_tree_prior()

  create_beast2_input_file(
    input_filenames = get_beautier_path("test_output_6.fas"),
    "create_cbs_tree_prior.xml",
    tree_priors = cbs_tree_prior
  )
  testit::assert(file.exists("create_cbs_tree_prior.xml"))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("create_cbs_tree_prior", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("create_ccp_tree_prior")
### * create_ccp_tree_prior

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: create_ccp_tree_prior
### Title: Create a Coalescent Constant Population tree prior
### Aliases: create_ccp_tree_prior create_tree_prior_ccp

### ** Examples

  ccp_tree_prior <- create_ccp_tree_prior()

  create_beast2_input_file(
    input_filenames = get_fasta_filename(),
    "create_ccp_tree_prior.xml",
    tree_priors = ccp_tree_prior
  )
  testit::assert(file.exists("create_ccp_tree_prior.xml"))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("create_ccp_tree_prior", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("create_cep_tree_prior")
### * create_cep_tree_prior

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: create_cep_tree_prior
### Title: Create a Coalescent Exponential Population tree prior
### Aliases: create_cep_tree_prior create_tree_prior_cep

### ** Examples

  cep_tree_prior <- create_cep_tree_prior()

  create_beast2_input_file(
    input_filenames = get_fasta_filename(),
    "create_cep_tree_prior.xml",
    tree_priors = cep_tree_prior
  )
  testit::assert(file.exists("create_cep_tree_prior.xml"))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("create_cep_tree_prior", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("create_clock_model")
### * create_clock_model

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: create_clock_model
### Title: General function to create a clock model
### Aliases: create_clock_model

### ** Examples

  rln_clock_model <- create_rln_clock_model()

  create_beast2_input_file(
    get_fasta_filename(),
    "create_clock_model_rln.xml",
    clock_models = rln_clock_model
  )
  testit::assert(file.exists("create_clock_model_rln.xml"))

  strict_clock_model <- create_strict_clock_model()

  create_beast2_input_file(
    get_fasta_filename(),
    "create_clock_model_strict.xml",
    clock_models = strict_clock_model
  )
  testit::assert(file.exists("create_clock_model_strict.xml"))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("create_clock_model", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("create_clock_models")
### * create_clock_models

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: create_clock_models
### Title: Creates all supported clock models, which is just a list of the
###   types returned by 'create_rln_clock_model', and
###   'create_strict_clock_model'
### Aliases: create_clock_models

### ** Examples

 clock_models <- beautier:::create_clock_models()
 testit::assert(beautier:::is_rln_clock_model(clock_models[[1]]))
 testit::assert(beautier:::is_strict_clock_model(clock_models[[2]]))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("create_clock_models", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("create_clock_rate_param")
### * create_clock_rate_param

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: create_clock_rate_param
### Title: Create a parameter called 'clock_rate', as needed by
###   'create_strict_clock_model'
### Aliases: create_clock_rate_param create_param_clock_rate

### ** Examples

  clock_rate_param <- create_clock_rate_param(
    id = "anthus_aco", estimate = FALSE, value = 1.0
  )

  # Use the parameter in a clock model
  strict_clock_model <- create_strict_clock_model(
    clock_rate_param = clock_rate_param
  )

  # Use the distribution to create a BEAST2 input file
  create_beast2_input_file(
    input_filenames = get_fasta_filename(),
    "create_clock_rate_param.xml",
    clock_models = strict_clock_model
  )
  testit::assert(file.exists("create_clock_rate_param.xml"))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("create_clock_rate_param", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("create_distr")
### * create_distr

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: create_distr
### Title: General function to create a distribution.
### Aliases: create_distr

### ** Examples

  # Use any distribution
  distr <- create_beta_distr()

  create_beast2_input_file(
    input_filenames = get_fasta_filename(),
    "create_distr.xml",
    tree_priors = create_yule_tree_prior(
      birth_rate_distr = distr
    )
  )
  testit::assert(file.exists("create_distr.xml"))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("create_distr", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("create_exp_distr")
### * create_exp_distr

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: create_exp_distr
### Title: Create an exponential distribution
### Aliases: create_exp_distr create_distr_exp

### ** Examples

  exp_distr <- create_exp_distr()

  create_beast2_input_file(
    input_filenames = get_fasta_filename(),
    "my_beast.xml",
    tree_priors = create_yule_tree_prior(
      birth_rate_distr = exp_distr
    )
  )
  testit::assert(file.exists("my_beast.xml"))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("create_exp_distr", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("create_gamma_distr")
### * create_gamma_distr

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: create_gamma_distr
### Title: Create a gamma distribution
### Aliases: create_gamma_distr create_distr_gamma

### ** Examples

  gamma_distr <- create_gamma_distr(
     alpha = create_alpha_param(value = 0.05),
     beta = create_beta_param(value = 10.0)
  )

  gtr_site_model <- create_gtr_site_model(
    rate_ac_prior_distr = gamma_distr
  )

  create_beast2_input_file(
    input_filenames = get_fasta_filename(),
    "create_gamma_distr.xml",
    site_model = gtr_site_model
  )
  testit::assert(file.exists("create_gamma_distr.xml"))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("create_gamma_distr", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("create_gamma_site_model")
### * create_gamma_site_model

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: create_gamma_site_model
### Title: Create a gamma site model, part of a site model
### Aliases: create_gamma_site_model

### ** Examples

  gamma_site_model <- create_gamma_site_model(prop_invariant = 0.5)

  site_model <- create_hky_site_model(gamma_site_model = gamma_site_model)

  create_beast2_input_file(
    get_fasta_filename(),
    "create_gamma_site_model.xml",
    site_model = site_model
  )
  testit::assert(file.exists("create_gamma_site_model.xml"))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("create_gamma_site_model", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("create_gtr_site_model")
### * create_gtr_site_model

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: create_gtr_site_model
### Title: Create a GTR site model
### Aliases: create_gtr_site_model create_site_model_gtr

### ** Examples

  gtr_site_model <- create_gtr_site_model()

  create_beast2_input_file(
    input_filenames = get_fasta_filename(),
    "create_gtr_site_model.xml",
    site_models = gtr_site_model
  )
  testit::assert(file.exists("create_gtr_site_model.xml"))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("create_gtr_site_model", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("create_hky_site_model")
### * create_hky_site_model

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: create_hky_site_model
### Title: Create an HKY site model
### Aliases: create_hky_site_model create_site_model_hky

### ** Examples

 hky_site_model <- create_hky_site_model()

 create_beast2_input_file(
   input_filenames = get_fasta_filename(),
   "beast.xml",
   site_models = hky_site_model
 )



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("create_hky_site_model", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("create_inv_gamma_distr")
### * create_inv_gamma_distr

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: create_inv_gamma_distr
### Title: Create an inverse gamma distribution
### Aliases: create_inv_gamma_distr create_distr_inv_gamma

### ** Examples

  inv_gamma_distr <- create_inv_gamma_distr()

  create_beast2_input_file(
    input_filenames = get_fasta_filename(),
    "my_beast.xml",
    tree_priors = create_yule_tree_prior(
      birth_rate_distr = inv_gamma_distr
    )
  )
  testit::assert(file.exists("my_beast.xml"))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("create_inv_gamma_distr", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("create_jc69_site_model")
### * create_jc69_site_model

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: create_jc69_site_model
### Title: Create a JC69 site model
### Aliases: create_jc69_site_model create_site_model_jc69

### ** Examples

 jc69_site_model <- create_jc69_site_model()

 create_beast2_input_file(
   input_filenames = get_fasta_filename(),
   "beast.xml",
   site_models = jc69_site_model
 )



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("create_jc69_site_model", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("create_jc69_site_models")
### * create_jc69_site_models

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: create_jc69_site_models
### Title: Creates a JC69_site_model for each ID
### Aliases: create_jc69_site_models

### ** Examples

  fasta_filenames <- get_beautier_paths(
    c("anthus_aco.fas", "anthus_nd2.fas")
  )
  site_models <- create_jc69_site_models(c("anthus_aco", "anthus_nd2"))
  create_beast2_input_file(
    fasta_filenames,
    "create_jc69_site_models.xml",
    site_models = site_models
  )
  testit::assert(file.exists("create_jc69_site_models.xml"))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("create_jc69_site_models", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("create_lambda_param")
### * create_lambda_param

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: create_lambda_param
### Title: Create a parameter called lambda
### Aliases: create_lambda_param create_param_lambda

### ** Examples

  # Create the parameter
  lambda_param <- create_lambda_param()

  # Use the parameter in a distribution
  poisson_distr <- create_poisson_distr(
    lambda = lambda_param
  )

  # Use the distribution to create a BEAST2 input file
  create_beast2_input_file(
    input_filenames = get_fasta_filename(),
    "create_lambda_param.xml",
    tree_priors = create_yule_tree_prior(
      birth_rate_distr = poisson_distr
    )
  )
  testit::assert(file.exists("create_lambda_param.xml"))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("create_lambda_param", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("create_laplace_distr")
### * create_laplace_distr

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: create_laplace_distr
### Title: Create a Laplace distribution
### Aliases: create_laplace_distr create_distr_laplace

### ** Examples

  laplace_distr <- create_laplace_distr()

  create_beast2_input_file(
    input_filenames = get_fasta_filename(),
    "create_laplace_distr.xml",
    tree_priors = create_yule_tree_prior(
      birth_rate_distr = laplace_distr
    )
  )
  testit::assert(file.exists("create_laplace_distr.xml"))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("create_laplace_distr", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("create_log_normal_distr")
### * create_log_normal_distr

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: create_log_normal_distr
### Title: Create a log-normal distribution
### Aliases: create_log_normal_distr create_distr_log_normal

### ** Examples

  log_normal_distr <- create_log_normal_distr()

  create_beast2_input_file(
    input_filenames = get_fasta_filename(),
    "my_beast.xml",
    tree_priors = create_yule_tree_prior(
      birth_rate_distr = log_normal_distr
    )
  )
  testit::assert(file.exists("my_beast.xml"))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("create_log_normal_distr", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("create_m_param")
### * create_m_param

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: create_m_param
### Title: Create a parameter called m
### Aliases: create_m_param create_param_m

### ** Examples

  # Create the parameter
  m_param <- create_m_param()

  # Use the parameter in a distribution
  log_normal_distr <- create_log_normal_distr(
    m = m_param
  )

  # Use the distribution to create a BEAST2 input file
  create_beast2_input_file(
    input_filenames = get_fasta_filename(),
    "create_m_param.xml",
    tree_priors = create_yule_tree_prior(
      birth_rate_distr = log_normal_distr
    )
  )
  testit::assert(file.exists("create_m_param.xml"))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("create_m_param", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("create_mcmc")
### * create_mcmc

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: create_mcmc
### Title: Function to create the MCMC options, as in the BEAUti MCMC tab.
### Aliases: create_mcmc

### ** Examples

  mcmc <- create_mcmc(chain_length = 50000)

  create_beast2_input_file(
    get_fasta_filename(),
    "create_mcmc.xml",
    mcmc = mcmc
  )
  testit::assert(file.exists("create_mcmc.xml"))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("create_mcmc", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("create_mean_param")
### * create_mean_param

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: create_mean_param
### Title: Create a parameter called mean
### Aliases: create_mean_param create_param_mean

### ** Examples

  # Create the parameter
  mean_param <- create_mean_param(value = 1.0)

  # Use the parameter in a distribution
  exp_distr <- create_exp_distr(
    mean = mean_param
  )

  # Use the distribution to create a BEAST2 input file
  create_beast2_input_file(
    input_filenames = get_fasta_filename(),
    "create_mean_param.xml",
    tree_priors = create_yule_tree_prior(
      birth_rate_distr = exp_distr
    )
  )
  testit::assert(file.exists("create_mean_param.xml"))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("create_mean_param", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("create_misc_options")
### * create_misc_options

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: create_misc_options
### Title: Function to create a misc_options, containing miscellaneous
###   options to fine-tune the created BEAST2 XML file. Whatever option
###   chosen here, the created XML file will be valid.
### Aliases: create_misc_options

### ** Examples

  misc_options <- create_misc_options(nucleotides_uppercase = TRUE)
  xml <- create_beast2_input(
    get_fasta_filename(),
    misc_options = misc_options
  )
  testit::assert(is.character(xml))
  testit::assert(length(xml) > 1)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("create_misc_options", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("create_mu_param")
### * create_mu_param

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: create_mu_param
### Title: Create a parameter called mu
### Aliases: create_mu_param create_param_mu

### ** Examples

  # Create the parameter
  mu_param <- create_mu_param()

  # Use the parameter in a distribution
  laplace_distr <- create_laplace_distr(
    mu = mu_param
  )

  # Use the distribution to create a BEAST2 input file
  create_beast2_input_file(
    input_filenames = get_fasta_filename(),
    "create_mu_param.xml",
    tree_priors = create_yule_tree_prior(
      birth_rate_distr = laplace_distr
    )
  )
  testit::assert(file.exists("create_mu_param.xml"))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("create_mu_param", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("create_normal_distr")
### * create_normal_distr

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: create_normal_distr
### Title: Create an normal distribution
### Aliases: create_normal_distr create_distr_normal

### ** Examples

  normal_distr <- create_normal_distr()

  create_beast2_input_file(
    input_filenames = get_fasta_filename(),
    "my_beast.xml",
    tree_priors = create_yule_tree_prior(
      birth_rate_distr = normal_distr
    )
  )
  testit::assert(file.exists("my_beast.xml"))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("create_normal_distr", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("create_one_div_x_distr")
### * create_one_div_x_distr

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: create_one_div_x_distr
### Title: Create a 1/x distribution
### Aliases: create_one_div_x_distr create_distr_one_div_x

### ** Examples

  one_div_x_distr <- create_one_div_x_distr()

  create_beast2_input_file(
    input_filenames = get_fasta_filename(),
    "my_beast.xml",
    tree_priors = create_yule_tree_prior(
      birth_rate_distr = one_div_x_distr
    )
  )
  testit::assert(file.exists("my_beast.xml"))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("create_one_div_x_distr", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("create_param")
### * create_param

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: create_param
### Title: General function to create a parameter.
### Aliases: create_param

### ** Examples

  # Create an alpha parameter
  alpha_param <- create_alpha_param()

  # Use the parameter in a distribution
  beta_distr <- create_beta_distr(
    alpha = alpha_param
  )

  # Use the distribution to create a BEAST2 input file
  create_beast2_input_file(
    input_filenames = get_fasta_filename(),
    "create_alpha_param.xml",
    tree_priors = create_yule_tree_prior(
      birth_rate_distr = beta_distr
    )
  )
  testit::assert(file.exists("create_alpha_param.xml"))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("create_param", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("create_poisson_distr")
### * create_poisson_distr

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: create_poisson_distr
### Title: Create a Poisson distribution
### Aliases: create_poisson_distr create_distr_poisson

### ** Examples

  poisson_distr <- create_poisson_distr()

  create_beast2_input_file(
    input_filenames = get_fasta_filename(),
    "create_poisson_distr.xml",
    tree_priors = create_yule_tree_prior(
      birth_rate_distr = poisson_distr
    )
  )
  testit::assert(file.exists("create_poisson_distr.xml"))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("create_poisson_distr", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("create_rate_ac_param")
### * create_rate_ac_param

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: create_rate_ac_param
### Title: Create a parameter called 'rate AC'
### Aliases: create_rate_ac_param create_param_rate_ac

### ** Examples

  # Create parameter
  rate_ac_param <- create_rate_ac_param(value = 1, estimate = FALSE)

  # Use the parameter to create a BEAST2 input file
  create_beast2_input_file(
    input_filenames = get_fasta_filename(),
    "create_rate_ac_param.xml",
    site_models = create_gtr_site_model(
      rate_ac_param = rate_ac_param
    )
  )
  testit::assert(file.exists("create_rate_ac_param.xml"))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("create_rate_ac_param", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("create_rate_ag_param")
### * create_rate_ag_param

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: create_rate_ag_param
### Title: Create a parameter called 'rate AG'
### Aliases: create_rate_ag_param create_param_rate_ag

### ** Examples

  # Create parameter
  rate_ag_param <- create_rate_ag_param(value = 1, estimate = FALSE)

  # Use the parameter to create a BEAST2 input file
  create_beast2_input_file(
    input_filenames = get_fasta_filename(),
    "create_rate_ag_param.xml",
    site_models = create_gtr_site_model(
      rate_ag_param = rate_ag_param
    )
  )
  testit::assert(file.exists("create_rate_ag_param.xml"))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("create_rate_ag_param", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("create_rate_at_param")
### * create_rate_at_param

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: create_rate_at_param
### Title: Create a parameter called 'rate AT'
### Aliases: create_rate_at_param create_param_rate_at

### ** Examples

  # Create parameter
  rate_at_param <- create_rate_at_param(value = 1, estimate = FALSE)

  # Use the parameter to create a BEAST2 input file
  create_beast2_input_file(
    input_filenames = get_fasta_filename(),
    "create_rate_at_param.xml",
    site_models = create_gtr_site_model(
      rate_at_param = rate_at_param
    )
  )
  testit::assert(file.exists("create_rate_at_param.xml"))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("create_rate_at_param", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("create_rate_cg_param")
### * create_rate_cg_param

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: create_rate_cg_param
### Title: Create a parameter called 'rate CG'
### Aliases: create_rate_cg_param create_param_rate_cg

### ** Examples

  # Create parameter
  rate_cg_param <- create_rate_cg_param(value = 1, estimate = FALSE)

  # Use the parameter to create a BEAST2 input file
  create_beast2_input_file(
    input_filenames = get_fasta_filename(),
    "create_rate_cg_param.xml",
    site_models = create_gtr_site_model(
      rate_cg_param = rate_cg_param
    )
  )
  testit::assert(file.exists("create_rate_cg_param.xml"))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("create_rate_cg_param", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("create_rate_ct_param")
### * create_rate_ct_param

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: create_rate_ct_param
### Title: Create a parameter called 'rate CT'
### Aliases: create_rate_ct_param create_param_rate_ct

### ** Examples

  # Create parameter
  rate_ct_param <- create_rate_ct_param(value = 1, estimate = FALSE)

  # Use the parameter to create a BEAST2 input file
  create_beast2_input_file(
    input_filenames = get_fasta_filename(),
    "create_rate_ct_param.xml",
    site_models = create_gtr_site_model(
      rate_ct_param = rate_ct_param
    )
  )
  testit::assert(file.exists("create_rate_ct_param.xml"))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("create_rate_ct_param", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("create_rate_gt_param")
### * create_rate_gt_param

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: create_rate_gt_param
### Title: Create a parameter called 'rate GT'
### Aliases: create_rate_gt_param create_param_rate_gt

### ** Examples

  # Create parameter
  rate_gt_param <- create_rate_gt_param(value = 1, estimate = FALSE)

  # Use the parameter to create a BEAST2 input file
  create_beast2_input_file(
    input_filenames = get_fasta_filename(),
    "create_rate_gt_param.xml",
    site_models = create_gtr_site_model(
      rate_gt_param = rate_gt_param
    )
  )
  testit::assert(file.exists("create_rate_gt_param.xml"))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("create_rate_gt_param", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("create_rln_clock_model")
### * create_rln_clock_model

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: create_rln_clock_model
### Title: Create a relaxed log-normal clock model
### Aliases: create_rln_clock_model create_clock_model_rln

### ** Examples

  rln_clock_model <- create_rln_clock_model()

  create_beast2_input_file(
    get_fasta_filename(),
    "create_rln_clock_model.xml",
    clock_models = rln_clock_model
  )
  testit::assert(file.exists("create_rln_clock_model.xml"))

  rln_clock_model_exp <- create_rln_clock_model(
    mean_rate_prior_distr = create_exp_distr()
  )

  create_beast2_input_file(
    get_fasta_filename(),
    "create_rln_clock_model_exp.xml",
    clock_models = rln_clock_model_exp
  )
  testit::assert(file.exists("create_rln_clock_model_exp.xml"))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("create_rln_clock_model", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("create_s_param")
### * create_s_param

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: create_s_param
### Title: Create a parameter called s
### Aliases: create_s_param create_param_s

### ** Examples

  # Create the parameter
  s_param <- create_s_param()

  # Use the parameter in a distribution
  log_normal_distr <- create_log_normal_distr(
    s = s_param
  )

  # Use the distribution to create a BEAST2 input file
  create_beast2_input_file(
    input_filenames = get_fasta_filename(),
    "create_s_param.xml",
    tree_priors = create_yule_tree_prior(
      birth_rate_distr = log_normal_distr
    )
  )
  testit::assert(file.exists("create_s_param.xml"))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("create_s_param", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("create_scale_param")
### * create_scale_param

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: create_scale_param
### Title: Create a parameter called scale
### Aliases: create_scale_param create_param_scale

### ** Examples

  # Create the parameter
  scale_param <- create_scale_param()

  # Use the parameter in a distribution
  laplace_distr <- create_laplace_distr(
    scale = scale_param
  )

  # Use the distribution to create a BEAST2 input file
  create_beast2_input_file(
    input_filenames = get_fasta_filename(),
    "create_scale_param.xml",
    tree_priors = create_yule_tree_prior(
      birth_rate_distr = laplace_distr
    )
  )
  testit::assert(file.exists("create_scale_param.xml"))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("create_scale_param", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("create_sigma_param")
### * create_sigma_param

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: create_sigma_param
### Title: Create a parameter called sigma
### Aliases: create_sigma_param create_param_sigma

### ** Examples

  # Create the parameter
  sigma_param <- create_sigma_param()

  # Use the parameter in a distribution
  normal_distr <- create_normal_distr(
    sigma = sigma_param
  )

  # Use the distribution to create a BEAST2 input file
  create_beast2_input_file(
    input_filenames = get_fasta_filename(),
    "create_sigma_param.xml",
    tree_priors = create_yule_tree_prior(
      birth_rate_distr = normal_distr
    )
  )
  testit::assert(file.exists("create_sigma_param.xml"))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("create_sigma_param", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("create_site_model")
### * create_site_model

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: create_site_model
### Title: General function to create a site model.
### Aliases: create_site_model

### ** Examples

  # GTR
  create_beast2_input_file(
    input_filenames = get_fasta_filename(),
    output_filename = "example_gtr.xml",
    site_models = create_gtr_site_model()
  )
  testthat::expect_true(file.exists("example_gtr.xml"))

  # HKY
  create_beast2_input_file(
    input_filenames = get_fasta_filename(),
    output_filename = "example_hky.xml",
    site_models = create_hky_site_model()
  )
  testthat::expect_true(file.exists("example_hky.xml"))

  # JC69
  create_beast2_input_file(
    input_filenames = get_fasta_filename(),
    output_filename = "example_jc69.xml",
    site_models = create_jc69_site_model()
  )
  testthat::expect_true(file.exists("example_jc69.xml"))

  # TN93
  create_beast2_input_file(
    input_filenames = get_fasta_filename(),
    output_filename = "example_tn93.xml",
    site_models = create_tn93_site_model()
  )
  testthat::expect_true(file.exists("example_tn93.xml"))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("create_site_model", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("create_site_models")
### * create_site_models

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: create_site_models
### Title: Creates all supported site models which is just a list of the
###   types returned by 'create_gtr_site_model', 'create_hky_site_model',
###   'create_jc69_site_model' and 'create_tn93_site_model'
### Aliases: create_site_models

### ** Examples

 site_models <- beautier:::create_site_models()
 testit::assert(beautier:::is_gtr_site_model(site_models[[1]]))
 testit::assert(beautier:::is_hky_site_model(site_models[[2]]))
 testit::assert(beautier:::is_jc69_site_model(site_models[[3]]))
 testit::assert(beautier:::is_tn93_site_model(site_models[[4]]))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("create_site_models", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("create_strict_clock_model")
### * create_strict_clock_model

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: create_strict_clock_model
### Title: Create a strict clock model
### Aliases: create_strict_clock_model create_clock_model_strict

### ** Examples

  strict_clock_model <- create_strict_clock_model()

  create_beast2_input_file(
    get_fasta_filename(),
    "create_strict_clock_model.xml",
    clock_models = strict_clock_model
  )
  testit::assert(file.exists("create_strict_clock_model.xml"))

  strict_clock_model_gamma <- create_strict_clock_model(
    clock_rate_distr = create_gamma_distr()
  )

  create_beast2_input_file(
    get_fasta_filename(),
    "create_strict_clock_model_gamma.xml",
    clock_models = strict_clock_model_gamma
  )
  testit::assert(file.exists("create_strict_clock_model_gamma.xml"))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("create_strict_clock_model", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("create_strict_clock_models")
### * create_strict_clock_models

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: create_strict_clock_models
### Title: Creates n strict clock_models
### Aliases: create_strict_clock_models

### ** Examples

  fasta_filenames <- get_beautier_paths(
    c("anthus_aco.fas", "anthus_nd2.fas")
  )
  clock_models <- create_strict_clock_models(
    ids = get_ids(fasta_filenames)
  )

  create_beast2_input_file(
    fasta_filenames,
    "create_strict_clock_models.xml",
    clock_models = clock_models
  )
  testit::assert(file.exists("create_strict_clock_models.xml"))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("create_strict_clock_models", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("create_tn93_site_model")
### * create_tn93_site_model

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: create_tn93_site_model
### Title: Create a TN93 site model
### Aliases: create_tn93_site_model create_site_model_tn93

### ** Examples

 tn93_site_model <- create_tn93_site_model()

 create_beast2_input_file(
   input_filenames = get_fasta_filename(),
   "beast.xml",
   site_models = tn93_site_model
 )



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("create_tn93_site_model", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("create_tree_prior")
### * create_tree_prior

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: create_tree_prior
### Title: Internal function to create a tree prior
### Aliases: create_tree_prior

### ** Examples

  create_beast2_input_file(
    input_filenames = get_fasta_filename(),
    "create_tree_prior_bd.xml",
    tree_priors = create_bd_tree_prior()
  )
  testit::assert(file.exists("create_tree_prior_bd.xml"))

  create_beast2_input_file(
    input_filenames = get_beautier_path("test_output_6.fas"),
    "create_tree_prior_cbs.xml",
    tree_priors = create_cbs_tree_prior()
  )
  testit::assert(file.exists("create_tree_prior_cbs.xml"))

  create_beast2_input_file(
    input_filenames = get_fasta_filename(),
    "create_tree_prior_ccp.xml",
    tree_priors = create_ccp_tree_prior()
  )
  testit::assert(file.exists("create_tree_prior_ccp.xml"))

  create_beast2_input_file(
    input_filenames = get_fasta_filename(),
    "create_tree_prior_cep.xml",
    tree_priors = create_cep_tree_prior()
  )
  testit::assert(file.exists("create_tree_prior_cep.xml"))

  create_beast2_input_file(
    input_filenames = get_fasta_filename(),
    "create_tree_prior_yule.xml",
    tree_priors = create_yule_tree_prior()
  )
  testit::assert(file.exists("create_tree_prior_yule.xml"))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("create_tree_prior", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("create_tree_priors")
### * create_tree_priors

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: create_tree_priors
### Title: Creates all supported tree priors, which is just a list of the
###   types returned by 'create_bd_tree_prior', 'create_cbs_tree_prior',
###   'create_ccp_tree_prior', 'create_cep_tree_prior' and
###   'create_yule_tree_prior'
### Aliases: create_tree_priors

### ** Examples

  tree_priors <- beautier:::create_tree_priors()
  testit::assert(beautier:::is_bd_tree_prior(tree_priors[[1]]))
  testit::assert(beautier:::is_cbs_tree_prior(tree_priors[[2]]))
  testit::assert(beautier:::is_ccp_tree_prior(tree_priors[[3]]))
  testit::assert(beautier:::is_cep_tree_prior(tree_priors[[4]]))
  testit::assert(beautier:::is_yule_tree_prior(tree_priors[[5]]))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("create_tree_priors", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("create_uniform_distr")
### * create_uniform_distr

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: create_uniform_distr
### Title: Create a uniform distribution
### Aliases: create_uniform_distr create_distr_uniform

### ** Examples

  uniform_distr <- create_uniform_distr()

  create_beast2_input_file(
    input_filenames = get_fasta_filename(),
    "create_uniform_distr.xml",
    tree_priors = create_yule_tree_prior(
      birth_rate_distr = uniform_distr
    )
  )
  testit::assert(file.exists("create_uniform_distr.xml"))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("create_uniform_distr", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("create_yule_tree_prior")
### * create_yule_tree_prior

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: create_yule_tree_prior
### Title: Create a Yule tree prior
### Aliases: create_yule_tree_prior create_tree_prior_yule

### ** Examples

  yule_tree_prior <- create_yule_tree_prior()

  create_beast2_input_file(
    input_filenames = get_fasta_filename(),
    "create_yule_tree_prior.xml",
    tree_priors = yule_tree_prior
  )
  testit::assert(file.exists("create_yule_tree_prior.xml"))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("create_yule_tree_prior", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("create_yule_tree_priors")
### * create_yule_tree_priors

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: create_yule_tree_priors
### Title: Creates n Yule tree priors
### Aliases: create_yule_tree_priors

### ** Examples

  fasta_filenames <- get_beautier_paths(
    c("anthus_aco.fas", "anthus_nd2.fas")
  )

  tree_priors <- create_yule_tree_priors(ids = get_ids(fasta_filenames))

  create_beast2_input_file(
    fasta_filenames,
    "create_yule_tree_priors.xml",
    tree_priors = tree_priors
  )
  testit::assert(file.exists("create_yule_tree_priors.xml"))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("create_yule_tree_priors", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("distr_to_xml")
### * distr_to_xml

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: distr_to_xml
### Title: Converts a distribution to XML
### Aliases: distr_to_xml

### ** Examples

  xml <- beautier:::distr_to_xml(create_uniform_distr(id = 1))
  testit::assert(is.character(xml))
  testit::assert(length(xml) == 1)
  testit::assert(nchar(xml) > 1)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("distr_to_xml", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("fasta_to_phylo")
### * fasta_to_phylo

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: fasta_to_phylo
### Title: Create a random phylogeny, with the same taxa names as the FASTA
###   file and the desired crown age
### Aliases: fasta_to_phylo

### ** Examples

  # Create a random phylogy, with
  # - the same taxa names as the FASTA file
  # - the desired crown age
  fasta_filename <- get_fasta_filename()
  initial_phylogeny <- fasta_to_phylo(
    fasta_filename,
    crown_age = 15
   )

  # Crown age fixed to the crown age of the phylogeny
  # Note: prefer create_beast2_input_file
  create_beast2_input_file_1_12(
    input_filenames = fasta_filename,
    "fasta_to_phylo.xml",
    fixed_crown_age = TRUE,
    initial_phylogenies = initial_phylogeny
  )
  testthat::expect_true(file.exists("fasta_to_phylo.xml"))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("fasta_to_phylo", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("fastas_to_phylos")
### * fastas_to_phylos

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: fastas_to_phylos
### Title: Create a random phylogeny, with the same taxa names as the FASTA
###   file and the desired crown age
### Aliases: fastas_to_phylos

### ** Examples

  # Create two random phylogies, with
  # - the same taxa names as the FASTA files
  # - the desired crown age
  fasta_filenames <- get_beautier_paths(
    c("anthus_aco.fas", "anthus_nd2.fas")
  )
  initial_phylogenies <- fastas_to_phylos(
    fasta_filenames,
    crown_age = 15
   )

  # Crown age fixed to the crown age of the phylogeny
  create_beast2_input_file_1_12(
    input_filenames = fasta_filenames,
    "fastas_to_phylos.xml",
    fixed_crown_ages = c(TRUE, TRUE),
    initial_phylogenies = initial_phylogenies
  )
  testthat::expect_true(file.exists("fastas_to_phylos.xml"))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("fastas_to_phylos", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("get_alignment_id")
### * get_alignment_id

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: get_alignment_id
### Title: Get the alignment ID from a FASTA filename
### Aliases: get_alignment_id

### ** Examples

  created <- get_alignment_id("/home/homer/anthus_aco_sub.fas")
  expected <- "anthus_aco_sub"
  testit::assert(created == expected)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get_alignment_id", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("get_beautier_path")
### * get_beautier_path

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: get_beautier_path
### Title: Get the full path of a file in the 'inst/extdata' folder
### Aliases: get_beautier_path

### ** Examples

  testit::assert(is.character(get_beautier_path("test_output_0.fas")))
  testit::assert(is.character(get_beautier_path("anthus_aco.fas")))
  testit::assert(is.character(get_beautier_path("anthus_nd2.fas")))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get_beautier_path", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("get_beautier_paths")
### * get_beautier_paths

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: get_beautier_paths
### Title: Get the full paths of files in the 'inst/extdata' folder
### Aliases: get_beautier_paths

### ** Examples

  testit::assert(
    length(
      get_beautier_paths(
        c("test_output_0.fas", "anthus_aco.fas", "anthus_nd2.fas")
      )
     ) == 3
   )



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get_beautier_paths", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("get_clock_model_name")
### * get_clock_model_name

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: get_clock_model_name
### Title: Get the BEAUti name for a clock model
### Aliases: get_clock_model_name

### ** Examples

  strict <- create_strict_clock_model()
  testit::assert(beautier:::get_clock_model_name(strict) == "StrictClock")
  rln <- create_rln_clock_model()
  testit::assert(beautier:::get_clock_model_name(rln) == "RelaxedClock")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get_clock_model_name", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("get_clock_model_names")
### * get_clock_model_names

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: get_clock_model_names
### Title: Get the clock model names
### Aliases: get_clock_model_names

### ** Examples

  names <- beautier:::get_clock_model_names()
  testit::assert("relaxed_log_normal" %in% names)
  testit::assert("strict" %in% names)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get_clock_model_names", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("get_distr_n_params")
### * get_distr_n_params

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: get_distr_n_params
### Title: Get the number of parameters a distribution uses
### Aliases: get_distr_n_params

### ** Examples

  testit::assert(
    beautier:::get_distr_n_params(create_beta_distr()) == 2
  )
  testit::assert(
    beautier:::get_distr_n_params(create_exp_distr()) == 1
  )
  testit::assert(
    beautier:::get_distr_n_params(create_gamma_distr()) == 2
  )
  testit::assert(
    beautier:::get_distr_n_params(create_inv_gamma_distr()) == 2
  )
  testit::assert(
    beautier:::get_distr_n_params(create_laplace_distr()) == 2
  )
  testit::assert(
    beautier:::get_distr_n_params(create_log_normal_distr()) == 2
  )
  testit::assert(
    beautier:::get_distr_n_params(create_normal_distr()) == 2
  )
  testit::assert(
    beautier:::get_distr_n_params(create_one_div_x_distr()) == 0
  )
  testit::assert(
    beautier:::get_distr_n_params(create_poisson_distr()) == 1
  )
  testit::assert(
    beautier:::get_distr_n_params(create_uniform_distr()) == 0
  )



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get_distr_n_params", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("get_distr_names")
### * get_distr_names

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: get_distr_names
### Title: Get the distribution names
### Aliases: get_distr_names

### ** Examples

  names <- beautier:::get_distr_names()
  testit::assert("uniform" %in% names)
  testit::assert("normal" %in% names)
  testit::assert("one_div_x" %in% names)
  testit::assert("log_normal" %in% names)
  testit::assert("exponential" %in% names)
  testit::assert("gamma" %in% names)
  testit::assert("beta" %in% names)
  testit::assert("laplace" %in% names)
  testit::assert("inv_gamma" %in% names)
  testit::assert("poisson" %in% names)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get_distr_names", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("get_fasta_filename")
### * get_fasta_filename

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: get_fasta_filename
### Title: Get the path of a FASTA file used in testing
### Aliases: get_fasta_filename

### ** Examples

  filename <- beautier::get_fasta_filename()
  testit::assert(file.exists(filename))

  create_beast2_input_file(
    input_filenames = filename,
    "my_beast.xml"
  )



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get_fasta_filename", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("get_file_base_sans_ext")
### * get_file_base_sans_ext

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: get_file_base_sans_ext
### Title: Get the filename's base without extension
### Aliases: get_file_base_sans_ext

### ** Examples

  testit::assert(
    beautier:::get_file_base_sans_ext("/home/richel/test.txt")
    == "test"
 )



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get_file_base_sans_ext", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("get_first_clock_model_index")
### * get_first_clock_model_index

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: get_first_clock_model_index
### Title: Get the index of a clock model with a list of clock models
### Aliases: get_first_clock_model_index

### ** Examples

  a <- create_strict_clock_model(id = 1)
  b <- create_rln_clock_model(id = 2)
  ab <- list(a, b)
  testit::assert(beautier:::get_first_clock_model_index(a, ab) == 1)
  testit::assert(beautier:::get_first_clock_model_index(b, ab) == 2)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get_first_clock_model_index", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("get_freq_equilibrium_names")
### * get_freq_equilibrium_names

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: get_freq_equilibrium_names
### Title: Returns valid values for the 'freq_equilibrium' argument
### Aliases: get_freq_equilibrium_names

### ** Examples

  names <- beautier:::get_freq_equilibrium_names()
  testit::assert("estimated" %in% names)
  testit::assert("empirical" %in% names)
  testit::assert("all_equal" %in% names)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get_freq_equilibrium_names", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("get_id")
### * get_id

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: get_id
### Title: Conclude the ID from a FASTA filename
### Aliases: get_id

### ** Examples

  testit::assert(get_id("anthus_aco.fas") == "anthus_aco")
  testit::assert(
    get_id("anthus_aco.fas", capitalize_first_char_id = TRUE)
    == "Anthus_aco")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get_id", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("get_ids")
### * get_ids

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: get_ids
### Title: Conclude the IDs from one or more FASTA filenames
### Aliases: get_ids

### ** Examples

  # Basic usage
  testit::assert(get_ids(c("a.fas", "b.fas")) == c("a", "b"))

  # Usage to create a BEAST2 XML file
  fasta_filenames <- get_beautier_paths(
    c("anthus_aco.fas", "anthus_nd2.fas")
  )
  clock_models <- create_strict_clock_models(
    ids = get_ids(fasta_filenames)
  )

  create_beast2_input_file(
    fasta_filenames,
    "create_strict_clock_models.xml",
    clock_models = clock_models
  )
  testit::assert(file.exists("create_strict_clock_models.xml"))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get_ids", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("get_operator_id_pre")
### * get_operator_id_pre

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: get_operator_id_pre
### Title: Get the prefix of operator IDs
### Aliases: get_operator_id_pre

### ** Examples

  bd_pre <- beautier:::get_operator_id_pre(
    tree_prior = create_bd_tree_prior()
  )
  testthat::expect_equal(bd_pre, "BirthDeath")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get_operator_id_pre", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("get_param_names")
### * get_param_names

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: get_param_names
### Title: Get the parameter names
### Aliases: get_param_names

### ** Examples

  names <- beautier:::get_param_names()
  testit::assert("alpha" %in% names)
  testit::assert("beta" %in% names)
  testit::assert("clock_rate" %in% names)
  testit::assert("kappa_1" %in% names)
  testit::assert("kappa_2" %in% names)
  testit::assert("lambda" %in% names)
  testit::assert("m" %in% names)
  testit::assert("mean" %in% names)
  testit::assert("mu" %in% names)
  testit::assert("rate_ac" %in% names)
  testit::assert("rate_ag" %in% names)
  testit::assert("rate_at" %in% names)
  testit::assert("rate_cg" %in% names)
  testit::assert("rate_ct" %in% names)
  testit::assert("rate_gt" %in% names)
  testit::assert("s" %in% names)
  testit::assert("scale" %in% names)
  testit::assert("sigma" %in% names)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get_param_names", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("get_phylo_crown_age")
### * get_phylo_crown_age

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: get_phylo_crown_age
### Title: Obtain the crown age of a phylony
### Aliases: get_phylo_crown_age

### ** Examples

  phylogeny <- ape::read.tree(text = "(a:15,b:15):1;")
  created <- beautier:::get_phylo_crown_age(phylogeny = phylogeny)
  testit::assert(created == 15)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get_phylo_crown_age", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("get_site_model_n_distrs")
### * get_site_model_n_distrs

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: get_site_model_n_distrs
### Title: Get the number of distributions a site model has
### Aliases: get_site_model_n_distrs

### ** Examples

  # gamma site model, rates AC, AG, AT, CG and GT
  testit::assert(
    beautier:::get_site_model_n_distrs(create_gtr_site_model()) == 6
  )

  # gamma site model, kappa
  testit::assert(
    beautier:::get_site_model_n_distrs(create_hky_site_model()) == 2
  )

  # gamma site model
  testit::assert(
    beautier:::get_site_model_n_distrs(create_jc69_site_model()) == 1
  )

  # gamma site model, kappa 1 and kappa 2
  testit::assert(
    beautier:::get_site_model_n_distrs(create_tn93_site_model()) == 3
  )



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get_site_model_n_distrs", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("get_site_model_n_params")
### * get_site_model_n_params

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: get_site_model_n_params
### Title: Get the number of distributions a site model has
### Aliases: get_site_model_n_params

### ** Examples

  testit::assert(
    beautier:::get_site_model_n_params(create_gtr_site_model()) == 11
  )
  testit::assert(
    beautier:::get_site_model_n_params(create_hky_site_model()) == 3
  )
  testit::assert(
    beautier:::get_site_model_n_params(create_jc69_site_model()) == 1
  )
  testit::assert(
    beautier:::get_site_model_n_params(create_tn93_site_model()) == 5
  )



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get_site_model_n_params", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("get_site_model_names")
### * get_site_model_names

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: get_site_model_names
### Title: Get the site models' names
### Aliases: get_site_model_names

### ** Examples

  names <- beautier:::get_site_model_names()
  testit::assert("JC69" %in% names)
  testit::assert("HKY" %in% names)
  testit::assert("TN93" %in% names)
  testit::assert("GTR" %in% names)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get_site_model_names", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("get_site_models_n_distrs")
### * get_site_models_n_distrs

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: get_site_models_n_distrs
### Title: Get the number of distributions a site model has
### Aliases: get_site_models_n_distrs

### ** Examples

  testit::assert(
    beautier:::get_site_models_n_distrs(list(create_gtr_site_model())) == 6
  )
  testit::assert(
    beautier:::get_site_models_n_distrs(list(create_hky_site_model())) == 2
  )
  testit::assert(
    beautier:::get_site_models_n_distrs(list(create_jc69_site_model())) == 1
  )
  testit::assert(
    beautier:::get_site_models_n_distrs(list(create_tn93_site_model())) == 3
  )



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get_site_models_n_distrs", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("get_site_models_n_params")
### * get_site_models_n_params

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: get_site_models_n_params
### Title: Get the number of distributions one or more site models have
### Aliases: get_site_models_n_params

### ** Examples

  testit::assert(
    beautier:::get_site_models_n_params(list(create_gtr_site_model())) == 11
  )
  testit::assert(
    beautier:::get_site_models_n_params(list(create_hky_site_model())) == 3
  )
  testit::assert(
    beautier:::get_site_models_n_params(list(create_jc69_site_model())) == 1
  )
  testit::assert(
    beautier:::get_site_models_n_params(list(create_tn93_site_model())) == 5
  )



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get_site_models_n_params", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("get_taxa_names")
### * get_taxa_names

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: get_taxa_names
### Title: Extract the names of taxa from a file
### Aliases: get_taxa_names

### ** Examples

  created <- get_taxa_names(get_beautier_path("anthus_aco_sub.fas"))
  expected <- c(
    "61430_aco", "626029_aco", "630116_aco", "630210_aco", "B25702_aco"
   )
  testit::assert(created == expected)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get_taxa_names", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("get_tree_prior_n_distrs")
### * get_tree_prior_n_distrs

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: get_tree_prior_n_distrs
### Title: Get the number of distributions a tree prior has
### Aliases: get_tree_prior_n_distrs

### ** Examples

 # birth_rate_distr and death_rate_distr
 testit::assert(
   beautier:::get_tree_prior_n_distrs(create_bd_tree_prior()) == 2
 )

 # none
 testit::assert(
   beautier:::get_tree_prior_n_distrs(create_cbs_tree_prior()) == 0
 )

 # pop_size_distr
 testit::assert(
   beautier:::get_tree_prior_n_distrs(create_ccp_tree_prior()) == 1
 )

 # pop_size_distr and growth_rate_distr
 testit::assert(
   beautier:::get_tree_prior_n_distrs(create_cep_tree_prior()) == 2
 )

 # birth_rate_distr
 testit::assert(
   beautier:::get_tree_prior_n_distrs(create_yule_tree_prior()) == 1
 )



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get_tree_prior_n_distrs", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("get_tree_prior_n_params")
### * get_tree_prior_n_params

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: get_tree_prior_n_params
### Title: Get the number of parameters a tree prior has
### Aliases: get_tree_prior_n_params

### ** Examples

 # birth_rate_distr is uniform, which has zero parameters
 # death_rate_distr is uniform, which has zero parameters
 testit::assert(
   beautier:::get_tree_prior_n_params(create_bd_tree_prior()) == 0
 )

 # no distributions, no parameters
 testit::assert(
   beautier:::get_tree_prior_n_params(create_cbs_tree_prior()) == 0
 )

 # pop_size_distr is 1/x, which has zero parameters
 testit::assert(
   beautier:::get_tree_prior_n_params(create_ccp_tree_prior()) == 0
 )

 # pop_size_distr is 1/x, which has zero parameters
 # growth_rate_distr is Laplace, which has two parameters
 testit::assert(
   beautier:::get_tree_prior_n_params(create_cep_tree_prior()) == 2
 )

 # birth_rate_distr is uniform, which has zero parameters
 testit::assert(
   beautier:::get_tree_prior_n_params(create_yule_tree_prior()) == 0
 )



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get_tree_prior_n_params", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("get_tree_prior_names")
### * get_tree_prior_names

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: get_tree_prior_names
### Title: Get the tree prior names
### Aliases: get_tree_prior_names

### ** Examples

  names <- beautier:::get_tree_prior_names()
  testit::assert("birth_death" %in% names)
  testit::assert("coalescent_bayesian_skyline" %in% names)
  testit::assert("coalescent_constant_population" %in% names)
  testit::assert("coalescent_exp_population" %in% names)
  testit::assert("yule" %in% names)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get_tree_prior_names", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("get_tree_priors_n_distrs")
### * get_tree_priors_n_distrs

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: get_tree_priors_n_distrs
### Title: Get the number of distributions a tree prior has
### Aliases: get_tree_priors_n_distrs

### ** Examples

 testit::assert(
   beautier:::get_tree_priors_n_distrs(
     list(
       create_bd_tree_prior(), # has two distributions
       create_ccp_tree_prior() # has one distribution
     )
   ) == 3)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get_tree_priors_n_distrs", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("get_tree_priors_n_params")
### * get_tree_priors_n_params

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: get_tree_priors_n_params
### Title: Get the number of parameters a list of tree priors has
### Aliases: get_tree_priors_n_params

### ** Examples

 testit::assert(
   beautier:::get_tree_priors_n_params(
     list(
       create_bd_tree_prior(), # zero
       create_cep_tree_prior() # two
     )
   ) == 2
 )



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get_tree_priors_n_params", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("get_xml_closing_tag")
### * get_xml_closing_tag

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: get_xml_closing_tag
### Title: Get the XML closing tag
### Aliases: get_xml_closing_tag

### ** Examples

  testit::assert(
    beautier:::get_xml_closing_tag("<my_tag text=something></my_tag>")
    == "my_tag"
  )
  testit::assert(
    is.na(
      beautier:::get_xml_closing_tag("<my_tag text=something/>")
    )
  )
  testit::assert(is.na(beautier:::get_xml_closing_tag("no_xml")))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get_xml_closing_tag", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("get_xml_opening_tag")
### * get_xml_opening_tag

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: get_xml_opening_tag
### Title: Get the XML opening tag
### Aliases: get_xml_opening_tag

### ** Examples

  testit::assert(
    beautier:::get_xml_opening_tag("<my_tag text=something/>")
    == "my_tag"
  )
  testit::assert(is.na(beautier:::get_xml_opening_tag("no_xml")))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get_xml_opening_tag", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("has_xml_short_closing_tag")
### * has_xml_short_closing_tag

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: has_xml_short_closing_tag
### Title: Is an XML closing tag with short closing text at the end of the
###   text?
### Aliases: has_xml_short_closing_tag

### ** Examples

  testit::assert(beautier:::has_xml_short_closing_tag("<my_tag id=1/>"))
  testit::assert(
    !beautier:::has_xml_short_closing_tag(
      "<my_tag id=1>text</my_tag>"
    )
  )



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("has_xml_short_closing_tag", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("init_gtr_site_model")
### * init_gtr_site_model

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: init_gtr_site_model
### Title: Initializes a GTR site model
### Aliases: init_gtr_site_model

### ** Examples

  gtr_site_model <- create_gtr_site_model()
  testit::assert(!beautier:::is_init_gtr_site_model(gtr_site_model))
  gtr_site_model <- beautier:::init_gtr_site_model(gtr_site_model)
  testit::assert(beautier:::is_init_gtr_site_model(gtr_site_model))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("init_gtr_site_model", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("init_hky_site_model")
### * init_hky_site_model

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: init_hky_site_model
### Title: Initializes an HKY site model
### Aliases: init_hky_site_model

### ** Examples

  hky_site_model <- create_hky_site_model()
  testit::assert(!beautier:::is_init_hky_site_model(hky_site_model))
  hky_site_model <- beautier:::init_hky_site_model(hky_site_model)
  testit::assert(beautier:::is_init_hky_site_model(hky_site_model))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("init_hky_site_model", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("init_jc69_site_model")
### * init_jc69_site_model

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: init_jc69_site_model
### Title: Initializes a JC69 site model
### Aliases: init_jc69_site_model

### ** Examples

  hky_site_model <- create_hky_site_model()
  testit::assert(!beautier:::is_init_hky_site_model(hky_site_model))
  hky_site_model <- beautier:::init_hky_site_model(hky_site_model)
  testit::assert(beautier:::is_init_hky_site_model(hky_site_model))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("init_jc69_site_model", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("init_strict_clock_model")
### * init_strict_clock_model

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: init_strict_clock_model
### Title: Initializes a strict clock model
### Aliases: init_strict_clock_model

### ** Examples

  strict_clock_model <- create_strict_clock_model()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("init_strict_clock_model", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("init_tn93_site_model")
### * init_tn93_site_model

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: init_tn93_site_model
### Title: Initializes a TN93 site model
### Aliases: init_tn93_site_model

### ** Examples

  tn93_site_model <- create_tn93_site_model()
  testit::assert(!beautier:::is_init_tn93_site_model(tn93_site_model))
  tn93_site_model <- beautier:::init_tn93_site_model(tn93_site_model)
  testit::assert(beautier:::is_init_tn93_site_model(tn93_site_model))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("init_tn93_site_model", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("is_clock_model_name")
### * is_clock_model_name

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: is_clock_model_name
### Title: Determines if the name is a valid clock model name
### Aliases: is_clock_model_name

### ** Examples

  testit::assert(beautier:::is_clock_model_name("relaxed_log_normal"))
  testit::assert(beautier:::is_clock_model_name("strict"))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("is_clock_model_name", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("is_distr_name")
### * is_distr_name

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: is_distr_name
### Title: Determines if the name is a valid distribution name
### Aliases: is_distr_name

### ** Examples

  testit::assert(beautier:::is_distr_name("uniform"))
  testit::assert(beautier:::is_distr_name("normal"))
  testit::assert(beautier:::is_distr_name("one_div_x"))
  testit::assert(beautier:::is_distr_name("log_normal"))
  testit::assert(beautier:::is_distr_name("exponential"))
  testit::assert(beautier:::is_distr_name("gamma"))
  testit::assert(beautier:::is_distr_name("beta"))
  testit::assert(beautier:::is_distr_name("laplace"))
  testit::assert(beautier:::is_distr_name("inv_gamma"))
  testit::assert(beautier:::is_distr_name("poisson"))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("is_distr_name", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("is_freq_equilibrium_name")
### * is_freq_equilibrium_name

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: is_freq_equilibrium_name
### Title: Checks if 'name' is a valid 'freq_equilibrium' argument value
### Aliases: is_freq_equilibrium_name

### ** Examples

  testit::assert(beautier:::is_freq_equilibrium_name("estimated"))
  testit::assert(beautier:::is_freq_equilibrium_name("empirical"))
  testit::assert(beautier:::is_freq_equilibrium_name("all_equal"))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("is_freq_equilibrium_name", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("is_gamma_site_model")
### * is_gamma_site_model

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: is_gamma_site_model
### Title: Is object x a gamma site model?
### Aliases: is_gamma_site_model

### ** Examples

  gamma_site_model <- create_gamma_site_model()
  testit::assert(beautier:::is_gamma_site_model(gamma_site_model))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("is_gamma_site_model", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("is_gtr_site_model")
### * is_gtr_site_model

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: is_gtr_site_model
### Title: Determine if the object is a valid GTR site model, as created by
###   'create_gtr_site_model'
### Aliases: is_gtr_site_model

### ** Examples

  gtr_site_model <- create_gtr_site_model()
  testit::assert(beautier:::is_gtr_site_model(gtr_site_model))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("is_gtr_site_model", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("is_hky_site_model")
### * is_hky_site_model

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: is_hky_site_model
### Title: Determine if the object is a valid HKY site model, as created by
###   'create_hky_site_model'
### Aliases: is_hky_site_model

### ** Examples

  hky_site_model <- create_hky_site_model()
  testit::assert(beautier:::is_hky_site_model(hky_site_model))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("is_hky_site_model", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("is_init_gtr_site_model")
### * is_init_gtr_site_model

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: is_init_gtr_site_model
### Title: Determine if x is an initialized GTR site model as created by
###   'create_gtr_site_model'
### Aliases: is_init_gtr_site_model

### ** Examples

  gtr_site_model <- create_gtr_site_model()
  testit::assert(!beautier:::is_init_gtr_site_model(gtr_site_model))
  gtr_site_model <- beautier:::init_gtr_site_model(gtr_site_model)
  testit::assert(beautier:::is_init_gtr_site_model(gtr_site_model))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("is_init_gtr_site_model", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("is_init_hky_site_model")
### * is_init_hky_site_model

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: is_init_hky_site_model
### Title: Determine if x is an initialized hky site model as created by
###   'create_hky_site_model'
### Aliases: is_init_hky_site_model

### ** Examples

  hky_site_model <- create_hky_site_model()
  testit::assert(!beautier:::is_init_hky_site_model(hky_site_model))
  hky_site_model <- beautier:::init_hky_site_model(hky_site_model)
  testit::assert(beautier:::is_init_hky_site_model(hky_site_model))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("is_init_hky_site_model", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("is_init_jc69_site_model")
### * is_init_jc69_site_model

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: is_init_jc69_site_model
### Title: Determine if x is an initialized JC69 site model as created by
###   'create_jc69_site_model'
### Aliases: is_init_jc69_site_model

### ** Examples

  jc69_site_model <- create_jc69_site_model()
  testit::assert(!beautier:::is_init_jc69_site_model(jc69_site_model))
  jc69_site_model <- beautier:::init_jc69_site_model(jc69_site_model)
  testit::assert(beautier:::is_init_jc69_site_model(jc69_site_model))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("is_init_jc69_site_model", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("is_init_tn93_site_model")
### * is_init_tn93_site_model

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: is_init_tn93_site_model
### Title: Determine if x is an initialized tn93 site model as created by
###   'create_tn93_site_model'
### Aliases: is_init_tn93_site_model

### ** Examples

  tn93_site_model <- create_tn93_site_model()
  testit::assert(!beautier:::is_init_tn93_site_model(tn93_site_model))
  tn93_site_model <- beautier:::init_tn93_site_model(tn93_site_model)
  testit::assert(beautier:::is_init_tn93_site_model(tn93_site_model))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("is_init_tn93_site_model", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("is_jc69_site_model")
### * is_jc69_site_model

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: is_jc69_site_model
### Title: Determine if the object is a valid JC69 site model
### Aliases: is_jc69_site_model

### ** Examples

  jc69_site_model <- create_jc69_site_model()
  testit::assert(beautier:::is_jc69_site_model(jc69_site_model))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("is_jc69_site_model", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("is_kappa_1_param")
### * is_kappa_1_param

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: is_kappa_1_param
### Title: Determine if the object is a valid kappa 1 parameter
### Aliases: is_kappa_1_param

### ** Examples

  kappa_1_param <- create_kappa_1_param()
  testit::assert(beautier:::is_kappa_1_param(kappa_1_param))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("is_kappa_1_param", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("is_kappa_2_param")
### * is_kappa_2_param

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: is_kappa_2_param
### Title: Determine if the object is a valid kappa 2 parameter
### Aliases: is_kappa_2_param

### ** Examples

  kappa_2_param <- create_kappa_2_param()
  testit::assert(beautier:::is_kappa_2_param(kappa_2_param))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("is_kappa_2_param", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("is_lambda_param")
### * is_lambda_param

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: is_lambda_param
### Title: Determine if the object is a valid lambda parameter
### Aliases: is_lambda_param

### ** Examples

  lambda_param <- create_lambda_param()
  testit::assert(beautier:::is_lambda_param(lambda_param))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("is_lambda_param", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("is_laplace_distr")
### * is_laplace_distr

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: is_laplace_distr
### Title: Determine if the object is a valid laplace distribution, as
###   created by 'create_laplace_distr'
### Aliases: is_laplace_distr

### ** Examples

  laplace_distr <- create_laplace_distr()
  testit::assert(beautier:::is_laplace_distr(laplace_distr))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("is_laplace_distr", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("is_log_normal_distr")
### * is_log_normal_distr

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: is_log_normal_distr
### Title: Determine if the object is a valid log-normal distribution, as
###   created by 'create_log_normal_distr'
### Aliases: is_log_normal_distr

### ** Examples

  log_normal_distr <- create_log_normal_distr()

  input_fasta_filename <- beautier::get_beautier_path("anthus_aco.fas")
  create_beast2_input_file(
    input_filenames = input_fasta_filename,
    "my_beast.xml",
    tree_priors = create_yule_tree_prior(
      birth_rate_distr = log_normal_distr
    )
  )
  testit::assert(file.exists("my_beast.xml"))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("is_log_normal_distr", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("is_misc_options")
### * is_misc_options

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: is_misc_options
### Title: Determine if the object is a valid misc_options
### Aliases: is_misc_options

### ** Examples

  misc_options <- create_misc_options()
  testit::assert(beautier:::is_misc_options(misc_options))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("is_misc_options", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("is_mu_param")
### * is_mu_param

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: is_mu_param
### Title: Determine if the object is a valid mu parameter
### Aliases: is_mu_param

### ** Examples

  mu_param <- create_mu_param()
  testit::assert(beautier:::is_mu_param(mu_param))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("is_mu_param", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("is_one_na")
### * is_one_na

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: is_one_na
### Title: Determines if x is one NA
### Aliases: is_one_na

### ** Examples

  testit::assert(beautier:::is_one_na(NA))
  testit::assert(!beautier:::is_one_na(NULL))
  testit::assert(!beautier:::is_one_na(42))
  testit::assert(!beautier:::is_one_na("Hello"))
  testit::assert(!beautier:::is_one_na(3.14))
  testit::assert(!beautier:::is_one_na(c(NA, NA)))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("is_one_na", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("is_param_name")
### * is_param_name

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: is_param_name
### Title: Determines if the name is a valid parameter name
### Aliases: is_param_name

### ** Examples

  testit::assert(beautier:::is_param_name("alpha"))
  testit::assert(beautier:::is_param_name("beta"))
  testit::assert(beautier:::is_param_name("clock_rate"))
  testit::assert(beautier:::is_param_name("kappa_1"))
  testit::assert(beautier:::is_param_name("kappa_2"))
  testit::assert(beautier:::is_param_name("lambda"))
  testit::assert(beautier:::is_param_name("m"))
  testit::assert(beautier:::is_param_name("mean"))
  testit::assert(beautier:::is_param_name("mu"))
  testit::assert(beautier:::is_param_name("rate_ac"))
  testit::assert(beautier:::is_param_name("rate_ag"))
  testit::assert(beautier:::is_param_name("rate_at"))
  testit::assert(beautier:::is_param_name("rate_cg"))
  testit::assert(beautier:::is_param_name("rate_ct"))
  testit::assert(beautier:::is_param_name("rate_gt"))
  testit::assert(beautier:::is_param_name("s"))
  testit::assert(beautier:::is_param_name("scale"))
  testit::assert(beautier:::is_param_name("sigma"))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("is_param_name", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("is_rate_ac_param")
### * is_rate_ac_param

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: is_rate_ac_param
### Title: Determine if the object is a valid 'rate AC' parameter
### Aliases: is_rate_ac_param

### ** Examples

  rate_ac_param <- create_rate_ac_param()
  testit::assert(beautier:::is_rate_ac_param(rate_ac_param))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("is_rate_ac_param", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("is_rate_ag_param")
### * is_rate_ag_param

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: is_rate_ag_param
### Title: Determine if the object is a valid 'rate AG' parameter
### Aliases: is_rate_ag_param

### ** Examples

  rate_ag_param <- create_rate_ag_param()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("is_rate_ag_param", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("is_rate_at_param")
### * is_rate_at_param

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: is_rate_at_param
### Title: Determine if the object is a valid 'rate AT' parameter
### Aliases: is_rate_at_param

### ** Examples

  rate_at_param <- create_rate_at_param()
  testit::assert(beautier:::is_rate_at_param(rate_at_param))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("is_rate_at_param", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("is_rate_cg_param")
### * is_rate_cg_param

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: is_rate_cg_param
### Title: Determine if the object is a valid 'rate CG' parameter
### Aliases: is_rate_cg_param

### ** Examples

  rate_cg_param <- create_rate_cg_param()
  testit::assert(beautier:::is_rate_cg_param(rate_cg_param))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("is_rate_cg_param", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("is_rate_ct_param")
### * is_rate_ct_param

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: is_rate_ct_param
### Title: Determine if the object is a valid 'rate CT' parameter
### Aliases: is_rate_ct_param

### ** Examples

  rate_ct_param <- create_rate_ct_param()
  testit::assert(beautier:::is_rate_ct_param(rate_ct_param))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("is_rate_ct_param", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("is_rate_gt_param")
### * is_rate_gt_param

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: is_rate_gt_param
### Title: Determine if the object is a valid 'rate GT' parameter
### Aliases: is_rate_gt_param

### ** Examples

  rate_gt_param <- create_rate_gt_param()
  testit::assert(beautier:::is_rate_gt_param(rate_gt_param))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("is_rate_gt_param", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("is_rln_clock_model")
### * is_rln_clock_model

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: is_rln_clock_model
### Title: Determine if the object is a valid relaxed log normal clock
###   model
### Aliases: is_rln_clock_model

### ** Examples

  rln_clock_model <- create_rln_clock_model()
  testit::assert(beautier:::is_rln_clock_model(rln_clock_model))

  strict_clock_model <- create_strict_clock_model()
  testit::assert(beautier:::is_strict_clock_model(strict_clock_model))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("is_rln_clock_model", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("is_site_model")
### * is_site_model

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: is_site_model
### Title: Determine if the object is a valid site_model
### Aliases: is_site_model

### ** Examples

  # site models
  testit::assert(beautier:::is_site_model(create_gtr_site_model()))
  testit::assert(beautier:::is_site_model(create_hky_site_model()))
  testit::assert(beautier:::is_site_model(create_jc69_site_model()))
  testit::assert(beautier:::is_site_model(create_tn93_site_model()))

  # other models
  testit::assert(!beautier:::is_site_model(create_strict_clock_model()))
  testit::assert(!beautier:::is_site_model(create_bd_tree_prior()))
  testit::assert(!beautier:::is_site_model(create_mcmc()))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("is_site_model", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("is_site_model_name")
### * is_site_model_name

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: is_site_model_name
### Title: Determines if the name is a valid site_model name
### Aliases: is_site_model_name

### ** Examples

  testit::assert(beautier:::is_site_model_name("JC69"))
  testit::assert(beautier:::is_site_model_name("HKY"))
  testit::assert(beautier:::is_site_model_name("TN93"))
  testit::assert(beautier:::is_site_model_name("GTR"))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("is_site_model_name", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("is_strict_clock_model")
### * is_strict_clock_model

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: is_strict_clock_model
### Title: Determine if the object is a valid strict clock model, as
###   returned by 'create_strict_clock_model'
### Aliases: is_strict_clock_model

### ** Examples

  strict_clock_model <- create_strict_clock_model()

  # rln: Relaxed Log-Normal
  rln_clock_model <- create_rln_clock_model()
  testit::assert(!beautier:::is_strict_clock_model(rln_clock_model))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("is_strict_clock_model", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("is_tn93_site_model")
### * is_tn93_site_model

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: is_tn93_site_model
### Title: Determine if the object is a valid TN93 site model,
### Aliases: is_tn93_site_model

### ** Examples

 create_beast2_input_file(
   input_filenames = get_fasta_filename(),
   "beast.xml",
   site_models = create_tn93_site_model()
 )



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("is_tn93_site_model", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("is_tree_prior_name")
### * is_tree_prior_name

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: is_tree_prior_name
### Title: Determines if the name is a valid tree prior name
### Aliases: is_tree_prior_name

### ** Examples

  testit::assert(
    beautier:::is_tree_prior_name("birth_death")
  )
  testit::assert(
    beautier:::is_tree_prior_name("coalescent_bayesian_skyline")
  )
  testit::assert(
    beautier:::is_tree_prior_name("coalescent_constant_population")
  )
  testit::assert(
    beautier:::is_tree_prior_name("coalescent_exp_population")
  )
  testit::assert(
    beautier:::is_tree_prior_name("yule")
  )



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("is_tree_prior_name", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("mcmc_to_xml_run")
### * mcmc_to_xml_run

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: mcmc_to_xml_run
### Title: Converts an MCMC object to the run section's XML
### Aliases: mcmc_to_xml_run

### ** Examples

  xml <- beautier:::mcmc_to_xml_run(create_mcmc())
  testit::assert(xml ==
    "<run id=\"mcmc\" spec=\"MCMC\" chainLength=\"10000000\">"
  )



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("mcmc_to_xml_run", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("mrca_prior_to_xml_lh_distr")
### * mrca_prior_to_xml_lh_distr

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: mrca_prior_to_xml_lh_distr
### Title: Converts an MRCA prior to the 'branchRateModel' section of the
###   XML as text
### Aliases: mrca_prior_to_xml_lh_distr

### ** Examples

 # <distribution id="posterior" spec="util.CompoundDistribution">
 #     <distribution id="prior" spec="util.CompoundDistribution">
 #     </distribution>
 #     <distribution id="likelihood" ...>
 #       HERE, where the ID of the distribution is 'likelihood'
 #     </distribution>
 # </distribution>



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("mrca_prior_to_xml_lh_distr", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("mrca_prior_to_xml_prior_distr")
### * mrca_prior_to_xml_prior_distr

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: mrca_prior_to_xml_prior_distr
### Title: Creates the distribution section in the prior section of the
###   distribution section of a BEAST2 XML parameter file. These lines
###   start with '<distribution id='
### Aliases: mrca_prior_to_xml_prior_distr

### ** Examples

 # <distribution id="posterior" spec="util.CompoundDistribution">
 #     <distribution id="prior" spec="util.CompoundDistribution">
 #       HERE, where the ID of the distribution is 'prior'
 #     </distribution>
 #     <distribution id="likelihood" ...>
 #     </distribution>
 # </distribution>



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("mrca_prior_to_xml_prior_distr", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("mrca_prior_to_xml_taxonset")
### * mrca_prior_to_xml_taxonset

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: mrca_prior_to_xml_taxonset
### Title: Creates the distribution section in the prior section of the
###   distribution section of a BEAST2 XML parameter file. These lines
###   start with '<distribution id='
### Aliases: mrca_prior_to_xml_taxonset

### ** Examples

  # <taxonset id="all" spec="TaxonSet">
  #     <taxon id="626029_aco" spec="Taxon"/>
  #     <taxon id="630116_aco" spec="Taxon"/>
  #     <taxon id="630210_aco" spec="Taxon"/>
  #     <taxon id="B25702_aco" spec="Taxon"/>
  #     <taxon id="61430_aco" spec="Taxon"/>
  # </taxonset>



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("mrca_prior_to_xml_taxonset", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("mrca_prior_to_xml_tracelog")
### * mrca_prior_to_xml_tracelog

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: mrca_prior_to_xml_tracelog
### Title: Creates the mrca prior's XML for the tracelog section
### Aliases: mrca_prior_to_xml_tracelog

### ** Examples

# <logger id="tracelog" ...>
#'   # Here
# </logger>



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("mrca_prior_to_xml_tracelog", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("mrca_priors_to_xml_prior_distr")
### * mrca_priors_to_xml_prior_distr

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: mrca_priors_to_xml_prior_distr
### Title: Creates the distribution section in the prior section of the
###   distribution section of a BEAST2 XML parameter file. These lines
###   start with '<distribution id='
### Aliases: mrca_priors_to_xml_prior_distr

### ** Examples

 # <distribution id="posterior" spec="util.CompoundDistribution">
 #     <distribution id="prior" spec="util.CompoundDistribution">
 #       HERE, where the ID of the distribution is 'prior'
 #     </distribution>
 #     <distribution id="likelihood" ...>
 #     </distribution>
 # </distribution>



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("mrca_priors_to_xml_prior_distr", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("mrca_priors_to_xml_tracelog")
### * mrca_priors_to_xml_tracelog

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: mrca_priors_to_xml_tracelog
### Title: Creates the mrca priors' XML for the tracelog section
### Aliases: mrca_priors_to_xml_tracelog

### ** Examples

# <logger id="tracelog" ...>
#'   # Here
# </logger>



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("mrca_priors_to_xml_tracelog", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("parameter_to_xml")
### * parameter_to_xml

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: parameter_to_xml
### Title: Converts a parameter to XML
### Aliases: parameter_to_xml

### ** Examples

  xml <- beautier:::parameter_to_xml(create_alpha_param(id = 1))
  testit::assert(length(xml) == 1)
  testit::assert(nchar(xml) > 1)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("parameter_to_xml", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("site_model_to_xml_lh_distr")
### * site_model_to_xml_lh_distr

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: site_model_to_xml_lh_distr
### Title: Converts a site model to XML, used in the 'siteModel' section
### Aliases: site_model_to_xml_lh_distr

### ** Examples

 # <distribution id="posterior" spec="util.CompoundDistribution">
 #     <distribution id="prior" spec="util.CompoundDistribution">
 #     </distribution>
 #     <distribution id="likelihood" ...>
 #       HERE, where the ID of the distribution is 'likelihood'
 #     </distribution>
 # </distribution>



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("site_model_to_xml_lh_distr", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("site_model_to_xml_tracelog")
### * site_model_to_xml_tracelog

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: site_model_to_xml_tracelog
### Title: Creates the site model's XML for the tracelog section
### Aliases: site_model_to_xml_tracelog

### ** Examples

# <logger id="tracelog" ...>
#'   # Here
# </logger>



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("site_model_to_xml_tracelog", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("site_models_to_xml_prior_distr")
### * site_models_to_xml_prior_distr

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: site_models_to_xml_prior_distr
### Title: Represent the site models as XML
### Aliases: site_models_to_xml_prior_distr

### ** Examples

 # <distribution id="posterior" spec="util.CompoundDistribution">
 #     <distribution id="prior" spec="util.CompoundDistribution">
 #       HERE, where the ID of the distribution is 'prior'
 #     </distribution>
 #     <distribution id="likelihood" ...>
 #     </distribution>
 # </distribution>



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("site_models_to_xml_prior_distr", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("site_models_to_xml_tracelog")
### * site_models_to_xml_tracelog

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: site_models_to_xml_tracelog
### Title: Creates the site models' XML for the tracelog section
### Aliases: site_models_to_xml_tracelog

### ** Examples

# <logger id="tracelog" ...>
#'   # Here
# </logger>



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("site_models_to_xml_tracelog", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("tree_models_to_xml_tracelog")
### * tree_models_to_xml_tracelog

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: tree_models_to_xml_tracelog
### Title: Creates the tree models' XML for the tracelog section
### Aliases: tree_models_to_xml_tracelog

### ** Examples

# <logger id="tracelog" ...>
#'   # Here
# </logger>



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("tree_models_to_xml_tracelog", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("tree_prior_to_xml_prior_distr")
### * tree_prior_to_xml_prior_distr

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: tree_prior_to_xml_prior_distr
### Title: Creates the distribution section in the prior section of the
###   distribution section of a BEAST2 XML parameter file. These lines
###   start with '<distribution id='
### Aliases: tree_prior_to_xml_prior_distr

### ** Examples

 # <distribution id="posterior" spec="util.CompoundDistribution">
 #     <distribution id="prior" spec="util.CompoundDistribution">
 #       HERE, where the ID of the distribution is 'prior'
 #     </distribution>
 #     <distribution id="likelihood" ...>
 #     </distribution>
 # </distribution>



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("tree_prior_to_xml_prior_distr", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("tree_prior_to_xml_tracelog")
### * tree_prior_to_xml_tracelog

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: tree_prior_to_xml_tracelog
### Title: Creates the tree prior's XML for the tracelog section
### Aliases: tree_prior_to_xml_tracelog

### ** Examples

# <logger id="tracelog" ...>
#'   # Here
# </logger>



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("tree_prior_to_xml_tracelog", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("tree_priors_to_xml_prior_distr")
### * tree_priors_to_xml_prior_distr

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: tree_priors_to_xml_prior_distr
### Title: Creates the distribution section in the prior section of the
###   distribution section of a BEAST2 XML parameter file. These lines
###   start with '<distribution id='
### Aliases: tree_priors_to_xml_prior_distr

### ** Examples

 # <distribution id="posterior" spec="util.CompoundDistribution">
 #     <distribution id="prior" spec="util.CompoundDistribution">
 #       HERE, where the ID of the distribution is 'prior'
 #     </distribution>
 #     <distribution id="likelihood" ...>
 #     </distribution>
 # </distribution>



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("tree_priors_to_xml_prior_distr", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("tree_priors_to_xml_tracelog")
### * tree_priors_to_xml_tracelog

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: tree_priors_to_xml_tracelog
### Title: Creates the tree priors' XML for the tracelog section
### Aliases: tree_priors_to_xml_tracelog

### ** Examples

# <logger id="tracelog" ...>
#'   # Here
# </logger>



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("tree_priors_to_xml_tracelog", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("yule_tree_prior_to_xml_prior_distr")
### * yule_tree_prior_to_xml_prior_distr

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: yule_tree_prior_to_xml_prior_distr
### Title: Creates the 'prior' section in the prior section of the prior
###   section of the distribution section of a BEAST2 XML parameter file
###   for a Yule tree prior
### Aliases: yule_tree_prior_to_xml_prior_distr

### ** Examples

 # <distribution id="posterior" spec="util.CompoundDistribution">
 #     <distribution id="prior" spec="util.CompoundDistribution">
 #       HERE, where the ID of the distribution is 'prior'
 #     </distribution>
 #     <distribution id="likelihood" ...>
 #     </distribution>
 # </distribution>



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("yule_tree_prior_to_xml_prior_distr", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
