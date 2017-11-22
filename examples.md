# Examples

![beautier logo](pics/beautier_logo.png)

For all examples, do load `beautier`:

```
library(beautier)
```

All examples read the alignment from a FASTA file (usually `my_fasta.fas`) 
and create a BEAST2 input file called `my_beast.xml`.

## Example #1: all default

Using all default settings, only specify a DNA alignment.

![Example #1: all default](pics/all_default.png)

```
create_beast2_input_file(
  "test_output_0.fas",
  "my_beast.xml"
)
```

All other parameters are set to their defaults, as in BEAUti.

## Example #2: fixed crown age

Using all default settings, only specify a DNA alignment.

```
[No screenshot, as this cannot be done in BEAUti yet]
```

```
create_beast2_input_file(
  "my_fasta.fas",
  "my_beast.xml",
  fixed_crown_age = TRUE,
  initial_phylogenies = fasta_to_phylo(
    fasta_filename = "my_fasta.fas",
    crown_age = 15)
)
```

`fasta_to_phylo` creates a random phylogeny from
a FASTA file of a certain crown age. 

## Example #3: JC69 site model

![Example #3: JC69 site model](pics/jc69_2_4.png)

```
create_beast2_input_file(
  "my_alignment.fas",
  "my_beast.xml",
  site_models = create_jc69_site_model()
)
```

## Example #4: Relaxed clock log normal

![Example #4: Relaxed clock log normal](pics/relaxed_clock_log_normal_2_4.png)

```{r example_4}
create_beast2_input_file(
  "my_alignment.fas",
  "my_beast.xml",
  clock_models = create_rln_clock_model()
)
```

## Example #5: Birth-Death tree prior

![Example #5: Birth-Death tree prior](pics/birth_death_2_4.png)

```{r example_5}
create_beast2_input_file(
  "my_alignment.fas",
  "my_beast.xml",
  tree_priors = create_bd_tree_prior() 
)
```

## Example #6: Yule tree prior with a normally distributed birth rate

![Example #6: Yule tree prior with a normally distributed birth rate](pics/birth_rate_normal_2_4.png)

```{r example_6}
create_beast2_input_file(
  "my_alignment.fas",
  "my_beast.xml",
  tree_priors = create_yule_tree_prior(
    birth_rate_distr = create_normal_distr()
  ) 
)
```

Thanks to Yacine Ben Chehida for this use case

## Example #7: HKY site model with a non-zero proportion of invariants

![Example #7: HKY site model with a non-zero proportion of invariants](pics/hky_prop_invariant_0_5_2_4.png)

```{r example_7}
create_beast2_input_file(
  "my_alignment.fas",
  "my_beast.xml",
  site_models = create_hky_site_model(
    gamma_site_model = create_gamma_site_model(prop_invariant = 0.5)
  )
)
```

Thanks to Yacine Ben Chehida for this use case

## Example #8: Strict clock with a known clock rate

![Example #8: Strict clock with a known clock rate](pics/strict_clock_rate_0_5_2_4.png)

```{r example_8}
create_beast2_input_file(
  "my_alignment.fas",
  "my_beast.xml",
  clock_models = create_strict_clock_model(
    clock_rate_parameter = create_clock_rate_parameter(value = 0.5)) 
)
```

Thanks to Paul van Els and Yacine Ben Chehida for this use case.

## Example #9: Two alignments

![Example 9: Two alignments](pics/anthus_2_4.png)

```{r example_9}
create_beast2_input_file(
  c("anthus_aco.fas", "anthus_nd2.fas"),
  "my_beast.xml"
)
```

Thanks to Paul van Els for this use case and supplying these FASTA files.

## Example #10: Two alignments, different site models

![Example 10: Two alignments, different site models](pics/aco_hky_nd2_tn93.png)

```{r example_10}
beautier::create_beast2_input_file(
  c("anthus_aco.fas", "anthus_nd2.fas"),
  "my_beast.xml",
  site_models = list(
    create_hky_site_model(), 
    create_tn93_site_model()
  )
)
```

Thanks to Paul van Els for this use case.
