# Examples

For all examples, do load `beautier`:

```
library(beautier)
```

All examples read the alignment from a FASTA file (usually `my_fasta.fas`) 
and create a BEAST2 input file called `my_beast.xml`.

## Example #1

Using all default settings, only specify a DNA alignment.

![All default](pics/all_default.png)

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
  initial_phylogeny = fasta_to_phylo(
    fasta_filename = "my_fasta.fas",
    crown_age = 15)
)
```

`fasta_to_phylo` creates a random phylogeny from
a FASTA file of a certain crown age. 

## Example #3: JC69 site model

![JC69](pics/jc69_2_4.png)

```
create_beast2_input_file(
  "my_alignment.fas",
  "my_beast.xml",
  site_models = create_jc69_site_model()
)
```

## Example #4: Relaxed clock log normal

![Relaxed clock log normal](pics/relaxed_clock_log_normal_2_4.png)

```
create_beast2_input_file(
  "my_alignment.fas",
  "my_beast.xml",
  clock_models = create_rln_clock_model()
)
```

## Example #5: Birth-Death tree prior

![Birth-Death tree prior](pics/birth_death_2_4.png)

```
create_beast2_input_file(
  "my_alignment.fas",
  "my_beast.xml",
  tree_priors = create_bd_tree_prior() 
)
```

## Example #6: Yule tree prior with a normally distributed birth rate

![Yule tree prior with a normally distributed birth rate](pics/birth_rate_normal_2_4.png)

```
create_beast2_input_file(
  "my_alignment.fas",
  "my_beast.xml",
  tree_priors = create_yule_tree_prior(
    birth_rate_distribution = create_normal_distribution()
  ) 
)
```

Thanks to Yacine Ben Chehida for this use case

## Example #7: HKY site model with a non-zero proportion of invariants

![HKY example](pics/hky_prop_invariant_0_5_2_4.png)

```
create_beast2_input_file(
  "my_alignment.fas",
  "my_beast.xml",
  site_models = create_hky_site_model(
    gamma_site_model = create_gamma_site_model(proportion_invariant = 0.5)
  )
)
```

Thanks to Yacine Ben Chehida for this use case

## Example #8: Strict clock with a known clock rate

![Strict clock with a rate of 0.5](pics/strict_clock_rate_0_5_2_4.png)

```
create_beast2_input_file(
  "my_alignment.fas",
  "my_beast.xml",
  clock_models = create_strict_clock_model(rate = 0.5) 
)
```

Thanks to Paul van Els and Yacine Ben Chehida for this use case.

## Example #9: Two alignments

![anthus_2_4.png](pics/anthus_2_4.png)

```
create_beast2_input_file(
  c("anthus_aco.fas", "anthus_nd2.fas"),
  "my_beast.xml"
)
```

Thanks to Paul van Els for this use case and supplying these FASTA files.

## Example #10: Two alignments, different site models

![Example 10](pics/aco_hky_nd2_tn93.png)

```
beautier::create_beast2_input_file(
  c("anthus_aco.fas", "anthus_nd2.fas"),
  "my_beast.xml"
  site_models = list(
    create_hky_site_model(), 
    create_tn93_site_model()
  )
)
```

Thanks to Paul van Els for this use case.

## Future examples

See [Future](Future.md)