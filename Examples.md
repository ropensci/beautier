# Examples

## Example #1

Using all default settings, only specify a DNA alignment.

![All default](all_default.png)

```
# Create the BEAST2 XML input file
beastscriptr::create_beast2_input_file(
  input_fasta_filename = "my_fasta.fas",
  output_xml_filename = "my_beast.xml"
)
```

All other parameters are set to their defaults as in BEAUti

## Example #2: fixed crown age

Using all default settings, only specify a DNA alignment.

```
[No screenshot, as this cannot be done in BEAUti yet]
```

```
beastscriptr::create_beast2_input_file(
  input_fasta_filename = "my_fasta.fas",
  output_xml_filename = "my_beast.xml",
  fixed_crown_age = TRUE,
  initial_phylogeny = beastscriptr::fasta_to_phylo(
    fasta_filename = "my_fasta.fas",
    crown_age = 15)
)
```

## Example #3: JC69 site model

![JC69](jc69_2_4.png)

```
beastscriptr::create_beast2_input_file(
  input_fasta_filenames = "my_alignment.fas",
  site_models = create_jc69_site_model(), 
  output_xml_filename = "my_beast.xml"
)
```

## Example #4: Relaxed clock log normal

![Relaxed clock log normal](relaxed_clock_log_normal_2_4.png)

```
beastscriptr::create_beast2_input_file(
  input_fasta_filenames = "my_alignment.fas",
  clock_models = create_rln_clock_model(), 
  output_xml_filename = "my_beast.xml"
)
```

## Example #5: Birth-Death tree prior

![Birth-Death tree prior](birth_death_2_4.png)

```
beastscriptr::create_beast2_input_file(
  input_fasta_filenames = "my_alignment.fas",
  tree_priors = create_bd_tree_prior(), 
  output_xml_filename = "my_beast.xml"
)
```

## Example #6: HKY site model with a non-zero proportion of invariants

![HKY example](hky_prop_invariant_0_5_2_4.png)

```
beastscriptr::create_beast2_input_file(
  input_fasta_filenames = "my_alignment.fas",
  site_models = create_hky_site_model(
    proportion_invariant = 0.5
  ), 
  output_xml_filename = "my_beast.xml"
)
```

Thanks to Yacine Ben Cheheda for this use case

## Example #7: Strict clock with a known clock rate

![Strict clock with a rate of 0.5](strict_clock_rate_0_5_2_4.png)

```
beastscriptr::create_beast2_input_file(
  input_fasta_filenames = "my_alignment.fas",
  clock_models = create_strict_clock_model(rate = 0.5), 
  output_xml_filename = "my_beast.xml"
)
```

Thanks to Paul van Els and Yacine Ben Cheheda for this use case.
