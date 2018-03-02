# FAQ

## Which version of BEAUti do you use as a guideline?

Version 2.4.7.

## What's the [road map](road_map.md)?

See [road map](road_map.md).

## How can I indicate a feature that I miss?

Submit an Issue.

## How can I submit code?

See [CONTRIBUTING](CONTRIBUTING.md), at 'Submitting code'

## How can I submit a bug?

See [CONTRIBUTING](CONTRIBUTING.md), at 'Submitting bugs' 

## How can I indicate something else?

Submit an Issue. Or send an email to Richel Bilderbeek.

## How do I reference to this work?

Cite:

 * Bilderbeek, Richel J.C., Etienne, Rampal S., "babette: BEAUti 2, BEAST2 and Tracer for R". bioRxiv 271866; doi: https://doi.org/10.1101/271866

## What is the idea behind the logo?

The logo consists of a rough redraw of Belle, from the Disney animated
movie 'Beauty and the Beast', and the R logo. 

## What are the FASTA files?

FASTA files `anthus_aco.fas` and `anthus_nd2.fas` from:
 
 * Van Els, Paul, and Heraldo V. Norambuena. "A revision of species limits in Neotropical pipits Anthus based on multilocus genetic and vocal data." Ibis.

Thanks to Paul van Els.

FASTA files `test_output_0.fas`, `anthus_nd3.fas` and `anthus_nd4.fas`
are artificially made FASTA files. 

## If I set a fixed crown age with multiple alignments, only the first alignment has so

Correct. This is a feature of BEAST2, which I assume is right. 

## Why are the functions prefixed with `create_`?

Or, why is this chosen:

```{r}
out <- create_beast2_input(
  "alignment.fas",
  tree_priors = create_yule_tree_prior(
    birth_rate_distr = create_exp_distr()    
  )
)
```

over this:

```{r}
out <- create_beast2_input(
  "alignment.fas",
  tree_priors = yule_tree_prior(
    birth_rate_distr = exp_distr()    
  )
)
```

Answer: because of readability. 

In this example, one could argue that prefixing `create_` 
hinders readability, as it makes the code unnecessarily
long. Additionally, `ggplot2` also omits the creation of `geom`s.

There are arguments against this line of thought: a function
'does' something, thus should contain a verb (as that would reflect its
meaning). The `create` or `make` prefix has its place in a Factory
Method Design Pattern. 

In the example above, however, the people involved in `beautier`
felt removing `create_` would be preferable. 

Yet, when doing so consistently throughout the package, problems
turn up.

When removing the `create_` prefix, this code:

```{r}
site_model <- list(
  gamma_site_model = create_gamma_site_model(),
  # ..
)
```

becomes:

```{r}
site_model <- list(
  gamma_site_model = gamma_site_model(),
  # ..
)
```

which cannot be interpreted. A workaround would be use an abbreviation
of the `gamma_site_model` argument:

```{r}
site_model <- list(
  gsm = gamma_site_model(),
  # ..
)
```

This abbreviation hurt readability. The `beautier` team felt is a
bad idea to be inconsistent in prefixing functions with `create_`.

In the end, it was decided to keep `create_`. Yes, it is more 
typing (auto-complete will fix that), but clarity comes first.

 

