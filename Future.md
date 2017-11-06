# Future use cases

## Example #9: Two alignments, different site models, clock models and tree priors 

```
beautier::create_beast2_input_file(
  c("anthus_aco.fas", "anthus_nd2.fas"),
  "my_beast.xml"
  site_models = c(
    create_jc69_site_model(), 
    create_hky_site_model()
  ),
  clock_models = c(
    create_strict_clock_model(rate = 0.1), 
    create_strict_clock_model(rate = 0.2)
  ),
  tree_priors = c(
    create_yule_tree_prior(), 
    create_bd_tree_prior()
  ),
  mcmc = create_mcmc(chain_length = 1000000),
  output_xml_filename = "my_beast.xml"
)
```

Thanks to Paul van Els for this use case.

