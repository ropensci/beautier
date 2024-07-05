# FAQ

## Shouldn't the slogan be 'beautier: BEAUti 2 for R'?

That slogan would indeed be more precise. That
extra precision would come at the cost of
readability (the extra '2'). As there is no `BEAUti 1`,
there is no possible confusion and that extra number
does not add extra information.

## Which version of BEAUti do you use as a guideline?

The first BEAST2 XML files created by `beautier`
followed BEAST2 v2.4. `beautier` follows the
BEAST2 versions, which is now at v2.6.0.

The BEAST2 version actually used by `babette`
can be found in the [beastier::install_beast2](https://github.com/ropensci/beastier/blob/master/R/install_beast2.R) function.

## Why does AppVeyor only check the `master` branch?

Because `ropensci` does not have AppVeyor.

To do check for Windows,
[the beautier_on_windows repo](https://github.com/richelbilderbeek/beautier_on_windows)
is created. That repo only checks the `master` branch of `beautier`.

## What's the [road map](road_map.md)?

See [road map](road_map.md).

## How can I indicate a feature that I miss?

Submit an Issue.

## How can I submit code?

See [CONTRIBUTING](../CONTRIBUTING.md), at 'Submitting code'

## How can I submit a bug?

See [CONTRIBUTING](../CONTRIBUTING.md), at 'Submitting bugs'

## How can I indicate something else?

Submit an Issue. Or send an email to Richèl Bilderbeek.

## How do I reference to this work?

Cite:

```
Bilderbeek, Richèl JC, and Rampal S. Etienne. "babette: BEAUti 2, BEAST 2 and Tracer for R." Methods in Ecology and Evolution (2018).
```

or

```
@article{bilderbeek2018babette,
  title={babette: BEAUti 2, BEAST 2 and Tracer for R},
  author={Bilderbeek, Richèl JC and Etienne, Rampal S},
  journal={Methods in Ecology and Evolution},
  year={2018},
  publisher={Wiley Online Library}
}
```

### Are there any related packages?

* [lumier](https://github.com/ropensci/lumier): Shiny app to help create the function call needed
* [BEASTmasteR](https://github.com/nmatzke/BEASTmasteR): tip-dating analyses using fossils as dated terminal taxa
* [BEASTifier](https://github.com/josephwb/BEASTifier): generate BEAST input files from a NEXUS file, similar to [beautier](https://github.com/ropensci/beautier)
* [RBeast](https://github.com/beast-dev/RBeast): misc other things

## What is the idea behind the logo?

The butterfly symbolizes beauty.
Then it was combined with an R logo.

## What are the FASTA files?

Filename               |Reference
-----------------------|------------
`anthus_aco.fas`       |[1]
`anthus_nd2.fas`       |[1]
`G_VII_pre2003_msa.fas`|[2]
Others                 |Artificial

* [1] Van Els, Paul, and Heraldo V. Norambuena. "A revision of species limits in Neotropical pipits Anthus based on multilocus genetic and vocal data." Ibis.
* [2] Durr, PA; Wibowo, MH; Tabbu, CR; Asmara, W; Selleck, P; Wang, J; Broz, I; Graham, K.; Dimitrov, K and Afonso, C. (in preparation). Phylodynamics of Genotype VII Newcastle disease virus in Indonesia.

Thanks to Peter A. Durr and Paul van Els for supplying the FASTA files.

## Why are the functions prefixed with `create_`?

Or, why is this chosen:

```{r}
out <- create_beast2_input(
  "alignment.fas",
  tree_prior = create_yule_tree_prior(
    birth_rate_distr = create_exp_distr()    
  )
)
```

over this:

```{r}
out <- create_beast2_input(
  "alignment.fas",
  tree_prior = yule_tree_prior(
    birth_rate_distr = exp_distr()    
  )
)
```

Answer: because function names should start with a
verb (see e.g. [https://style.tidyverse.org/functions.html#naming](The Tidyverse Style Guide))

## Why the name?

`beautier` is 'BEAUti for R'.

Additionally, it is a joke that suggests `beautier` would have more beauty than `BEAUti`.
This suggestion benefits the image of author of `beautier`, who, however, thinks that
both tools are equally valuable and beautiful.

## Why the logo?

Initially, the logo was a low-tech remake of Belle, for Beauty and the Beast.
To prevent problems with Disney, a different logo was picked.

The current logo shows a butterfly, an animal considered to be beautiful.
The butterfly is drawn by Jose Scholte, who kindly allowed her work to
be used for free, by attribution.

## `BEAUti` problems

### Not enough memory

```
./beauti
```

```
Can't start up: not enough memory
```

On Artful Aardvark, remove `-Xms256m -Xmx4g` from the `bin/beauti` file's last line. Change:

```
"$JAVA" -Dlauncher.wait.for.exit=true -Xms256m -Xmx4g -Djava.library.path="$BEAST_LIB" -Duser.language=en -cp "$BEAST_LIB/launcher.jar" beast.app.beauti.BeautiLauncher -capture $*
```

to

```
"$JAVA" -Dlauncher.wait.for.exit=true -Djava.library.path="$BEAST_LIB" -Duser.language=en -cp "$BEAST_LIB/launcher.jar" beast.app.beauti.BeautiLauncher -capture $*
```

### BEAUti requires Java version at least 8

Do:

```
sudo update-alternatives --config java
```

Pick `/usr/lib/jvm/java-8-openjdk-amd64/jre/bin/java`:

```
There are 5 choices for the alternative java (providing /usr/bin/java).

  Selection    Path                                            Priority   Status
------------------------------------------------------------
  0            /usr/lib/jvm/java-9-openjdk-amd64/bin/java       1091      auto mode
  1            /usr/bin/gij-4.8                                 1048      manual mode
  2            /usr/bin/gij-5                                   1050      manual mode
  3            /usr/bin/gij-6                                   1060      manual mode
* 4            /usr/lib/jvm/java-8-openjdk-amd64/jre/bin/java   1081      manual mode
  5            /usr/lib/jvm/java-9-openjdk-amd64/bin/java       1091      manual mode
```
