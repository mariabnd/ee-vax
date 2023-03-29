# ee-vax

The preprocessing scripts stored in the `munge` directory are executed sequentially when `load.project()` is called. The numbered prefixes indicate the ordering of execution.

<div class="center">

```mermaid
graph TD
    A[01_manual-cantons.R] --> B[02_manual-adjacency.R];
    B[02_manual-adjacency.R] --> C[03_load-data-map.R];
    C[03_load-data-map.R] --> D[04_load-data-mobility-and-transport.R];
    D[04_load-data-mobility-and-transport.R] --> E[05_load-data-contact-and-policy.R];
    E[05_load-data-contact-and-policy.R] --> F[06_load-data-vaccines-and-population.R];
    F[06_load-data-vaccines-and-population.R] --> G[07_load-data-cases.R];
    G[07_load-data-cases.R] --> H[08_create-coverage-covariate.R];
    H[08_create-coverage-covariate.R] --> I[09_models-setup.R];
    I[09_models-setup.R] --> J[10_scenario-analysis-alternative-covariate.R];
```
</div>

After this scripts for sensitivity analyses are run

<div class="center">

```mermaid
graph TD
    A[11_sensitivity-vaccination-coverage.R];
```
</div>
