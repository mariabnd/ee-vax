# ee-vax

Project-specific configurations are stored in `lib/globals.R`. This is the first file loaded from `lib`, so any functions in `lib`, `munge` or `src` can reference this configuration through `config$my_config_var`.

