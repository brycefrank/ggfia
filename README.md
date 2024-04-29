# ggfia

`ggfia` provides easy to use functions to display FIA plots with `ggplot2`.
`ggfia` is currently in development, with some ideas for functionality given
below.

- Display the complete FIA plot (e.g., Bechtold and Patterson figure)

```
ggplot() +
  geom_fia_plt()
```

- Display the complete FIA plot without the microplots

```
ggplot() +
  geom_fia_plt(subptyp = c("macro", "mid"))
```

Strategies:

1. User inputs a single point and gets a full fia plot back:
  `geom_fia_plt`
  - aes(x = 0, y = 0, macro = T, micro = T, mid = T)
2. User inputs all points they want rendered manually:
  `geom_fia_subp`