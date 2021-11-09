#!/usr/bin/env python
#
# plot_untreated_population.py resultados gráficos del modelo de crecimiento
# poblacional de gatos sin cacería
import numpy as np
import pandas as pd
from geci_cli import geci_cli
from geci_plots import geci_plot, plt

path = geci_cli()

data_path = path.input[0][0]
plot_path = path.output[0][0]
data_population = pd.read_csv(data_path)

fig, ax = geci_plot()
ax.fill_between(
    data_population["yrs"],
    data_population["n_lo"],
    data_population["n_up"],
    alpha=0.2,
    label="Zona de confianza",
    color="b",
)
ax.plot(data_population["yrs"], data_population["n_md"], color="b", label=r"Proporción $N_0$")
ax.set_ylim(1, 2.6)
ax.set_xlim(2019.5, 2030)
ax.legend(loc="upper left", fontsize=15, framealpha=0)
ax.set_xlabel("Años", size=20)
ax.set_ylabel(r"Proporcional $N_0$", size=20)
ax.tick_params(labelsize=15)
fig.savefig(plot_path, dpi=300, transparent=True)
