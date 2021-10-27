#!/usr/bin/env python
#
# plot_constant_proportional_annual_cull.py resultados gráficos del modelo de crecimiento
# poblacional de gatos sin cacería
import numpy as np
import pandas as pd
from geci_cli import geci_cli
from geci_plots import geci_plot
import matplotlib.pyplot as plt

path = geci_cli()

data_path = path.input[0][0]
plot_path = path.output[0][0]
data_population = pd.read_csv(data_path)

fig, ax = geci_plot()
ax.fill_between(
    data_population["harv.prop.consist"],
    data_population["min.lo.n"],
    data_population["min.up.n"],
    alpha=0.2,
    label="Zona de confianza",
    color="b",
)
ax.plot(
    data_population["harv.prop.consist"],
    data_population["min.med.n"],
    color="b",
    label=r"Proporción $N_0$",
)
ax.set_ylim(-0.01, 1)
ax.set_xlim(0.2, 1)
ax.legend(loc="upper left", fontsize=15)
ax.set_xlabel("Proporción constante de reducción", size=20)
ax.set_ylabel(r"Proporcional $N_0$", size=20)
ax.tick_params(labelsize=15)
fig.savefig(plot_path, dpi=300, transparent=True)
