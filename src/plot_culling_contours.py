#!/usr/bin/env python
#
# plot_culling_contours.py
# Grafica de contornos de la poblacion remanente en funcion de la remoción de individuos

from geci_cli import geci_cli
from geci_plots import geci_plot
import matplotlib
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd


path = geci_cli()

data_path = path.input[0][0]
plot_path = path.output[0][0]
data_culling = pd.read_csv(data_path)

x_grid, y_grid = np.meshgrid(data_culling.x.unique(), data_culling.y.unique())
z_grid = np.reshape(data_culling.z.to_numpy(), x_grid.T.shape)
x_grid = x_grid.T
y_grid = y_grid.T

fig, ax = geci_plot()
plt.contourf(x_grid, y_grid, z_grid, np.arange(0, 0.33, 0.01), alpha=0.8, extend="both")
CS = ax.contour(x_grid, y_grid, z_grid, np.arange(0, 0.33, 0.03), colors="black")
ax.clabel(CS, CS.levels, inline=True, fontsize=10)
plt.colorbar()
plt.xlabel("Initial cull", size=20)
plt.ylabel("Maintenance cull", size=20)
ax.tick_params(labelsize=15)
fig.savefig(plot_path, dpi=300, transparent=True)
