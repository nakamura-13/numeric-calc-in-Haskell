import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
plt.style.use('ggplot')

data_euler = pd.read_csv("./euler.csv", header=None, names=("t", "y"))
data_lk4 = pd.read_csv("./lk4.csv", header=None, names=("t", "y"))

t_exact = np.linspace(0, 600, 100)
y_exact = 1.0 * np.exp(-0.01*t_exact)

plt.scatter(data_euler["t"], data_euler["y"], c="blue", label="Euler")
plt.scatter(data_lk4["t"], data_lk4["y"], c="green", label="LK4")
plt.plot(t_exact, y_exact, label="Exact")
plt.xlabel("t")
plt.ylabel("y")
plt.legend()
plt.show()
