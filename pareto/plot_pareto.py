import pandas as pd
import matplotlib.pyplot as plt
plt.style.use('seaborn-v0_8-whitegrid')

# Load the data
set1 = pd.read_csv('data/optimization_results/set1.csv', delimiter=" ")
set2 = pd.read_csv('data/optimization_results/set2.csv', delimiter=" ")
set3 = pd.read_csv('data/optimization_results/set3.csv', delimiter=" ")
set4 = pd.read_csv('data/optimization_results/set4.csv', delimiter=" ")

pairs = [[0, 1], [0, 2], [1, 2]]

objectives = ["economic", "biodiversity", "health"]

# Plot the Pareto fronts
fig, ax = plt.subplots(2, 2, figsize=(8, 6))
for i, pair in enumerate(pairs):
    ax[pair[1]-1][pair[0]].plot(set1.iloc[:, pair[0]],
                                set1.iloc[:, pair[1]], 'o', label='private, no STEM')
    ax[pair[1]-1][pair[0]].plot(set2.iloc[:, pair[0]],
                                set2.iloc[:, pair[1]], 'o', label='private, STEM')
    ax[pair[1]-1][pair[0]].plot(set3.iloc[:, pair[0]],
                                set3.iloc[:, pair[1]], 'o', label='public, no STEM')
    ax[pair[1]-1][pair[0]].plot(set4.iloc[:, pair[0]],
                                set4.iloc[:, pair[1]], 'o', label='public, STEM')

    ax[pair[1]-1][pair[0]].set_xlabel(objectives[pair[0]])
    ax[pair[1]-1][pair[0]].set_ylabel(objectives[pair[1]])
    # ax[i].set_title(f'Objective {pair[0]+1} vs. Objective {pair[1]+1}')
    # if i == 2:
    #    ax[i].legend()

# Get the handles and labels from the first plot
handles, labels = ax[0, 0].get_legend_handles_labels()

# Move the legend to the free subplot [1, 0]
ax[0, 1].legend(
    handles, labels, loc='center', fontsize=12, markerscale=1, labelspacing=1.2
)

# Hide the plot on ax[1, 0] since it's only for the legend
ax[0, 1].axis('off')

fig.tight_layout()
fig.savefig('figures/pareto_fronts.png', dpi=300)

plt.show()
