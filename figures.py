#Script that I made to generate plots for my manuscript
import seaborn as sns
import pandas as pd
import matplotlib.pyplot as plt
import os
import matplotlib.collections as clt
import numpy as np
import pandas as pd
import ptitprince as pt
import seaborn as sns


df = pd.read_csv("reduced_ndata.csv")
sns.set_theme(style="ticks")

# Show the joint distribution using kernel density estimation

plt.figure(figsize=(5,4))
sns.set_style("whitegrid", {'axes.grid': True, 'grid.color':'#E5E4E2'})
g = sns.jointplot(
    data=df,
    x="Science Anxiety", y="Math Anxiety", hue= "Anxiety 
Profiles",hue_order = ["High Math Anxiety", "Low STEM Anxiety"],
    palette = ["#440381", "#3AB795"],marginal_ticks = False, marginal_kws 
= {"alpha": 0.65},
    kind="scatter", xlim = [-5, 5], ylim = [-5,5], space=0
)

g.plot_joint(sns.kdeplot)
g.ax_joint.set_xticks([-2, -1, 0, 1, 2])
g.ax_joint.set_yticks([-2, -1, 0, 1, 2])
plt.savefig("JointDensity2.png", dpi = 400)

df = pd.read_csv("long_data.csv")
df.head()
dx = "Var"; dy = "Z-scores"; dhue = "Anxiety Profiles"; ort = "h"; pal =  
["#440381", "#3AB795"]; sigma = .2; hue_order = ["High Math Anxiety", "Low 
STEM Anxiety"]
f, ax = plt.subplots(figsize=(6, 4))
ax=pt.RainCloud(x = dx, y = dy, hue = dhue, data = df, palette = pal, 
hue_order = hue_order, bw = sigma, width_viol = .7,
                ax = ax, orient = ort , alpha = .65, dodge = False, 
pointplot = False, move = .02)

ax.set(ylabel = None)
plt.title('B.', y=1, x = -.07, fontsize = 15)
plt.savefig("Distribution.png", dpi = 400,bbox_inches='tight')
