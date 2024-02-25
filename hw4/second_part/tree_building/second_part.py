from ucimlrepo import fetch_ucirepo 
from sklearn import tree
import matplotlib.pyplot as plt
  
# fetching 
iris = fetch_ucirepo(id=53) 

X = iris.data.features 
y = iris.data.targets 
  
# variable information 
print(iris.metadata) 
print(iris.variables) 

# build iris decision tree
clf = tree.DecisionTreeClassifier()
clf = clf.fit(X, y)

# visualize the decision tree
tree.plot_tree(clf)
plt.show()

# export the decision tree data to a file
tree.export_graphviz(clf, out_file='tree.dot')

# convert the decision tree data to a png file
from subprocess import call
call(['dot', '-T', 'png', 'tree.dot', '-o', 'tree.png'])
