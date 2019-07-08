The following functions allow visualization of various aspects of GBM gradient boosted regression tree outputs

tree_plot
- Creates a tree of histograms showing partitioning of data through a single tree within the model
- Only able to plot 6 splits
Example:
![alt text](Example_images/tree_plot.png)

sankey_tree
- Uses a sankey diagram to visualize data partitioning through a single tree in the model
- Can view all splits
Example:
![alt text](Example_images/sankey_tree.png)

aggregate_split_sankey
- Aggregates all splits and following splits to show the most common split variables and the following split
- Option to exclude terminal nodes 
Example:
![alt text](Example_images/aggregate_split_sankey.png)

aggregate_tree_sankey
- Aggregates all trees throughout the model organized by split row to show typical patterns among split variables
Example:
![alt text](Example_images/aggregate_tree_sankey.png)
