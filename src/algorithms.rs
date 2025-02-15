use std::collections::HashMap;

use crate::{Class, ClassId, EGraph, Node, NodeId};

pub const MISSING_ARG_VALUE: &str = "·";

impl EGraph {
    /// Inline all leaves (e-classes with a single node that has no children) into their parents, so that they
    /// are added to the function name like f(10, ·).
    /// Returns the number of leaves inlined.
    pub fn inline_leaves(&mut self) -> usize {
        // 1. Create mapping of eclass to nodes as well as nodes to their parents
        let mut eclass_to_nodes = std::collections::HashMap::new();
        let mut node_to_parents = std::collections::HashMap::new();
        for (node_id, node) in &self.nodes {
            eclass_to_nodes
                .entry(node.eclass.clone())
                .or_insert_with(Vec::new)
                .push((node_id.clone(), node));
            for child in &node.children {
                node_to_parents
                    .entry(child.clone())
                    .or_insert_with(Vec::new)
                    .push(node_id.clone());
            }
        }
        // 2. Find all leaves (e-classes with a single node that has no children and also not in root-eclasses)
        let mut leaves = Vec::new();
        let mut leave_to_op = std::collections::HashMap::new();
        for (eclass, nodes) in eclass_to_nodes {
            if nodes.len() == 1 && nodes[0].1.children.is_empty() {
                leaves.push((eclass, nodes[0].0.clone()));
                leave_to_op.insert(nodes[0].0.clone(), nodes[0].1.op.clone());
            }
        }
        // 3. Create mapping from all parents which are updated to the children which are inlined
        let mut parents_to_children = std::collections::HashMap::new();
        for (_, node_id) in &leaves {
            let parents = node_to_parents.get(node_id);
            // There will be no parents for isolated nodes with no parents or children
            if let Some(parents) = parents {
                for parent in parents {
                    parents_to_children
                        .entry(parent.clone())
                        .or_insert_with(Vec::new)
                        .push(node_id.clone());
                }
            }
        }
        // 4. Inline leaf nodes into their parents
        for (parent, leaf_children) in &parents_to_children {
            let additional_cost = leaf_children
                .iter()
                .map(|child| self.nodes.get(child).unwrap().cost)
                .sum::<ordered_float::NotNan<f64>>();
            let parent_node = self.nodes.get_mut(parent).unwrap();
            let args = parent_node
                .children
                .iter()
                .map(|child| {
                    if leaf_children.contains(child) {
                        leave_to_op.get(child).unwrap()
                    } else {
                        MISSING_ARG_VALUE
                    }
                })
                .collect::<Vec<_>>();
            // Remove leaf children from children
            parent_node
                .children
                .retain(|child| !leaf_children.contains(child));
            // If the parent node already had some children replaced, then just replace the remaining children
            // otherwise, replace the entire op
            let new_op = if parent_node.op.matches(MISSING_ARG_VALUE).count() == args.len() {
                // Replace all instances of MISSING_ARG_VALUE with the corresponding arg by interleaving
                // the op split by MISSING_ARG_VALUE with the args
                parent_node
                    .op
                    .split(MISSING_ARG_VALUE)
                    .enumerate()
                    .flat_map(|(i, s)| {
                        if i == args.len() {
                            vec![s.to_string()]
                        } else {
                            vec![s.to_string(), args[i].to_string()]
                        }
                    })
                    .collect::<String>()
            } else {
                format!("{}({})", parent_node.op, args.join(", "))
            };
            parent_node.op = new_op;
            parent_node.cost += additional_cost;
        }
        let mut n_inlined = 0;
        // 5. Remove leaf nodes from egraph, class data, and root eclasses
        for (eclass, node_id) in &leaves {
            // If this node has no parents, don't remove it, since it wasn't inlined
            if !node_to_parents.contains_key(node_id) {
                continue;
            }
            n_inlined += 1;
            self.nodes.swap_remove(node_id);
            self.class_data.swap_remove(eclass);
            self.root_eclasses.retain(|root| root != eclass);
        }
        n_inlined
    }

    /// Inline all leaves (e-classes with a single node that has no children) into their parents, recursively.
    pub fn saturate_inline_leaves(&mut self) {
        while self.inline_leaves() > 0 {}
    }

    /// Given some function `should_split`, after calling this method, all nodes where it is true will have at most
    /// one other node in their e-class and if they have parents, will no other nodes in their e-class.
    ///
    /// All nodes in an e-class shared with a split node will still be in a e-class with that node, but no longer
    /// in an e-class with each other.
    ///
    /// This is used to help make the visualization easier to parse, by breaking cycles and allowing these split nodes
    /// to be later inlined into their parents with `inline_leaves`, which only applies when there is a single node in an
    /// e-class.
    ///
    /// For example, if we split on all "primitive" nodes or their wrappers, like Int(1), then those nodes
    /// can then be inlined into their parents and are all nodes equal to them are no longer in single e-class,
    /// making the graph layout easier to understand.
    ///
    /// Note that `should_split` should only ever be true for either a single node, or no nodes, in an e-class.
    /// If it is true for multiple nodes in an e-class, then this method will panic.
    ///
    /// Another way to think about it is that any isomporphic function can be split, since if f(a) = f(b) then a = b,
    /// in that case.
    pub fn split_classes(&mut self, should_split: impl Fn(&NodeId, &Node) -> bool) {
        // run till fixpoint since splitting a node might add more parents and require splitting the child down the line
        let mut changed = true;
        while changed {
            changed = false;
            // Mapping from class ID to all nodes that point to any node in that e-class
            let parents: HashMap<ClassId, Vec<(NodeId, usize)>> =
                self.nodes
                    .iter()
                    .fold(HashMap::new(), |mut parents, (node_id, node)| {
                        for (position, child) in node.children.iter().enumerate() {
                            let child_class = self.nodes[child].eclass.clone();
                            parents
                                .entry(child_class)
                                .or_default()
                                .push((node_id.clone(), position));
                        }
                        parents
                    });

            for Class { id, nodes } in self.classes().clone().into_values() {
                let (unique_nodes, other_nodes): (Vec<_>, Vec<_>) = nodes
                    .into_iter()
                    .partition(|node_id| should_split(node_id, &self.nodes[node_id]));
                if unique_nodes.len() > 1 {
                    panic!(
                        "Multiple nodes in one e-class should be split. E-class: {:?} Nodes: {:?}",
                        id, unique_nodes
                    );
                }
                let unique_node = unique_nodes.into_iter().next();
                let class_data = self.class_data.get(&id).cloned();
                if let Some(unique_node_id) = unique_node {
                    let unique_node = self.nodes[&unique_node_id].clone();
                    let n_other_nodes = other_nodes.len();
                    let mut offset = 0;
                    if n_other_nodes == 0 {
                        continue;
                    }
                    // split out other nodes if there are multiple of them.
                    // Leave one node in this e-class and make new e-classes for remaining nodes
                    for other_node_id in other_nodes.into_iter().skip(1) {
                        changed = true;
                        // use same ID for new class and new node added to that class
                        let new_id = format!("split-{}-{}", offset, unique_node_id);
                        offset += 1;
                        let new_class_id: ClassId = new_id.clone().into();
                        // Copy the class data if it exists
                        if let Some(class_data) = &class_data {
                            self.class_data
                                .insert(new_class_id.clone(), class_data.clone());
                        }
                        // Change the e-class of the other node
                        self.nodes[&other_node_id].eclass = new_class_id.clone();
                        // Create a new unique node with the same data
                        let mut new_unique_node = unique_node.clone();
                        new_unique_node.eclass = new_class_id;
                        self.nodes.insert(new_id.into(), new_unique_node);
                    }
                    // If there are other nodes, then make one more copy and point all the parents at that
                    let parents = parents.get(&id).cloned().unwrap_or_default();
                    if parents.is_empty() {
                        continue;
                    }
                    changed = true;
                    let new_id = format!("split-{}-{}", offset, unique_node_id);
                    let new_class_id: ClassId = new_id.clone().into();
                    // Copy the class data if it exists
                    if let Some(class_data) = &class_data {
                        self.class_data
                            .insert(new_class_id.clone(), class_data.clone());
                    }
                    // Create a new unique node with the same data
                    let mut new_unique_node = unique_node.clone();
                    new_unique_node.eclass = new_class_id;
                    self.nodes.insert(new_id.clone().into(), new_unique_node);
                    for (parent_id, position) in parents {
                        // Change the child of the parent to the new node
                        self.nodes.get_mut(&parent_id).unwrap().children[position] =
                            new_id.clone().into();
                    }
                }
            }
            // reset the classes computation
            self.once_cell_classes.take();
        }
    }
}
