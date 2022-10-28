use crate::Term;

pub struct Graph<T> {
    nodes: Vec<T>,
    edges: Vec<(usize, usize)>,
}

/// Takes a list of terms as input and return a list of ordered terms by their interdependencies
pub fn dependency_graph(terms: Vec<Term>) -> Result<Vec<Term>, String> {

    /*
    let mut graph = Graph {
        nodes: Vec::new(),
        edges: Vec::new()
    };

    for t in terms {

    }
    */

    todo!()
}

impl<T> Graph<T> {

    fn dfs(&self, s: usize, queue: &mut Vec<usize>, marked: &mut Vec<bool>) {
        marked[s] = true;
        for (from, to) in &self.edges {
            if *from == s {
                if !marked[*to] {
                    self.dfs(*to, queue, marked);
                }
            }
        }
        queue.push(s);
    }

    fn cycle_dfs(&self, s: usize, on_stack: &mut Vec<bool>, edge_to: &mut Vec<usize>, marked: &mut Vec<bool>) -> Option<Vec<usize>>{
        marked[s] = true;
        on_stack[s] = true;
        for (from, to) in &self.edges {
            if *from == s {
                if !marked[*to] {
                    edge_to[*to] = *from;
                    self.cycle_dfs(*to, on_stack, edge_to, marked);
                } else if on_stack[*to] {
                    let mut stack = Vec::new();
                    let mut x = *from;
                    while x != *to {
                        stack.push(x);
                        x = edge_to[x];
                    }
                    stack.push(*to);
                    stack.push(*from);
                    return Some(stack);
                }
            }
        }
        on_stack[s] = false;
        None
    }

    /// Returns a list of vertex id's in depth first post order
    pub fn depth_first_order(&self) -> Vec<usize> {
        let mut queue = Vec::new();
        let mut marked = vec![false; self.nodes.len()];

        for i in 0..self.nodes.len() {
            if !marked[i] {
                // dfs
                self.dfs(i, &mut queue, &mut marked);
            }
        }
        queue
    }
    /// Returns a topological sorted list of vertex id's (depth first reverse post order)
    pub fn topological_sort(&self) -> Vec<usize> {
        let mut depth_first_order = self.depth_first_order();
        depth_first_order.reverse();
        depth_first_order
    }

    pub fn cycle(&self) -> Option<Vec<usize>> {
        let mut marked = vec![false;self.nodes.len()];
        let mut on_stack = vec![false;self.nodes.len()];
        let mut edge_to = vec![0;self.nodes.len()];

        for i in 0..self.nodes.len() {
            if !marked[i] {
                if let Some(res) = self.cycle_dfs(i, &mut on_stack, &mut edge_to, &mut marked) {
                    return Some(res);
                }
            }
        }
        None
    }
}

mod tests {
    use crate::dependency_graph::Graph;

    #[test]
    fn top_sort_small_graph() {
        //arrange
        let graph = Graph {
            nodes: vec![0,1,2,3,4,5,6,7,8,9,10,11,12],
            edges: vec![
                (0,5), (0,1), (0,6),
                (2,3), (2,0),
                (3,5),
                (5,4),
                (6,9), (6,4),
                (7,6),
                (8,7),
                (9,12), (9,11), (9,10),
                (11,12)
            ]
        };

        //act
        let sorted = graph.depth_first_order();

        //assert
        assert_eq!(sorted, vec![4,5,1,12,11,10,9,6,0,3,2,7,8]);

    }
}
