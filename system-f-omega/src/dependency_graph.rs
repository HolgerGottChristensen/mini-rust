use std::borrow::Borrow;
use std::collections::{HashMap, HashSet, VecDeque};
use std::ops::Deref;
use crate::free_variables::{bound_variable_term, free_variables_term};
use crate::Term;

pub struct Graph<T> {
    nodes: Vec<T>,
    edges: Vec<(usize, usize)>,
}

/// Takes a list of terms as input and return a list of ordered terms by their interdependencies
pub fn dependency_graph(terms: Vec<Term>) -> Result<Vec<Term>, String> {

    //graph to build
    let mut graph = Graph {
        nodes: Vec::new(),
        edges: Vec::new(),
    };

    let mut id_counter: usize = 0;
    let mut id_to_term_map = HashMap::new();

    for t in &terms {
        graph.nodes.push((bound_variable_term(t.clone()), t.clone(), id_counter));
        id_to_term_map.insert(id_counter, t.clone());
        id_counter += 1;
    }

    for (_, t, id_1) in &graph.nodes {
        let free_vars = free_variables_term(t.clone());

        for variable in &free_vars {
            for (bound, t, id_2) in &graph.nodes {
                if let Some(res) = bound {
                    if res == variable {
                        graph.edges.push((id_2.clone(), id_1.clone()));
                    }
                }
            }
        }
    }

    let sorted_ids = graph.topological_sort();

    let mut sorted_terms = Vec::new();
    for id in &sorted_ids {
        if let Some(term) = id_to_term_map.get(id) {
            sorted_terms.push(term.clone());
        }
    }
    Ok(sorted_terms)
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
                    if let Some(c) = self.cycle_dfs(*to, on_stack, edge_to, marked) {
                        return Some(c);
                    }
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
    /// Given a Directed Acyclic Graph,
    /// Returns a topological sorted list of vertex id's (depth first reverse post order)
    pub fn topological_sort(&self) -> Vec<usize> {
        if let Some(_) = &self.cycle() {
            return Vec::new();
        }
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
    use crate::dependency_graph::{dependency_graph, Graph};
    use crate::{Int, Term, TermAbs, TermApp, TermVar, Type};
    use crate::Term::{Define, Integer};


    #[test]
    fn dependency_graph_gives_correct_order() -> Result<(), String>{
        //arrange
        let term_1 = TermApp(
            Box::new(TermVar("g".to_string())),
            Box::new(Integer(64))
        );

        let term_2 = Define(
            "g".to_string(),
            Type::TypeArrow(
                Box::new(Type::Base(Int)),
                Box::new(Type::Base(Int)),
            ),
            Box::new(TermAbs(
                "x".to_string(),
                Type::Base(Int),
                Box::new(
                    TermApp(
                        Box::new(TermVar("f".to_string())),
                        Box::new(TermVar("x".to_string()))
                    )
                )
            ))
        );

        let term_3 = Define(
            "f".to_string(),
            Type::TypeArrow(
                Box::new(Type::Base(Int)),
                Box::new(Type::Base(Int)),
            ),
            Box::new(TermAbs(
                "x".to_string(),
                Type::Base(Int),
                Box::new(
                    TermVar("x".to_string())
                )
            ))
        );

        let terms = vec![term_2.clone(), term_1.clone(), term_3.clone()];

        //act
        let sorted = dependency_graph(terms);

        //assert
        // expect the order y, y, some permutation of term 1 2 & 3
        assert_eq!(sorted?, vec![term_3.clone(), term_2.clone(), term_1.clone()]);

        Ok(())
    }

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

    #[test]
    fn small_graph_no_topological_order() {
        //arrange
        let graph = Graph {
            nodes: vec![0,1,2],
            edges: vec![
                (0,1), (1,2), (2,0),
            ]
        };
        //act
        let c = graph.topological_sort();
        //assert
        assert_eq!(c, Vec::new());
    }

    #[test]
    fn small_graph_topological_order() {
        //arrange
        let graph = Graph {
            nodes: vec![0,1,2],
            edges: vec![
                (0,1), (0,2), (2,1),
            ]
        };
        //act
        let sorted = graph.topological_sort();
        //assert
        assert_eq!(sorted, vec![0, 2, 1]);
    }

    #[test]
    fn small_2_graph_topological_order() {
        //arrange
        let graph = Graph {
            nodes: vec![0,1,2,3,4],
            edges: vec![
                (1,0), (3,2),
            ]
        };
        //act
        let sorted = graph.topological_sort();
        //assert
        assert_eq!(sorted, vec![4,3,2,1,0]);
    }

    #[test]
    fn small_graph_cycle_found() {
        //arrange
        let graph = Graph {
            nodes: vec![0,1,2],
            edges: vec![
                (0,1), (1,2), (2,0),
            ]
        };
        //act
        //assert
        if let Some(c) = graph.cycle() {
            assert_eq!(c, vec![2,1,0,2]);
        } else {
            assert!(false);
        }
    }

    #[test]
    fn small_graph_cycle_not_found() {
        //arrange
        let graph = Graph {
            nodes: vec![0,1,2],
            edges: vec![
                (0,1), (0,2), (2,1),
            ]
        };
        //act
        let c = graph.cycle();
        //assert
        assert_eq!(c, None);
    }

    #[test]
    fn big_graph_cycle_found() {
        //arrange
        let graph = Graph {
            nodes: vec![0,1,2,3,4,5,6,7,8,9,10,11,12],
            edges: vec![
                (0,5), (0,1), (0,6),
                (2,3), (2,0),
                (3,5),
                (4,3), (4,2),
                (5,4),
                (6,9), (6,4),
                (7,6),
                (8,7),
                (9,12), (9,11), (9,10),
                (11,12)
            ]
        };
        //act
        //assert
        if let Some(c) = graph.cycle() {
            assert_eq!(c, vec![3,4,5,3]);
        } else {
            assert!(false);
        }
    }

    #[test]
    fn big_graph_cycle_not_found() {
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
        let c = graph.cycle();
        //assert
        assert_eq!(c, None);
    }
}
