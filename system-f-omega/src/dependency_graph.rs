use std::borrow::Borrow;
use std::collections::{HashMap, HashSet, VecDeque};
use std::ops::Deref;
use crate::free_variables::{bound_variable_term, free_term_variables};
use crate::Term;

pub struct Graph {
    nodes: Vec<usize>,
    edges: Vec<(usize, usize)>,
}

/// Takes a list of terms as input and return a list of ordered terms by their interdependencies
pub fn dependency_graph(terms: Vec<Term>) -> Result<Vec<Term>, String> {

    //graph to build
    let mut graph = Graph {
        nodes: Vec::new(),
        edges: Vec::new(),
    };

    let mut aux_nodes = Vec::new();

    let mut id_counter: usize = 0;
    let mut id_to_term_map = HashMap::new();

    for t in &terms {
        aux_nodes.push((bound_variable_term(t.clone()), t.clone(), id_counter));
        graph.nodes.push(id_counter);
        id_to_term_map.insert(id_counter, t.clone());
        id_counter += 1;
    }

    for (_, t, id_1) in &aux_nodes {
        let free_vars = free_term_variables(t.clone());

        for variable in &free_vars {
            for (bound, t, id_2) in &aux_nodes {
                if let Some(res) = bound {
                    if res == variable {
                        graph.edges.push((id_2.clone(), id_1.clone()));
                    }
                }
            }
        }
    }

    /*
    //TODO: make it work
    //construct super (group) graph
    let mut group_id_counter: usize = 0;
    let mut id_to_group_map = HashMap::new();
    let mut group_to_id_map = HashMap::new();

    let connected_components = graph.strongly_connected_component();

    for group in &connected_components {
        for member in group {
            id_to_group_map.insert(member, group_id_counter);
        }
        group_to_id_map.insert(group_id_counter, group.clone());
        group_id_counter += 1;
    }

    let group_edges = HashSet::new();
    for (from, to) in &graph.edges {
        let from_group = id_to_group_map.get(from);
        let to_group = id_to_group_map.get(to);
        if let (Some(f), Some(t)) = (from_group, to_group) {
            if f != t {
                &group_edges | &HashSet::from([(f.clone(),t.clone())]);
            }
        }
    }

    let mut group_nodes = Vec::new();
    for key in group_to_id_map.keys() {
        group_nodes.push(key.clone());
    }

    let group_graph = Graph {
        nodes: group_nodes,
        edges: Vec::from_iter(group_edges)
    };

    let sorted_group_ids = group_graph.topological_sort();
    let mut unwrapped_sorted_group_ids = Vec::new();
    for group in &sorted_group_ids {
        if let Some(ids) = group_to_id_map.get(group) {
            unwrapped_sorted_group_ids.push(ids.clone());
        }
    }
    let sorted_ids: Vec<usize> = unwrapped_sorted_group_ids.into_iter().flatten().collect();
    */
    let sorted_ids = graph.topological_sort();
    let mut sorted_terms = Vec::new();
    for id in &sorted_ids {
        if let Some(term) = id_to_term_map.get(id) {
            sorted_terms.push(term.clone());
        }
    }
    Ok(sorted_terms)
}

impl Graph {

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

    fn strongly_connected_components_util(&self,
                                         u: usize,
                                         low: &mut Vec<i64>,
                                         disc: &mut Vec<i64>,
                                         stack_member: &mut Vec<bool>,
                                         stack: &mut Vec<usize>,
                                         comps: &mut Vec<Vec<usize>>, time: &mut i64
    ) {
        disc[u] = time.clone();
        low[u] = time.clone();
        *time += 1;
        stack_member[u] = true;
        stack.push(u);

        for (from, to) in &self.edges {
            if *from == u {
                if disc[*to] == -1 {
                    self.strongly_connected_components_util(to.clone(), low, disc, stack_member, stack, comps, time);
                    low[*from] = low[*from].min(low[*to]);
                } else if stack_member[*to] {
                    low[*from] = low[*from].min(disc[*to]);
                }
            }
        }

        let mut w: i64 = -1;
        let mut group = Vec::new();
        if low[u] == disc[u] {
            let u_int = u as i64;
            while w != u_int {
                if let Some(res) = stack.pop() {
                    w = res as i64;
                    group.push(w as usize);
                    stack_member[w as usize] = false;
                }
            }
            comps.push(group);
        }

    }

    pub fn strongly_connected_component(&self) -> Vec<Vec<usize>> {
        let mut low = vec![-1; self.nodes.len()];
        let mut disc = vec![-1; self.nodes.len()];
        let mut stack_member = vec![false; self.nodes.len()];
        let mut stack = Vec::new();
        let mut comps = Vec::new();
        let mut time = 0;

        for node in 0..self.nodes.len() {
            if disc[node] == -1 {
                self.strongly_connected_components_util(node, &mut low, &mut  disc, &mut  stack_member, &mut  stack, &mut  comps, &mut time);
            }
        }

        comps
    }

    /// Returns a list of vertex id's in depth first post order
    pub fn depth_first_order(&self) -> Vec<usize> {
        let mut queue = Vec::new();
        let mut marked = vec![false; self.nodes.len()];

        for i in &self.nodes {
            if !marked[*i] {
                // dfs
                self.dfs(i.clone(), &mut queue, &mut marked);
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
    use crate::Term::{Define, If, Integer, True};


    #[test]
    fn dependency_graph_gives_correct_order_on_sequential_program() -> Result<(), String>{
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
    fn dependency_graph_gives_correct_order_on_recursive_program() -> Result<(), String>{
        //arrange
        let term_1 = TermApp(
            Box::new(TermVar("f".to_string())),
            Box::new(Integer(3))
        );

        let term_2 = TermApp(
            Box::new(TermVar("g".to_string())),
            Box::new(Integer(3))
        );

        let term_3 = Define(
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

        let term_4 = Define(
            "f".to_string(),
            Type::TypeArrow(
                Box::new(Type::Base(Int)),
                Box::new(Type::Base(Int)),
            ),
            Box::new(TermAbs(
                "x".to_string(),
                Type::Base(Int),
                Box::new(If(
                    Box::new(True),
                    Box::new(Integer(0)),
                    Box::new(TermApp(
                        Box::new(TermVar("f".to_string())),
                        Box::new(Integer(3))
                    ))
                ))
            ))
        );

        let terms = vec![term_2.clone(), term_4.clone(), term_1.clone(), term_3.clone()];

        //act
        let sorted = dependency_graph(terms);

        //assert
        // expect the order y, y, some permutation of term 1 2 & 3
        assert_eq!(sorted?, vec![term_4.clone(), term_3.clone(), term_1.clone(), term_2.clone()]);

        Ok(())
    }

    #[test]
    fn dependency_graph_gives_correct_order_on_mutual_recursive_program() -> Result<(), String>{
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
                Box::new(If(
                    Box::new(True),
                    Box::new(Integer(0)),
                    Box::new(TermApp(
                        Box::new(TermVar("f".to_string())),
                        Box::new(Integer(3))
                    ))
                ))
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
                Box::new(If(
                    Box::new(True),
                    Box::new(Integer(0)),
                    Box::new(TermApp(
                        Box::new(TermVar("g".to_string())),
                        Box::new(Integer(3))
                    ))
                ))
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

    #[test]
    fn medium_graph_5_strongly_connected_components() {
        //arrange
        let graph = Graph {
            nodes: vec![0,1,2,3,4,5,6],
            edges: vec![
                (0,1),
                (1,2), (1,3), (1,4), (1,6),
                (2,0),
                (3,5),
                (4,5),
            ]
        };

        let expected_comps = vec![
            vec![5],
            vec![3],
            vec![4],
            vec![6],
            vec![2,1,0]
        ];
        //act
        let c = graph.strongly_connected_component();
        //assert
        assert_eq!(c, expected_comps);
    }

    #[test]
    fn big_graph_5_strongly_connected_components() {
        //arrange
        let graph = Graph {
            nodes: vec![0,1,2,3,4,5,6,7,8,9,10],
            edges: vec![
                (0,1), (0,3),
                (1,2), (1,4), (1,6),
                (2,0), (2,6),
                (3,2), (3,5),
                (4,5), (4,6),
                (5,6), (5,7), (5,8), (5, 9),
                (6, 4),
                (7,9),
                (8,9),
                (9,8)
            ]
        };

        let expected_comps = vec![
            vec![8,9],
            vec![7],
            vec![5,4,6],
            vec![3,2,1,0],
            vec![10]
        ];
        //act
        let c = graph.strongly_connected_component();
        //assert
        assert_eq!(c, expected_comps);
    }
}
