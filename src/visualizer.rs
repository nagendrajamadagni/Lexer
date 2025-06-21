use std::collections::HashMap;

use crate::fa::Symbol;
use eframe::{run_native, App, CreationContext, NativeOptions};
use egui::Color32;
use egui_graphs::{
    DefaultGraphView, Graph, SettingsInteraction, SettingsNavigation, SettingsStyle,
};
use petgraph::{graph::EdgeIndex, graph::NodeIndex, prelude::StableGraph};

use crate::fa::FA;

struct Visualizer {
    graph: Graph,
}

impl Visualizer {
    fn new(_: &CreationContext<'_>, graph: Graph) -> Self {
        Visualizer { graph }
    }
}

impl App for Visualizer {
    fn update(&mut self, ctx: &egui::Context, _: &mut eframe::Frame) {
        egui::CentralPanel::default().show(ctx, |ui| {
            let navigation_settings = &SettingsNavigation::new()
                .with_zoom_and_pan_enabled(true)
                .with_fit_to_screen_enabled(true);
            let interactive_settings = &SettingsInteraction::new()
                .with_dragging_enabled(true)
                .with_node_clicking_enabled(true)
                .with_node_selection_enabled(true)
                .with_node_selection_multi_enabled(true)
                .with_edge_clicking_enabled(true)
                .with_edge_selection_enabled(true)
                .with_edge_selection_multi_enabled(true);
            let style_settings = &SettingsStyle::default().with_labels_always(true);
            ui.add(
                &mut DefaultGraphView::new(&mut self.graph)
                    .with_styles(style_settings)
                    .with_interactions(interactive_settings)
                    .with_navigations(navigation_settings),
            );
        });
    }
}

fn generate_stable_graph<T: FA>(fa: &T) -> Graph {
    let mut stable_graph = StableGraph::new();

    let num_states = fa.get_num_states();

    let start_node_color = Color32::from_rgb(20, 67, 130);
    let accept_node_color = Color32::from_rgb(20, 130, 90);

    let mut edge_map: HashMap<(NodeIndex, NodeIndex), EdgeIndex> = HashMap::new();

    // Add all nodes

    for _state_idx in 0..num_states {
        stable_graph.add_node(());
    }

    // Add all edges and store in map for adding labels later

    for state_idx in 0..num_states {
        let transition_list = fa.get_state_transitions(state_idx);

        for transition in transition_list {
            let edge_target = transition.1;

            if !stable_graph.contains_edge(NodeIndex::new(state_idx), NodeIndex::new(*edge_target))
            {
                let edge_idx = stable_graph.add_edge(
                    NodeIndex::new(state_idx),
                    NodeIndex::new(*edge_target),
                    (),
                );
                edge_map.insert(
                    (NodeIndex::new(state_idx), NodeIndex::new(*edge_target)),
                    edge_idx,
                );
            }
        }
    }

    let mut graph = Graph::from(&stable_graph);

    let start_node = fa.get_start_state();
    let start_node = graph.node_mut(NodeIndex::new(start_node)).unwrap();
    start_node.set_color(start_node_color);

    for accept_state in fa.get_acceptor_states().iter_ones() {
        let accept_node = graph.node_mut(NodeIndex::new(accept_state)).unwrap();
        accept_node.set_color(accept_node_color);
    }

    for state_idx in 0..num_states {
        let node_label = format!("State {}", state_idx);
        graph
            .node_mut(NodeIndex::new(state_idx))
            .unwrap()
            .set_label(node_label);

        let transition_list = fa.get_state_transitions(state_idx);

        for transition in transition_list {
            let edge_label = match *transition.0 {
                Symbol::Char(ch) => format!("{}", ch),
                Symbol::Epsilon => "eps".to_string(),
            };

            let edge_target = transition.1;
            let edge_idx = edge_map
                .get(&(NodeIndex::new(state_idx), NodeIndex::new(*edge_target)))
                .unwrap();
            let edge = graph.edge_mut(*edge_idx).unwrap();

            let old_label = edge.label();
            let new_label = if old_label.starts_with("e") {
                edge_label
            } else {
                format!("{}, {}", old_label, edge_label)
            };

            edge.set_label(new_label);
        }
    }

    graph
}
/// Visualizes the finite automata provided
pub fn visualize<T: FA>(fa: &T) {
    let graph = generate_stable_graph(fa);
    run_native(
        "finite automata visualizer",
        NativeOptions::default(),
        Box::new(|cc| Ok(Box::new(Visualizer::new(cc, graph)))),
    )
    .unwrap();
}
