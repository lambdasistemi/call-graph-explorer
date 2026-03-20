var _cy = null;

var _style = [
  {
    selector: "node",
    style: {
      label: "data(label)",
      "text-valign": "center",
      "text-halign": "center",
      "font-size": "12px",
      "font-family": "monospace",
      "background-color": "#4a90d9",
      color: "#fff",
      "text-outline-color": "#4a90d9",
      "text-outline-width": 1.5,
      width: "label",
      height: "label",
      padding: "6px",
      shape: "round-rectangle",
    },
  },
  {
    selector: "node.module",
    style: {
      "background-color": "#1a1a2e",
      "background-opacity": 0.15,
      "border-width": 2,
      "border-color": "#4a90d9",
      "border-opacity": 0.6,
      "text-valign": "top",
      "text-halign": "center",
      "font-size": "16px",
      "font-weight": "bold",
      color: "#4a90d9",
      "text-outline-width": 0,
      padding: "20px",
      shape: "round-rectangle",
    },
  },
  {
    selector: "node.type",
    style: {
      "background-color": "#8e44ad",
      "text-outline-color": "#8e44ad",
      "font-size": "13px",
      "font-weight": "bold",
      shape: "diamond",
      padding: "8px",
    },
  },
  {
    selector: "node.function",
    style: {
      "background-color": "#27ae60",
      "text-outline-color": "#27ae60",
      shape: "ellipse",
    },
  },
  {
    selector: "node.constructor",
    style: {
      "background-color": "#e67e22",
      "text-outline-color": "#e67e22",
      shape: "rectangle",
      "font-size": "10px",
    },
  },
  {
    selector: "node.field",
    style: {
      "background-color": "#3498db",
      "text-outline-color": "#3498db",
      shape: "round-rectangle",
      "font-size": "10px",
    },
  },
  {
    selector: "edge",
    style: {
      width: 0.8,
      "line-color": "#555",
      "target-arrow-color": "#555",
      "target-arrow-shape": "triangle",
      "curve-style": "bezier",
      "arrow-scale": 0.5,
      opacity: 0.5,
    },
  },
  {
    selector: "edge.type-edge",
    style: {
      "line-style": "dotted",
      "line-color": "#7f4a9e",
      "target-arrow-color": "#7f4a9e",
      opacity: 0.2,
    },
  },
  {
    selector: "node.root",
    style: {
      "border-width": 3,
      "border-color": "#e74c3c",
    },
  },
];

function runLayout() {
  if (!_cy) return;
  try {
    _cy
      .layout({
        name: "elk",
        elk: {
          algorithm: "layered",
          "elk.direction": "DOWN",
          "elk.layered.spacing.nodeNodeBetweenLayers": "60",
          "elk.spacing.nodeNode": "25",
          "elk.hierarchyHandling": "INCLUDE_CHILDREN",
        },
        fit: true,
        padding: 30,
      })
      .run();
  } catch (e) {
    _cy
      .layout({
        name: "cose",
        fit: true,
        padding: 30,
        animate: false,
      })
      .run();
  }
}

// Create an empty Cytoscape instance.
export const initCytoscape = (containerId) => () => {
  var container = document.getElementById(containerId);
  if (!container) return;
  if (_cy) {
    _cy.destroy();
    _cy = null;
  }
  _cy = cytoscape({
    container: container,
    elements: [],
    style: _style,
    layout: { name: "preset" },
    wheelSensitivity: 0.3,
    minZoom: 0.1,
    maxZoom: 5,
  });
};

// Replace all elements and re-layout.
export const setElements = (elements) => () => {
  if (!_cy) return;
  _cy.elements().remove();
  _cy.add(elements);
  runLayout();
};

// Register a callback for tap on a non-module node.
// Callback receives the node ID.
export const onNodeTap = (callback) => () => {
  if (!_cy) return;
  _cy.on("tap", "node[!parent]", function (evt) {
    // tap on module → ignore
  });
  _cy.on("tap", "node[parent]", function (evt) {
    callback(evt.target.id())();
  });
};

// Mark a node visually as the focus root.
export const markRoot = (nodeId) => () => {
  if (!_cy) return;
  _cy.nodes().removeClass("root");
  var node = _cy.getElementById(nodeId);
  if (node.nonempty()) node.addClass("root");
};

// Clear root marking.
export const clearRoot = () => {
  if (!_cy) return;
  _cy.nodes().removeClass("root");
};

// Fit viewport to all elements.
export const fitAll = () => {
  if (!_cy) return;
  _cy.animate({ fit: { padding: 30 }, duration: 300 });
};
