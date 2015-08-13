## Quick test of a network-building GUI.

The goal is to explore using FRP to handle two layers simultaneously:

- The domain layer in this case is a simple DAG of nodes that exchange boolean values over links.  The nodes and their links are modeled as Dynamics, so state is recomputed automatically as nodes are toggled by the user.

- On top of that, we have a GUI layer allowing the user to construct and alter the network.  The data structure representing the network topology (what are the nodes, what are their links) is itself Dynamic.

The result is that the network is represented as a Dynamic containing Dynamics.  Although we could flatten this into a single Dynamic value representing the current state of both the network topology and node state, leaving them layered like this seems to result in pretty nice code.

The rendering, on the other hand, is currently very ugly.
