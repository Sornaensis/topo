# Simulation DAG

> **Module:** `Topo.Simulation.DAG` (407 LOC)
> **Status:** Stub

## Overview

Constructs and executes a directed acyclic graph of simulation nodes.
Each tick, the DAG executor runs all nodes in dependency order.

## Construction

`buildSimDAG` takes a list of `SimNode`s and:
1. Validates acyclicity
2. Topological sort
3. Groups into concurrent wavefronts

## Execution

`tickSimulation` per-tick:
1. Run concurrent **reader** wavefronts (overlay reads, no terrain writes)
2. Run sequential **writer** phase (terrain mutations)

## Determinism

Despite concurrent readers, execution is deterministic — readers only
read, and writers run sequentially in sorted order.
