# NammaCloud

This repository serves as the single source of truth for defining infrastructure in code. It enables reproducible deployments, type-safe infrastructure definitions, and streamlined operations through a declarative DSL powered by Rust's type system.

# Designing Principle: Stratum Protocol

A Unified Theory of System State. A DSL (Domain Specific Language) that creates a strict Stratified Ontology — a way of describing the world where complexity emerges from the rigid interaction of simpler, lower-level components.

It defines the philosophy, the strict boundaries, the type system, and applies it to three distinct domains (Data Center, Urban Planning, and Biology).

## 1. Abstract

Complex systems—whether digital, biological, or societal—fail not because of a lack of components, but due to a lack of structural integrity in their dependencies. We propose the **Stratum Protocol**, a strictly typed DSL designed to model systems as a series of immutable, unidirectional layers. This language enforces a rule where **Layer $N$** provides the physical reality for **Layer $N+1$**, while remaining agnostic to the existence of **Layer $N+1$**.

## 2. Core Philosophy & Axioms

To ensure the DSL is expressive enough for a Data Center yet abstract enough for Biology, we define three axioms:

### Axiom I: The Law of Agnosticism
**Lower layers are blind.**

Layer 0 (Physical) does not know that Layer 1 (OS) exists. A server rack does not care if it hosts Linux or Windows; it only cares about power and weight.

*   **Validation Rule**: A type definition in Layer $N$ cannot reference a type in Layer $N+1$.

### Axiom II: The Law of Dependency
**Upper layers are parasitic.**

Layer 1 cannot exist without a specific, validated state in Layer 0. A "Virtual Machine" (L2) cannot be instantiated unless a "Hypervisor" (L1) has validated a "CPU Core" (L0).

*   **Validation Rule**: Every object in Layer $N (>0)$ must adhere to a `binds_to` contract with an object in Layer $N-1$.

### Axiom III: The Law of Finite Capacity
**Abstraction does not create resources; it consumes them.**

Every layer introduces overhead. The sum of requirements at Layer $N$ can never exceed the provisioned capacity of Layer $N-1$.

## 3. The Type System

To make this DSL universal, we cannot use terms like "Server" or "City" in the core language. Instead, we define **Archetypes**.

### 3.1. The Primitives
*   **Substrate (The Context)**: Defines the environment boundaries (e.g., A single Data Center, A Petri Dish, An Island).
*   **Node (The Actor)**: A discrete entity exists in a layer (e.g., A Server, A Cell, A Human).
*   **Link (The Horizontal Bond)**: A connection between Nodes in the same layer (e.g., Cabling, Chemical Bond, Social Tie).

### 3.2. The Properties (Interface)
*   **Capacity (Output)**: What a Node provides to the layer above (e.g., Watts, CPU Cycles, Caloric Energy).
*   **Load (Input)**: What a Node consumes from the layer below.
*   **Constraint (The Rule)**: Boolean logic determining validity (e.g., `dimensions.width < rack.width`).

## 4. Implementation: Domain Modeling

Here is how the Stratum DSL models your three specific examples using the Type System defined above.

### Case Study A: The Data Center (Infrastructure)
**Goal**: Ensure physical fit and logical connectivity.

*   **Layer 0: The Physical (Hardware)**
    *   **Nodes**: Rack, BladeServer, PDU (Power Distribution Unit).
    *   **Links**: CopperCable (Port A to Port B).
    *   **Capacities**: SpaceUnit (U), Power (Watts), ThermalDissipation (BTU).
    *   **Validation**:
        $\displaystyle \sum_{i} Server_i.Weight < Rack.MaxWeight$
        $\displaystyle \forall Server, \exists Cable \text{ connecting } Server.NIC \to Switch.Port$

*   **Layer 1: The Executive (Operating System)**
    *   **Nodes**: Kernel, NetworkInterface, DiskPartition.
    *   **Binds To**: BladeServer (Layer 0).
    *   **Capacities**: SystemThreads, BlockStorage.
    *   **Validation**: Does the BladeServer have a physical NIC to support the NetworkInterface requested by the OS?

*   **Layer 2: The Service (Cloud/App)**
    *   **Nodes**: NovaInstance, NeutronNetwork, PostgresDB.
    *   **Binds To**: Kernel (Layer 1).
    *   **Validation**: The DSL checks if the OS (L1) has exposed enough generic SystemThreads to support the NovaInstance allocation.

### Case Study B: Urban Development (Maslow’s Hierarchy)
**Goal**: Ensure survival needs are met before social structures form.

*   **Layer 0: Nature (The Geography)**
    *   **Nodes**: LandMass, River, Forest, Animal.
    *   **Capacities**: ArableArea (Acres), WaterFlow (Liters/sec), PreyBiomass.
    *   **Validation**: Is the geography physically consistent? (Rivers flow downhill).

*   **Layer 1: Survival (The Settlement)**
    *   **Nodes**: Shelter, Farm, Hunter.
    *   **Binds To**: LandMass (Layer 0).
    *   **Constraint**: A Farm requires `ArableArea > 0` and `distance to River < 1km`.
    *   **Validation**:
        $\displaystyle \sum Humans.CaloricNeed < \sum Farms.CaloricOutput + \sum PreyBiomass$

*   **Layer 2: Society (The Economy)**
    *   **Nodes**: Market, School, Corporation.
    *   **Binds To**: Settlement (Layer 1).
    *   **Constraint**: A Corporation requires a cluster of Humans (from L1) with Skill attributes. A Market requires connectivity (roads) between Shelters.

### Case Study C: Cellular Biology
**Goal**: Emergence of life from non-life.

*   **Layer 0: The Molecular (Chemistry)**
    *   **Nodes**: Atom (C, H, O, N), Molecule (Amino Acid, Lipid).
    *   **Links**: CovalentBond, IonicBond.
    *   **Properties**: Electronegativity, Charge.
    *   **Validation**: Rules of Valence electrons.

*   **Layer 1: The Cellular (Life)**
    *   **Nodes**: Membrane, Mitochondria, Ribosome.
    *   **Binds To**: Aggregates of Molecules (Layer 0).
    *   **Validation**: A Membrane is valid only if formed by a phospholipid bilayer (L0 pattern) that creates a closed loop.

*   **Layer 2: The Organism (Tissue)**
    *   **Nodes**: MuscleTissue, Neuron, Organ.
    *   **Binds To**: Cell clusters (Layer 1).
    *   **Validation**: MuscleTissue requires specific energy input (ATP) from underlying Mitochondria (L1).

## 5. Benefits of Stratified Topology

By treating "Data Center Management" as a stratified topology problem identical to "Urban Planning" or "Biology," we gain robust validation.

*   **Isolation**: Changes in Layer 2 (App config) never require re-validation of Layer 0 (Cabling).
*   **Simulation**: We can simulate a "Power Failure" in Layer 0 and deterministically predict which "Corporations" or "Cloud Apps" in Layer 2 will die.
*   **Clarity**: The DSL serves as the single source of truth for the system's reality.



## Implementation

NammaCloud uses a **type-safe, Rust-based Domain-Specific Language** for defining datacenter infrastructure. The DSL provides a declarative way to define your infrastructure from Layer 0 (hardware) through Layer 1 (OS) and beyond.

**Core DSL Location:** The core constructs of the entire DSL are located in [`crates/namma-core/`](./crates/namma-core/). This folder contains all the fundamental types, builders, and the Infrastructure IR that powers the declarative infrastructure definition.

### Quick Start with the DSL

```bash
# Build the project
cargo build --release

# Run the example to generate infrastructure
cd examples/nxtgen-dc
cargo run

# Use the CLI to explore infrastructure
./target/release/namma-cloud-cli layer0 list-racks
./target/release/namma-cloud-cli layer1 list-hosts
```

See [`namma-dsl.md`](./namma-dsl.md) for complete DSL documentation.

## DSL Architecture & Implementation

### Core Design: Path-Based References

The DSL uses **typed paths** instead of Rust references with lifetimes to enable cross-layer references:

```rust
// Layer 1 stores a path to Layer 0 machine
pub struct Host {
    machine_path: MachinePath,  // "R1.M1" - typed string wrapper
}

// Resolve through IR when needed
let machine = ir.resolve_machine_path(&host.machine_path)?;
```

**Benefits:**
- No lifetime parameters - all types are plain structs
- Fully serializable with `#[derive(Serialize, Deserialize)]`
- Type-safe paths (can't mix MachinePath with HostPath)
- CLI can load IR without recompilation

### Infrastructure Macro

The `infrastructure!` macro provides declarative, identifier-based syntax:

```rust
infrastructure! {
    Datacenter NxtGen {
        Rack R1 size=42 {
            Machine M1 {
                name: "Dell PowerEdge R740"
                kind: Compute
                cores: 64
                memory: 512
            }
        }

        Layer1 {
            Host compute1 {
                machine: "R1.M1"  // Path references use strings
                kind: Compute
                os: Ubuntu 24.04
            }
        }
    }
}?
```

**Key Features:**
- Identifier-based IDs (`Machine M1` not `Machine "M1"`)
- String-based paths for cross-references (supports dots: `"R1.M1"`)
- Expands to builder pattern code at compile-time
- Returns `Result<InfrastructureIR, Error>`

### Layer Architecture

#### Layer 0: Hardware (Implemented)

```rust
pub struct Datacenter {
    name: String,
    racks: Vec<Rack>,
}

pub struct Rack {
    id: String,
    size: u8,              // Rack units (1-42)
    machines: Vec<Machine>,
}

pub struct Machine {
    id: String,
    name: String,
    machine_type: MachineType,
    cores: u32,
    memory_gb: u32,
}

pub enum MachineType {
    Compute,
    Storage,
    Network,
}
```

#### Layer 1: Operating System (Implemented)

```rust
pub struct Host {
    id: String,
    machine_path: MachinePath,  // Path to Layer 0 machine
    host_type: HostType,
    os: OperatingSystem,
}

pub struct OperatingSystem {
    name: String,
    version: String,
}

// Type-safe path wrapper
#[derive(Serialize, Deserialize)]
pub struct MachinePath(String);
```

#### Intermediate Representation (Implemented)

```rust
#[derive(Serialize, Deserialize)]
pub struct InfrastructureIR {
    pub layer0: Datacenter,
    pub layer1: Option<Layer1>,
    pub layer2: Option<Layer2>,  // Planned
}

impl InfrastructureIR {
    pub fn resolve_machine_path(&self, path: &MachinePath) -> Result<&Machine, Error> {
        // Parse "R1.M1" and find machine in datacenter
    }
}
```

## Repository Structure

```
namma-cloud/
├── crates/                          # Rust workspace crates
│   ├── namma-core/                  # CORE DSL CONSTRUCTS - All fundamental types, builders, IR
│   │   ├── src/layer0.rs            # Datacenter, Rack, Machine types
│   │   ├── src/layer1.rs            # Host, OS, MachinePath types
│   │   ├── src/ir.rs                # InfrastructureIR with path resolution
│   │   ├── src/errors.rs            # Error types
│   │   └── src/lib.rs               # Public API exports
│   │
│   ├── namma-cli/                   # CLI tool for infrastructure exploration
│   │   └── src/main.rs              # CLI entry point
│   │
│   └── namma-macros/                # Procedural macros for DSL
│       └── src/lib.rs               # infrastructure! macro implementation
│
├── examples/                        # Example infrastructure definitions
│   └── nxtgen-dc/                  # Sample datacenter definition
│       └── src/main.rs             # Uses infrastructure! macro
│
├── nix/                            # Nix flake configuration
│   └── modules/                    # Nix modules
│       ├── rust.nix                # Rust project configuration
│       ├── devshell.nix            # Development shell
│       └── pre-commit.nix          # Pre-commit hooks
│
├── Cargo.toml                      # Workspace configuration
├── Cargo.lock                      # Dependency lock file
├── flake.nix                       # Nix flake entry point
├── justfile                        # Just command runner recipes
├── namma-dsl.md                    # Complete DSL documentation
├── STATUS.md                       # Implementation status
└── README.md                       # This file
```

## Design Principles

1. **Types as Proofs**: Leverage Rust's type system to make invalid configurations unrepresentable
2. **Single Source of Truth**: DSL declarations define canonical state
3. **Less Verbose**: Identifier-based syntax reduces cognitive load
4. **Layer Isolation**: Lower layers never depend on upper layers
5. **Path-Based References**: Type-safe without lifetime complexity

## Quick Reference

### Build & Run DSL

```bash
# Build the project
cargo build --release

# Run the example
cd examples/nxtgen-dc && cargo run

# Use the CLI
./target/release/namma-cloud-cli layer0 list-racks
./target/release/namma-cloud-cli layer1 describe-host compute1
```

### Nix Development Environment

```bash
# Enter nix dev shell
nix develop

# Run with just
just build
just run
```

## Components

| Component | Description |
|-----------|-------------|
| [crates/namma-core/](./crates/namma-core/) | **Core DSL constructs** - All fundamental types, builders, and Infrastructure IR |
| [crates/namma-cli/](./crates/namma-cli/) | CLI tool for infrastructure exploration |
| [crates/namma-macros/](./crates/namma-macros/) | Procedural macros for declarative DSL syntax |
| [examples/nxtgen-dc/](./examples/nxtgen-dc/) | Example datacenter definition |
| [namma-dsl.md](./namma-dsl.md) | Complete DSL documentation |
| [STATUS.md](./STATUS.md) | Implementation status and roadmap |

## Architecture Overview

```
┌─────────────────────────────────────────────────────────────┐
│                      NammaCloud DSL                        │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│  ┌──────────────────────────────────────────────────────┐  │
│  │              Rust DSL Infrastructure Definition      │  │
│  │    Layer 0: Hardware → Layer 1: OS → Layer 2: ...  │  │
│  │         (defined in crates/namma-core/)              │  │
│  └──────────────────────────────────────────────────────┘  │
│                          │                                  │
│                          ▼                                  │
│  ┌──────────────────────────────────────────────────────┐  │
│  │           Infrastructure IR (Serializable)           │  │
│  │         - Path resolution                            │  │
│  │         - JSON/YAML export                           │  │
│  └──────────────────────────────────────────────────────┘  │
│                          │                                  │
│                          ▼                                  │
│  ┌──────────────────────────────────────────────────────┐  │
│  │                   CLI & Tools                         │  │
│  │         - Query infrastructure                       │  │
│  │         - Generate configs                           │  │
│  └──────────────────────────────────────────────────────┘  │
│                                                             │
└─────────────────────────────────────────────────────────────┘
```

## Getting Started

### First-Time Setup

```bash
# Clone the repository
git clone <repo-url>
cd namma-cloud

# Build the project
cargo build --release

# Run the example
cd examples/nxtgen-dc
cargo run
```

### Define Your Infrastructure

Edit `examples/nxtgen-dc/src/main.rs` to define your datacenter:

```rust
use namma_core::infrastructure;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let ir = infrastructure! {
        Datacenter MyDC {
            Rack R1 size=42 {
                Machine M1 {
                    name: "Dell PowerEdge R740"
                    kind: Compute
                    cores: 64
                    memory: 512
                }
            }

            Layer1 {
                Host compute1 {
                    machine: "R1.M1"
                    kind: Compute
                    os: Ubuntu 24.04
                }
            }
        }
    }?;

    // Save to JSON
    let json = serde_json::to_string_pretty(&ir)?;
    std::fs::write("infrastructure.json", json)?;

    Ok(())
}
```

## Contributing

This repository follows Rust best practices:

1. Run `cargo fmt` before committing
2. Run `cargo clippy` to catch lints
3. Add tests for new functionality
4. Update documentation for public APIs
5. Use the DSL for all infrastructure definitions

## Naming Conventions

- **Folders**: lowercase with hyphens (e.g., `namma-core`, `namma-cli`)
- **Files**: lowercase with hyphens (e.g., `layer0.rs`, `layer1.rs`)
- **Rust types**: CamelCase (e.g., `Datacenter`, `MachineBuilder`)
- **Rust functions**: snake_case (e.g., `resolve_machine_path`, `build`)
- **Macro identifiers**: PascalCase (e.g., `infrastructure!`, `Datacenter`, `Rack`)

## License

Internal Use Only - NammaCloud