# Infrastructure Algebra Design Document

This document outlines the design principles and layered architecture of the `infra_algebra` project. The goal is to treat infrastructure not as a set of scripts or procedures, but as a rigorous mathematical object with strict layers of abstraction.

## Core Philosophy

**"Infrastructure as a State Space"**

Instead of defining infrastructure by *how* to build it (Terraform, Ansible), we define *what* it is using algebraic data types. This allows us to:
1.  **Make Invalid States Unrepresentable:** By designing types that enforce constraints.
2.  **Separate Concerns via Layering:** Hardware physics is distinct from software configuration.
3.  **Validate without Execution:** We can mathematically prove a design is valid before buying a single cable.

## Layered Architecture

The system is built on strict, unidirectional layers. Higher layers depend on lower layers, but lower layers are oblivious to higher ones.

### Layer 0: The Physical Layer (`layer0.hs`)

**"The Territory" - Immutable Physics**

This layer models the physical reality of the datacenter. It deals with atoms, electricity, and geometry.

*   **Purpose:** To define the physical constraints that cannot be violated (e.g., a 1U server cannot fit in 0U space, a cable cannot connect to a non-existent port).
*   **Key Types:**
    *   `Datacenter`: The root aggregate.
    *   `Rack`: Represents physical space constraints (42U height).
    *   `Machine` / `Switch`: Physical devices identified by `SerialNumber`.
    *   `Link` / `Cable`: Physical connectivity.
    *   `Wattage`: Power constraints.
*   **Abstraction Principle:** If you can kick it, it belongs in Layer 0.
*   **Constraints:**
    *   **Geometry:** Sum of device heights $\le$ Rack capacity.
    *   **Physics:** Sum of device power draw $\le$ PDU capacity.
    *   **Topology:** Cables must connect valid ports (referenced by Slot/Port Number).

### Layer 1: The Host/OS Layer (`layer1.hs`)

**"The Platform" - Software Identity**

This layer models the software environment running *on top of* Layer 0. It bridges the gap between metal and application.

*   **Purpose:** To assign logical identity (Hostname, IP) to physical entities (Serial Number, MAC).
*   **Key Types:**
    *   `HostSystem`: A logical server instance (e.g., "controller-01").
    *   `OSType`: The operating system (Linux, Windows).
    *   `LogicalInterface`: Binds an IP address to a Layer 0 `NetworkInterface` (via MAC address).
    *   `Process`: Software running on the OS.
*   **Abstraction Principle:** Software that requires a kernel to exist belongs in Layer 1.
*   **Connection to Layer 0:**
    *   Layer 1 objects *reference* Layer 0 objects via stable identifiers (Serial Numbers, MAC Addresses).
    *   *Example:* `HostSystem.hostHardwareRef` points to `Machine.machineSerial`.

## Design Principles for Management & Constraints

### 1. Separation of Volatility
*   **Layer 0 (Hardware)** changes slowly (months/years). It is the stable foundation.
*   **Layer 1 (OS)** changes moderately (days/weeks).
*   **Future Layers** (Apps/Containers) change rapidly (minutes/seconds).
*   **Benefit:** We can re-provision Layer 1 (reimage a server) without "destroying" Layer 0 in our model. The hardware remains modeled even if the OS is wiped.

### 2. Referential Integrity as a Constraint
Instead of hard-coding relationships, we use references (Foreign Keys in database terms).
*   A `LogicalInterface` in Layer 1 claims to own a specific MAC address.
*   **Constraint:** That MAC address *must* exist in Layer 0. If you try to assign an IP to a non-existent NIC, the system is invalid.

### 3. Capacity Planning as Algebra
Because Layer 0 is strongly typed with physical properties:
*   **Capacity Planning** becomes a simple summation function.
*   `RemainingPower = PDU.maxWatts - sum(Machine.psu.maxWatts)`
*   This is not an estimate; it is a derived property of the state.

## Summary of Types

| Concept | Layer 0 (Physical) | Layer 1 (Logical/OS) |
| :--- | :--- | :--- |
| **Identity** | Serial Number, Asset Tag | Hostname, UUID |
| **Network** | Port, MAC Address, Cable | IP Address, Interface Name, Bond |
| **Compute** | CPU Cores, RAM Stick | Kernel Scheduler, Processes |
| **Storage** | Physical Disk (NVMe/HDD) | Filesystem, Mount Point |
| **State** | Power On/Off, Broken | Active, Maintenance, Zombie |

## Why this matters?

By formalizing these layers:
1.  **Auditing:** We can query "Which physical rack contains the host running 'critical-db'?" by joining Layer 1 $\to$ Layer 0.
2.  **Safety:** We prevent logical errors (assigning 100 servers to a 42U rack) at the design phase.
3.  **Clarity:** Developers understand exactly what they are manipulating—a physical box or a software process.
