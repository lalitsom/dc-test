# Infra-DSL: A Denotational Language for Infrastructure

Infrastructure is not a sequence of commands. It is a constrained mathematical object.

## Motivation

Most infrastructure tools treat infrastructure as procedures: scripts that mutate reality step by step.

This project starts from a different premise:
> Infrastructure is a state space, not a program.

A correct infrastructure system should therefore be:
- Describable without execution
- Validatable without deployment
- Reasonable using algebra, not logs

This repository explores a domain-specific language (DSL) for infrastructure whose semantics are defined mathematically before any syntax, planner, or executor exists.

## Core Philosophy

The design follows three non-negotiable principles:
1. Semantics precede syntax
2. Invalid states should be unrepresentable or unreachable
3. Execution is a compilation artifact, not the language

If a concept cannot be stated as a function, predicate, or relation, it does not belong in the core.

## Formal Model

The DSL is defined as a mathematical structure: `(S, V, A, apply)`

Where:

### 1. State Space (`S`)
`S` = All representable infrastructure states

A state is pure data:
- Machines
- Networks
- Volumes
- Relations between them

There is no behavior in the model.

### 2. Valid States (`V ⊆ S`)
`V = { s ∈ S | valid(s) }`

Validity is defined by invariants, e.g.:
- Every machine references an existing network
- A volume is attached to at most one machine
- No dangling references exist

These invariants define the geometry of the state space.

### 3. Actions (`A`)
Actions are partial functions: `a: V ⇀ V`

Key properties:
- Actions may be undefined
- If defined, they preserve invariants
- Actions are not scripts; they are state transformations

Examples:
- `CreateMachine`
- `CreateNetwork`
- `AttachVolume`

Actions are intentionally small and local.

### 4. Closure Property
For all actions `a` and valid states `s`:
`a(s) defined ⇒ a(s) ∈ V`

This is the infrastructure analogue of type safety:
**Well-formed changes cannot produce invalid infrastructure.**

## Composition and Algebra

Actions compose via function composition:
`(a ∘ b)(s) = a(b(s))`

Properties:
- Associative where defined
- Partial
- Not invertible in general

This forms a partial algebra, not a group.
Idempotence and commutativity are desirable, not assumed.

## Declarative Semantics

Users do not write actions. They describe a desired state `s_d ∈ V`.

A planner (outside the DSL core) computes:
`plan(s_c, s_d) = [a_1, a_2, ..., a_n]`

Execution is merely:
`a_n ∘ ... ∘ a_1(s_c)`

The DSL itself is meaningful without planning or execution.

## Why Not a State Machine?

State machines are an implementation, not a foundation. This DSL is more naturally viewed as:
- A constraint system
- A graph rewrite system
- A monotone transformation over a partial order

State machines emerge only at execution time.

## Implementation (Rust)

Rust is used deliberately:
- Algebraic data types
- Explicit partiality (`Option`)
- No hidden side effects
- No runtime mutation of global state

The code mirrors the math:
- Models = structs
- Invariants = predicates
- Actions = pure functions

No parser, no YAML, no syntax bikeshedding.

## What This Is Not

- Not a configuration language
- Not a scripting tool
- Not a Terraform replacement
- Not “infrastructure as code”

It is **infrastructure as mathematics**.

## Design Constraints

- No hidden global state
- No implicit ordering
- No magic defaults
- No execution logic in the core

If something feels “convenient” but obscures semantics, it is rejected.

## Roadmap (Derived, Not Assumed)

Future layers—derived from the core, not baked in:
- State diffing: `S × S → A*`
- Planning as search
- Policy vs hard invariants
- Typed invariants using Rust’s type system
- Optional surface syntax

Each layer must preserve the core semantics.

## Final Note

This project is an experiment in taste.

It asks:
> What would infrastructure tools look like if designed by someone who cares about algebra, invariants, and meaning?

If you find this uncomfortable or “over-theoretical”, that is intentional.

**Beauty comes from restriction.**
