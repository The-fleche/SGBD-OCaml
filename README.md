# 🗄️ OCaml Relational Database & Normalization Engine

## 📖 Project Overview
This project is a relational database management system built entirely in **OCaml** as part of a functional programming module. It provides a complete engine for manipulating relational data and performing automated logical analysis to evaluate a table's **normalization level** (1NF, 2NF, and 3NF).

The goal was to move beyond simple recursive exercises and build a complex system that simulates real-world database logic using functional paradigms.

> 📑 **Looking for the full technical analysis?** (written in french) [Read the Project Report (PDF)](./rapport_projet_ipfl12.pdf)

## ⚙️ How it Works (Core Logic)

### 1. The Relational Data Model
The system represents a table as a record containing a schema and data rows.
* **dbvalue**: The atomic unit of data, represented as a sum type: `VInt`, `VText`, or `VNull`.
* **Schema**: A list of pairs defining column names, types, and nullability constraints.

```ocaml
type dbvalue = VInt of int | VText of string | VNull 
type row = dbvalue list
type table = { cols : schema; rows : row list }
```

### 2. Relational Algebra Operations
The engine implements standard operations to transform and query data:
* **Projection**: Reconstructs rows by extracting and reordering columns according to user-specified indexes.
* **Restriction**: Applies predicates to filter the `rows` list.
* **Cartesian Product**: Merges two tables by generating all possible combinations.

### 3. Normalization Analysis (Logic Engine)
The core "intelligence" of the project lies in its ability to evaluate structural integrity:
* **Functional Dependencies ($X \rightarrow Y$)**: The engine compares every pair of rows. If they are equal on columns $X$, they must be equal on $Y$.
* **Candidate Keys**: Identifies minimal sets of columns that uniquely determine the entire table.
* **Normalization Levels**:
    * **1NF by Design**: By using lists of atomic sum types, the system natively forbids multi-valued attributes or nested tables.
    * **2NF**: Validated if there are no partial dependencies.
    * **3NF**: Validated if there are no transitive dependencies.

## 🛠 Technologies & Tools
- **Language:** OCaml (Functional Paradigm).
- **Core Library:** Standard library (List.filter, List.for_all, List.concat_map).
- **Logic:** Relational Algebra & Normalization Theory.

## 🧠 What I Learned 

Building this engine was a deep dive into functional architecture. It taught me:
- **Design Autonomy**: Being given a free-form project forced me to architect my own internal structure and auxiliary functions from scratch.
- **Complexity Management**: I learned to optimize "Brute force" algorithms for functional dependencies by using elementary dependencies and subset analysis.
- **Data Integrity**: I implemented "fail-fast" mechanisms using OCaml exceptions to prevent corrupted data from entering the normalization pipeline.
- **Functional Power**: I realized that OCaml is suited for high-level, complex, and structured systems, not just isolated recursive functions.

## How to run the project
**Compile the engine:**
   ```bash
   make
   ```
