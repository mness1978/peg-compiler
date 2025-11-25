# PEG Compiler

A compiler that transforms PEG (Parsing Expression Grammar) grammar files into bytecode for execution by the PEG VM.

## Contents

- `parser.c` / `parser.h` - PEG parser implementation (based on peg.c from peg-0.1.19)
- `compiler.c` / `compiler.h` - Bytecode compiler
- `pegcc.c` - Command-line compiler tool

## Building

```bash
make
```

This will build the `pegcc` executable.

## Usage

Compile a PEG grammar file to bytecode:

```bash
./pegcc <grammar.peg> -o <output.pbc>
```

## Example

```bash
./pegcc ../amos/amos_with_actions.peg -o ../amos/amos_with_actions.pbc
```
