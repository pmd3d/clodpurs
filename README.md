A purescript rewrite of [nqcc2](https://github.com/nlsandler/nqcc2), implementing a C compiler based on Nora Sandler's book [Writing a C Compiler](https://nostarch.com/writing-c-compiler).

This implementation does not include extra credit features or the optimizations from chapters 19 and 20.

## Prerequisites

- node
- purescript
- spago
- gcc (for linking and assembler)
- linux

## Build and Run

```bash
npx spago build
npx spago run
```

### Run tests

```bash
npx spago test
```
