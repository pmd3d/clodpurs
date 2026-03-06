# clod--

An F# rewrite of [nqcc2](https://github.com/nlsandler/nqcc2), implementing a C compiler based on Nora Sandler's book [Writing a C Compiler](https://nostarch.com/writing-c-compiler).

This implementation does not include extra credit features or the optimizations from chapters 19 and 20.

## Prerequisites

- .NET 8 SDK
- gcc (for linking and assembler)
- Linux or WSL

## Build and Run

```bash
git clone https://github.com/pmd3d/clod--.git
cd clod--/src/
dotnet run
```

## Publish Standalone Executable

WSL:
```bash
dotnet publish -c Release --self-contained -p:PublishSingleFile=true
```

Linux:
```bash
dotnet publish -c Release --self-contained -p:PublishSingleFile=true -r linux-x64 -p:UseAppHost=true
```
