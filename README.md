# kewar
(pronounced qr)

Generate Kewar code from given input string

[![MIT license](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

## Installation

```
cabal install exe:kewar

# or

cabal install lib:kewar
```

## CLI

### Example

```bash
$ kewar --error-correction=H "Hello World"
```

### Usage 

```bash
$ kewar [options] [INPUT]
```

kewar is a tool to generate QR codes from any supported string,
utilizing an error correction level to allow data recovery.

For more information on QR codes: https://www.qrcode.com/en/

### Options

```
  -v         --version                   print qr version
  -h         --help                      print this help
  -e[LEVEL]  --error-correction[=LEVEL]  use error correction LEVEL for encoding. Defaults to Q
```

## Library

```haskell
module Main where

import Kewar (generate, CorrectionLevel(Q))
import MyModule (doStuff)

main = do
  let input = "my string"
  case generate input Q of
    Left e -> print e
    Right grid -> doStuff grid
```