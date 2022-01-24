# qr
Generate QR code from given input [version 0.1.0.0]

[![MIT license](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

## Installation

```
cabal install qr
```

## CLI

### Example

```bash
$ qr --error-correction=H "Hello World"
```

### Usage 

```bash
$ qr [options] [INPUT]
```

qr is a tool to generate QR codes from any supported string,
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

import QR (generate, CorrectionLevel(Q))
import MyModule (doStuff)

main = do
  let input = "my string"
  case generate input Q of
    Left e -> print e
    Right grid -> doStuff grid
```