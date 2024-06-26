<h1 align="center">
  <br>
  <img src="images/logo.png" alt="Polydraw" width="200">
  <br>
  Polydraw
  <br>
</h1>

<h4 align="center">
Library for generating 3D models via a Haskell EDSL <a href="https://openscad.org/">OpenSCAD</a>.
</h4>

<hr />

# Graphics.Polydraw

[![Built with Nix][builtwithnix-badge]][builtwithnix] [[https://img.shields.io/badge/built%20with-Haskell-8f4e8b.svg]]

# 🚀&nbsp; Installation

## Using nix flake

There needs to be an input:
```nix
inputs = {
    ...
    polydraw.url = "github:dsunshi/polydraw";
    ...
};
```

Then we can add the package:
```nix
haskellProjects.default = {
    packages = {
        polydraw.source = inputs.polydraw;
    };
};
```

### Example flake.nix

The following is the content of a `flake.nix` file, which can be used to build a
project using polydraw.
```nix
{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    polydraw.url = "github:dsunshi/polydraw";
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [ inputs.haskell-flake.flakeModule ];

      perSystem = { self', pkgs, ... }: {

        haskellProjects.default = {
            packages = {
                polydraw.source = inputs.polydraw;
            };
        };

        # haskell-flake doesn't set the default package, but you can do it here.
        packages.default = self'.packages.<your-package-name>;
      };
    };
}
```


## ❤️&nbsp; Origin from Graphics.Polydraw

Polydraw is primarily a fork of the brilliant [Graphics.OpenSCAD](https://hackage.haskell.org/package/OpenSCAD-0.2.1.0/docs/Graphics-OpenSCAD.html).
It has been updated to more modern versions of Haskell (as well as my personal opinions).
This project is in very early stages and is subject to change.

### How?
This library provides an algebraic data type for describing [OpenSCAD](http://openscad.org) models, and a function
that converts that data type into a string.

## More info

 - [Graphics.OpenSCAD](https://hackage.haskell.org/package/OpenSCAD-0.2.1.0/docs/Graphics-OpenSCAD.html)
 - [OpenSCAD](http://openscad.org)

[builtwithnix-badge]: https://img.shields.io/badge/builtwith-nix-7d81f7?logo=nixos&logoColor=white&style=flat-square
[builtwithnix]: https://builtwithnix.org/

## 📘&nbsp; License

[![License](https://img.shields.io/badge/license-MIT-blue.svg)](/LICENSE)
