# Alloy2Cs (Alloy to C#)
This project intends to contribute to the AlloyMDA project, adding support for transforming Alloy models into C# models to be used with Microsoft's official ORM, Entity Framework (EF).

The generated C# code is ready to be used in a project with Entity Framework Core using the Code First approach in order to create the structure of the database through Migrations.

## How to use Alloy2Cs
1. Get [Alloy2Cs](https://github.com/nanunintan/Alloy2Cs.git) (this already includes the AlloyMDA project)
1. Download and install [Haskell](https://www.haskell.org/platform/)
1. Get [Cabal's](https://www.haskell.org/cabal/) latest list of packages by running `cabal update`. Cabal is a system for building and packaging Haskell libraries and programs.
1. Open a new terminal window and go to the project folder
1. Run `cabal sandbox init` to initialize a sandbox environment to avoid package conflicts between different projects.
1. Install dependencies with `cabal install mtl syb parsec`.
1. Open the `Makefile` file and copy the appropriate command to create the executable. Then run `ghc --make Tools/Alloy2Cs.hs -o alloy2cs`.
1. Run the tool: `alloy2cs < Alloy/Examples/Book.als`

## Supported EF versions
Entity Framework (EF) is the Microsoft's official ORM tool. There are currently two versions:
- Entity Framework 6 (EF6), a tested, stable data access technology with lots of features.
- Entity Framework Core (EF Core), a lightweight, extensible, and cross-platform version of Entity Framework.

Alloy2Cs is mainly designed to be used with EF Core, however it could easily be used with EF 6 since both implement the same basic class mapping.

It means Alloy2Cs could be used with:
- EF Core 1.x
- EF Core 2.0.1 (current version at the time of publication) as well as all previous versions.
- EF 6. In this case, it may be necessary to add additional configuration settings before running the migrations to create the database.

## Alloy2Cs is made in Haskell
Alloy2Cs is an extension of the existing AlloyMDA project. Hence it's also written in Haskell.

## Alloy MDA
[AlloyMDA](https://sourceforge.net/p/alloymda/wiki/Home/) intends to develop tools to enable MDA support for Alloy, a formal modeling language.