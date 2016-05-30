# FsMap3

### A Function Library for Procedural 3-D Textures

FsMap3 is a function and combinator library for 3-textures, which are formalized as self-maps in 3-space.

FsMap3Gui contains Windows GUIs for viewing, exploring and editing textures.

FsMap3 and FsMap3Gui are licensed under the terms of the MIT license.

FsMap3Gui includes some PNG format icons from the [Modern UI Icons](http://modernuiicons.com/) project. They are licensed separately - see the license file `ModernUiIconsLicense.txt`.

### State and Roadmap

We are in alpha. The first binary release of FsMap3 Editor is 0.3.0.

Plans for features include:

* Undo and redo.
* Crossover tool.
* Aspect ratio support to enable tiling in different aspect ratios. As we are dealing with 3-textures, aspect ratios are X:Y:Z instead of the usual X:Y.
* View aspect ratio setting.
* Native `.dds` export, both 2-D and 3-D.
* Screen space textures, i.e., 2-textures. Unconstrained 3-textures are very powerful but their planar slices are hard to control.
* Nicer node tree editing features.
* Multi-channel editing for, e.g., surface reflection models.
* Easy integration into C# and F# code so that saved texture descriptions can be unpacked during runtime, as part of a content pipeline, or during program installation.

### Features

* All textures are grid based. Thanks to a common layout scheme, they can be made to tile on any axis independently.
* Well known 3-textures such as Perlin noise and Worley basis, as well as some original concoctions.
* Highly modular architecture.
* SIMD support on Windows.
* A powerful set of combinators, including fractalizers and various mixing operations.
* A "Dna" system for generic procedural generation. Generated textures can be serialized in YAML or binary.
* Many color space transformations and a palette generator. Palettes in 3-space enable rich color schemes.
* A simple ray traced preview.
* Detail level filtering. Maps that are too dull or so detailed that they would look like pixel soup are automatically rejected.
