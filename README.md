# FsMap3
### A Function Library for Procedural 3-D Textures

FsMap3 is a function and combinator library for 3-textures, which are formalized as self-maps in 3-space.

FsMap3Gui is a Windows GUI for creating and exploring textures.

FsMap3 and FsMap3Gui are licensed under the terms of the MIT license.

FsMap3Gui includes some PNG format icons from the [Modern UI Icons](http://modernuiicons.com/) project. They are licensed separately - see the license file `ModernUiIconsLicense.txt`.


### Features

* All textures are cell based. Thanks to a common layout scheme, they can be made to tile on any axis independently.
* Many well known 3-textures such as Perlin noise and Worley basis, as well as some of my own concoctions.
* Highly modular architecture.
* A versatile set of combinators, including fractalizers and various mixing operations.
* A "Dna" system for generic procedural generation. Generated textures can be serialized in YAML or binary.
* Many color space transformations and a palette generator. Palettes in 3-space enable rich color schemes.
