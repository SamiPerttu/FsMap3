# FsMap3 Editor User Guide

** Table of Contents **

[TOC]

## Introduction

**FsMap3 Editor** is a Windows GUI for creating and exploring *[procedural textures](http://en.wikipedia.org/wiki/Procedural_texture)*. Texture descriptions can be edited, loaded and saved, exported as images, randomized, mutated and evolved. This user guide helps you get started.

At the time of this writing **FsMap3** is considered to be in alpha, which means that things are still changing rapidly. Backward compatibility of saved textures is not guaranteed until we move to beta.

### What Can You Do With It?

* Create wallpapers, abstract art and repeating backgrounds and patterns.
* Create textures for games that can be stored in procedural form and unpacked during installation. Procedural descriptions typically fit within 1 kB, promising ridiculous compression ratios.
* Create *ad hoc* graphics for your .NET program that are inserted directly as source code and unpacked when needed.

## What Are Procedural Textures?

Procedural textures are images created by an algorithm from a simple description. The textures in **FsMap3** are *3-dimensional*. 3-textures are also called *solid textures* because they can be likened to solid slabs of material.

Because solid textures associate a color with every point in *3-space*, they can texture any object, no matter how complicated its surface.

Technically, a **3-texture** is a box that accepts 3 numbers (**X**, **Y** and **Z**), processes them somehow, and cranks out another 3 numbers. The numbers that come out of the box can be interpreted as a color (for instance, in RGB format) and displayed, or they can be interpreted as another set of coordinates and forwarded to some other box for further processing.

These boxes are typically called **nodes** in texture editors. A complete procedural texture is built by connecting several nodes in a tree or graph.

### Examples

Here are some example images. Each image is a 2-dimensional cross section of a 3-texture created with **FsMap3**.

|   |   |
|---|---|
|<img src="examples/fireplace.jpg" alt="Fireplace" style="width: 320px;"/> | <img src="examples/clouds.jpg" alt="Clouds" style="width: 320px;"/>|
|<img src="examples/jupiter.jpg" alt="Jupiter" style="width: 320px;"/> | <img src="examples/haze.jpg" alt="Haze" style="width: 320px;"/>|
|<img src="examples/Cyclones.jpg" alt="Noises/Cyclones" style="width: 320px;"/> | <img src="examples/Colony.jpg" alt="Noises/Colony" style="width: 320px;"/>|


## A Look at the User Interface

Below is an example screenshot with the main sections delineated.

![Main sections of the user interface.](quickLook.png)

The user interface is divided horizontally into three parts: *parameters*, *control panel*, and *texture view*.

### Parameter Panel

The parameter panel contains all **parameters** of the texture under view. Together, the parameters completely define what the texture looks like. They are divided into three parts:

* **Palette** defines the color scheme of the texture. At each point in space, the final 3 values that emerge from the texture are colored according to the palette.

* **Global parameters** consists of just the layout for now. In the future, other parameters may be added. The *layout* determines how the texture *tiles*: not at all, along all axes, or only along X or Y axis.

* **Node tree** contains the meat of the texture definition. There are four types of nodes:
  * **Binary** nodes have two child branches, which are somehow combined. For example, the first child could be *layered* over the second child.

  * **Unary** nodes have a single child, which they modify. For example, the output from the child could be *overdriven*, effecting a contrasty and saturated look when viewed in color.

  * **Bases** are *leaves* of the node tree. They produce the basic texture content that is modified and combined. In the example screenshot, the whole tree consists of just one basis node containing [Perlin noise](http://en.wikipedia.org/wiki/Perlin_noise). You may be familiar with its look in the texture view on the right.
    * Every basis obeys the *layout* of the texture.
    * Every basis has a *frequency* parameter, which is the scale at which texture features occur. Doubling the frequency doubles the detail level.

  * **Fractalizers**, finally, are special nodes that take many basis samples and combine them. They are called fractalizers because, typically, samples are taken at different frequencies in a *[geometric progression](http://en.wikipedia.org/wiki/Geometric_progression)*. When combined, the result has fractal-like qualities because the same general look occurs at many scales.

The parameter panel can be resized by dragging the faintly gray vertical divider at the left of the control panel. If you are not interested in parameters, you can minimize the parameter panel by moving the divider all the way to the left.

### Keyboard Shortcuts

|    |    |
|---:|:---|
| **+** | zoom in  |
| **-** | zoom out |
| **arrow keys** | pan view |
| **Ctrl-C** | copy texture from current view |
| **Ctrl-Q** | quit |
| **Ctrl-R** | randomize all views |
| **Ctrl-S** | save |
| **Ctrl-V** | paste texture into current view |
| **Ctrl-Z** | undo |
| **Ctrl-Y** | redo |

### Control Panel

From top to bottom:

* **Randomize All** creates random textures in all visible views. The view coordinates  and zoom are reset.

* **Mutation mode** influences what kind of modifications are made when mutating a texture.
  * **Everything**: All parameters are mutated with equal probability.
  * **Colors and Effects**: Mostly, the palette, unary nodes, and basis shapes are mutated. (*Shapes* are extra modifiers available by default in basis nodes. They are very similar to unary nodes.)
  * **Scales and Offsets**: Mutate position and scale parameters. This is intended to maintain the overall flavor of the texture while rearranging and repositioning individual parts of it.
  * **Details**: Make small changes only, like what kind of interpolation is used, radii of basis features, and parameters of *mixing operators* like layering.
  * **Choose at Random**: One of the above modes is chosen randomly for each mutation.

* **Default layout mode** determines the layout mode chosen for random textures. Set this appropriately for the kind of textures you want to create. This setting has no influence on already created textures. (For those, you can simply change the layout parameter in the parameter panel.) 

  * **No Tiling**: The texture does not tile. In addition, individual instances of basis nodes are rotated randomly, to break as much as possible regularities arising from fixed orientations.
  * **Tile All**: All axes repeat at intervals of *1 unit*. Basically, the texture fills the space with copies of a unit sized cube. All basis frequencies are rounded to the nearest integer.
  * **Tile X Only**: Only the **X** axis repeats at an interval of 1 unit. All basis frequencies are rounded to the nearest integer.
  * **Tile Y Only**: Only the **Y** axis repeats at an interval of 1 unit. All basis frequencies are rounded to the nearest integer.
  * **Tile Z Only**: Only the **Z** axis repeats at an interval of 1 unit. All basis frequencies are rounded to the nearest integer.

* **View Modes**. This setting selects how the view panel is arranged. At the moment, there are three distinct view modes. When switching a mode, if there is a view in focus, it will be copied to the new mode.

  * **Big View**: There is one big view. The mutate tool is not available in this mode.

  * **Quad View**: There is a 2-by-2 grid of small views. Each view can be edited independently.

  * **Nono View**: There is a 3-by-3 grid of small views. Each view can be edited independently.

 The main attraction of quad and nono modes is *interactive evolution*: With the mutate tool, select your favorite texture and click on it. The other views are then created as its variations. If one of them looks nicer than the original, select it for the next round.

* **Tools** are for working with texture views. Inside views the left mouse button operates tools, while the right mouse button opens a context menu.

  * **Pan Tool**. To pan around, drag the view. Use the mouse wheel to control the **Z** coordinate.

  * **Pan-Zoom Tool**. To pan around, drag the view. Mouse wheel controls zoom: roll up to zoom in, down to zoom out.

  * **Zoom Tool**. Select the center of the area you want to zoom into and drag to choose the size of the area. Mouse wheel controls the **Z** coordinate.

  * **Mutate Tool**. The mutate tool can be used in *quad* and *nono* view modes. Clicking on a view, mutated copies of it appear in the other views. Keep clicking on your favorite and it will (hopefully) evolve into something super impressive.

  * **Jolt Tool**. Click on a view to mutate it according to the current mutation mode. The jolt tool is nearly useless.

* **View Info Box**. At the bottom of the control panel is the view info box where the coordinates of the view in focus are displayed. A view always shows a square slice of the texture in the XY plane, with positive **X** pointing right, positive **Y** pointing down, and **Z** being constant. The **Z** axis is depth and increases toward the screen.

  Axis labels are colored green for axes that tile. In addition, if an axis tiles exactly in the current view, a loop is drawn around the corresponding axis label.
  
  View info box also contains a *detail level* estimate, which is based on sampling. The detail level is expressed in pixels. It is an estimate of the minimum (horizontal or vertical) resolution where most of the detail becomes discernible.

### Texture View

The textures are displayed here. Any change to the parameters of a texture triggers an immediate re-render. Here, the left mouse button operates the current tool. The right mouse button accesses the **context menu**:

* **Maximize**: Take the texture to the big view mode.
* **Go to Quad View**: Take the texture to the quad view mode.
* **Go to None View**: Take the texture to the nono view mode.
* **Zoom In**: Increase zoom level by 100%.
* **Zoom Out**: Reduce zoom level by 50%.
* **Reset Zoom**: Reset zoom to default, which is a view area of 1x1 units. Note that tiling textures tile perfectly at the default zoom level.
* **Reset View**: Reset zoom and view area to the 1x1 square between 0 and 1 on **X** and **Y** axes. Reset **Z** to *0.5*.
* **Randomize**: Create a new random texture in this view.
* **Open in New Window**: Open the texture in a new editor window.
* **Create Preset**: Save the texture as a preset.
* **Update Preset**: If the texture was loaded from a preset, overwrites the preset.
* **Copy F# Code to Clipboard**: Copy to clipboard an expression containing the texture. Texture data will be packed in a Base64 encoded string.
* **Show Ray Trace**: Open a window displaying a simple scene with a few diffuse textured objects: a cube, sphere and cylinder. For reflectance models we have Oren-Nayar for diffuse and Phong for specular. The camera strafes around the scene.

## Bases

**FsMap3** comes equipped with a wide assortment of 3-bases.

|    |    |    |
|---:|:---|:---|
|![Perlin noise](bases/perlin.png) | Perlin noise | [Perlin noise](http://en.wikipedia.org/wiki/Perlin_noise), as many other noise bases, emulates band-limited noise. This version has unconstrainted gradients and a selectable interpolation fade.|
|![cubex noise](bases/cubex.png) | cubex noise | *Cubex noise* is similar to Perlin noise. It is slower to calculate but has more options. |
|![isotropic noise](bases/isotropicNoise.png) | isotropic noise | Isotropic noise is another band-limited noise. It is designed to avoid all directional artifacts: it looks similar whatever angle it is viewed from. This is not true of Perlin noise (or cubex noise below). However, it is slower to calculate than Perlin noise.
|![radial value noise](bases/radial.png) | radial value noise | *Radial value noise* is yet another noise-like basis. Unlike the others, its features are points, not gradients. The points are distance weighted. |
|![Worley](bases/worley.png) | Worley | The [Worley basis](http://en.wikipedia.org/wiki/Worley_noise) outputs processed distances to nearest feature points. This version is specially adapted to produce 3-dimensional results. The distance metric is selectable. |
|![colored Worley](bases/coloredWorley.png) | colored Worley | A colored version of the Worley basis. The output is colored according to the nearest cell. |
|![tiles](bases/jigsaw.png) | tiles | Another colored Worley basis resembling tiles with more straightforward parameters.
|![leopard](bases/leopard.png) | leopard | The *leopard* basis is a bunch of spots mixed together. |
|![peacock](bases/peacock.png) | peacock | The *peacock* basis is like the leopard basis except the spots are now randomly oriented shapes (technically, potential fields). |
|![Julia](bases/julia.png) | Julia | The *Julia* basis turns some well-known [fractal iteration formulas](http://en.wikipedia.org/wiki/Julia_set) into a tileable 3-texture form. Fractal parameters are interpolated between feature points. |
|![Julia orbit trap](bases/orbit.png) | Julia orbit trap | *Julia orbit trap* basis. Like the Julia basis but the coloring is done with the orbit trap technique. |
|![capsule flow](bases/capsuleFlow.png) | capsule flow | *Capsule flow*. The flow bases distributes oriented potentials like the Peacock basis. However, in the flow bases the orientation at each point is sampled from a separate *flow texture*. The capsule flow basis consists of oriented capsules. |
|![potential flow](bases/potentialFlow.png) | potential flow | The *potential flow* basis consists of shapes oriented according to the flow basis. |
|![weave](bases/weave.png) | weave | *Weave* is special in that, for now, it is the only 3-basis designed to look 2-dimensional, specifically, a woven or threaded pattern. |
