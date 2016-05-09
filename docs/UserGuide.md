# FsMap3 Explorer User Guide

[TOC]

## Introduction

**FsMap3 Explorer** is a Windows GUI for creating and exploring *[procedural textures](http://en.wikipedia.org/wiki/Procedural_texture)*. Texture descriptions can be edited, loaded and saved, exported as images, randomized, mutated and evolved. This user guide helps you get started.

At the time of this writing, **FsMap3** is considered to be in alpha, which means that things are still changing rapidly. Backward compatibility of saved textures is not guaranteed until we move to beta.

## What Are Procedural Textures?

Procedural textures are images created by an algorithm from a simple description. The textures in **FsMap3** are *3-dimensional*. 3-textures are also called *solid textures* because they can texture any solid object. For example, a statue sculpted from marble shows a marble texture in its surface. As the sculptor chips away at the stone, the texture always maintains its general form, as the visible part is just the surface of something that is solid.

Technically, a 3-texture is a box that accepts 3 numbers (X, Y and Z), processes them somehow, and cranks out another 3 numbers. The numbers that come out of the box can be interpreted as a color, in RGB format for instance, and displayed, or they can be interpreted as another set of coordinates and forwarded to some other box for further processing.

These boxes are typically called **nodes** in texture editors. A complete procedural texture is built by connecting several nodes in a tree or graph.

## A Quick Look at the User Interface

Below is an example screenshot with the main sections delineated.

![Main sections of the user interface.](quickLook.png)

The user interface is divided horizontally into three parts: *parameters*, *control panel*, and *texture view*.

### Parameter Panel

The parameter panel contains all **parameters** of the texture under view. Together, the parameters completely define what the texture looks like. They are divided into three parts:

- **Palette** defines the color scheme of the texture. At each point in space, the final 3 values that emerge from the texture are colored according to the palette.

- **Global parameters** consists of just the layout for now. In the future, other parameters may be added. The *layout* determines how the texture *tiles*: not at all, along all axes, or only along X or Y axis.

- **Node tree** contains the meat of the texture definition. There are four types of nodes:
 - **Binary** nodes have two child branches, which are somehow combined. For example, the first child could be *layered* over the second child.
 - **Unary** nodes have a single child, which they modify. For example, the output from the child could be *overdriven*, effecting a contrasty and saturated look when viewed in color.
 - **Bases** are *leaves* of the node tree. They produce the basic texture content that is modified and combined. In the example screenshot, the whole tree consists of just one basis node containing [Perlin noise](http://en.wikipedia.org/wiki/Perlin_noise). You may be familiar with its look in the texture view on the right.
   - Every basis obeys the *layout* of the texture.
   - Every basis has a *frequency* parameter, which is the scale at which texture features occur. Doubling the frequency doubles the detail level.
 - **Fractalizers**, finally, are special nodes that take many basis samples and combine them. They are called fractalizers because, typically, samples are taken at different frequencies in a *[geometric progression](http://en.wikipedia.org/wiki/Geometric_progression)*. When combined, the result has fractal-like qualities because the same general look occurs at many scales.

The parameter panel can be resized by dragging the faintly gray vertical divider at the left of the control panel. If you are not interested in parameters, you can minimize the parameter panel by moving the divider all the way to the left.
