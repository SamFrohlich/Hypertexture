# Hypertexture Domain Specific Language

## Hypertexture 

Hypertexture is shape description method that allows the representation of phenomena like fire or smoke. Instead of being represented as a shell 3D shapes are represented as an array of density values.

## The Code 

This is a domain specific language embedded into Haskell providing an expressive way of creating and combining Hypertextures. The code includes a deep embedding that can be given different semantics by providing different instances of the `Alg` type class. Two example instances are also given:

* __Maya:__ Turns a subset of the DSL into `MEL` code that creates 3D shapes in [Maya](https://www.autodesk.co.uk)
* __Set:__ Compiles the language down to a set membership function. This function can be used to sample densities at specific points to produce cross sections or 3D renderings of Hypertexture.