---
author: Rebecca Skinner
title: Syntax Highlighting in Hakyll
categories: ["haskell", "hakyll", "nix"]
description: Making Your Hakyll Blog Reproducible, With Nix
---

# The Great Nixification

I recently migrated this blog away from stack to being built with nix and
cabal. The process of migrating to nix has been largely smooth with a couple of
learning opportunities along the way. In this post I'll walk you through how the
blog is set up, with a particular focus on how to patch hakyll 4.14 to work with
recent versions of nixpkgs, including setting up a custom `haskellPackages`
overlay. After you've read this you should be able to build your own nixified
hakyll site, and generally have a better understanding of how to manage overlays
in the haskell nixpkgs ecosystem.

# The Architecture of a Hakyll Site

[Hakyll](https://jaspervdj.be/hakyll/) is a library for building static site
generators (SSGs). Like any other static site built with a static site
generator, a hakyll site is a static site that is built for you from some
component pieces like template files, markdown, and static images, javascript,
or CSS. Unlike other static site generators, and a key difference when we are
considering how to build our site is that hakyll, being a library, is not a
stand-alone tool that we can directly use to build our site. Instead, building
our hakyll site is a two-step process: First we need to build our own static
site generator, which imports hakyll as a library to do the heavy lifting for
us, and second we need to use our newly compiled static site generator to build
the site itself.

# Nixification Part 1

# Patching Hakyll

# Nixifixation Part 2: Overlays

# Conclusion
