---
author: Rebecca Skinner
title: The Nixification of rebeccaskinner.net
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

I chose to start the nixification of my site by borrowing some ideas from the
architecture of [the haskell.org site](https://github.com/haskell-infra/www.haskell.org)
since it's an existing hakyll site that I know is reliable and easy to work
with. Throughout this post I'll focus on the versions that I built, but I would
encourage an interested reader to follow up by reviewing the haskell.org code as
well to compare and contrast the two quite similar approaches toward building an
application with nix.

Conceptually, the layout is fairly straightforward. All of the _content_ for the
site, like markdown files, images, and CSS, lives in the root directory of the
project. A `default.nix` in the project root has targets to build the site using
a _builder_, which is the static site generator that we're creating using hakyll.

## The Builder

The builder is the static site generator that's responsible for actually
building the site. The builder for this site consists of a few files, listed
below.

- site.hs: The source code to the builder
- rebeccaskinner-net.cabal: The cabal file we'll use to build the builder
- LICENSE: A copy of the BSD3 LICENSE
- default.nix: The nix expression to build the builder

For this post we'll focus on the nix files, but if you are interested in
reviewing the full source of the builder you can [take a look at it on github](https://github.com/rebeccaskinner/rebeccaskinner.github.io/tree/src).

This nix expression, like most, is a function whose parameter is a record with a
`pkgs` field that will be the package set that we should build with. As is
typical, if no package set is provided, we'll default to the current nixpkgs:

```nix
{ pkgs ? import <nixpkgs> {} }:
```

We're going to build our package with the
`developPackage` function that's given to use by the
nixpkgs haskell infrastructure.
[The documentation](https://github.com/NixOS/nixpkgs/blob/master/pkgs/development/haskell-modules/make-package-set.nix#L250)
for this function is available in the nixpkgs comments, and describes how to use
the function.

```nix
(pkgs.haskellPackages.developPackage {
```

We need to pass in a record with a few required fields, and some optional
ones. The first parameter we need to pass in is the root of the project
filesystem. This needs to be a derivation in the package store, so we can't
simply pass in a path or list of files directly. One option to create a
filesystem in the nix packagestore is to use `builtins.filterPackage`, but we'll
opt for the more ergonomic `gitignoreSourcePure` from [nix-gitignore](https://nixos.org/manual/nixpkgs/stable/#sec-pkgs-nix-gitignore).

We call this function with a list of globs, using the same syntax we could use
for gitignore files, and a root directory. We'll get back a derivation in the
nix store with the filesystem:

```nix
  root = pkgs.nix-gitignore.gitignoreSourcePure [
    "dist-newstyle"
    "dist"
    ".*#"
    ".git"
  ] ./.;
```

In our case, we're filtering out cabal's `dist` and `dist-newstyle` directories
from our builds, as well as any emacs backup files, and any git directory. If
you're curious, we can see the resulting derivation by running this command in
the nix repl:

```sh
~/projects/rebeccaskinner.github.io λ cd builder
~/projects/rebeccaskinner.github.io/builder λ nix repl '<nixpkgs>'
Welcome to Nix version 2.3.11. Type :? for help.

Loading '<nixpkgs>'...
Added 14253 variables.

nix-repl> nix-gitignore.gitignoreSourcePure ["dist" "dist-newstyle" ".*#" ".git" ] ./.
"/nix/store/zq7lwzzn4rlwfdxlrp93b26wf47mbyx2-builder"


~/projects/rebeccaskinner.github.io/builder λ ls -la "/nix/store/zq7lwzzn4rlwfdxlrp93b26wf47mbyx2-builder"
total 4940
dr-xr-xr-x    2 root root      4096 Dec 31  1969 ./
drwxrwxr-t 8575 root nixbld 5033984 Jun  6 23:58 ../
-r--r--r--    1 root root       497 Dec 31  1969 default.nix
-r--r--r--    1 root root      1458 Dec 31  1969 LICENSE
-r--r--r--    1 root root       480 Dec 31  1969 rebeccaskinner-net.cabal
-r--r--r--    1 root root      2815 Dec 31  1969 site.hs
```

As you can see, the resulting derivation contains only the files that we need to
compile our builder.

The next thing we would like to add are some build-time
dependencies. `developPackage` will use our cabal file to resolve our build-time
library dependencies, but we'd like to add any tools that we want to make
available within a nix shell. We can pass any any extra haskell dependencies
that we want in the `modifier` field, where we can add some things to the
`buildTools` list:

```nix
  modifier = drv: pkgs.haskell.lib.overrideCabal drv (attrs: {
    buildTools = with pkgs.haskellPackages; (attrs.buildTools or []) ++ [
      cabal-install
      hakyll
      pkgs.linkchecker
    ];
```

Finally, we need to override some settings on the derivation that
`developPackage` generates for us. In particular, we need to set some locale
information to ensure we don't run into any problems rendering our documents:

```nix
}).overrideAttrs (old: {
  LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
  LC_ALL = "C.UTF-8";
})
```

The final version of our nix code looks like this:

```nix
{ pkgs ? import <nixpkgs> {}
}:
(pkgs.haskellPackages.developPackage {
  root = pkgs.nix-gitignore.gitignoreSourcePure [
    "dist-newstyle"
    ".*#"
    ".git"
  ] ./.;

  modifier = drv: pkgs.haskell.lib.overrideCabal drv (attrs: {
    buildTools = with pkgs.haskellPackages; (attrs.buildTools or []) ++ [
      cabal-install
      hakyll
      pkgs.linkchecker
    ];
  });
}).overrideAttrs (old: {
  LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
  LC_ALL = "C.UTF-8";
})
```

## A Top-Level Derivation

The next thing we'll add is a top-level nix derivation. This will let us build
both the builder itself, as well as use the builder to build our website. We'll
start our expression much like we did the builder expression, but we'll add an
additional parameter, `doCheck`. This will let us temporarily disable running
`linkchecker` if we are in the middle of active development and want to test the
site with known broken links.

```nix
{ doCheck ? true
, pkgs ? import <nixpkgs> {}
}:
```

Next up, we'll import our builder and make it available here:

```nix
let
  builder = import ./builder { inherit pkgs; };
```

We also want to define a new derivation for our site. We'll use the standard nix
`mkDerivation` function for this:

```nix
  site = pkgs.stdenv.mkDerivation {
    name = "rebeccaskinner.net";
    inherit doCheck;
    src = pkgs.nix-gitignore.gitignoreSourcePure [
      ./.gitignore
      ".git"
      "*.cabal"
      "*.hs"
      ".github"
      "builder"
      "dist"
      "dist-newstyle"
      ".#*"
    ] ./.;
```

The name of our derivation gets set to the website, and we inherit our `doCheck`
parameter. Remember this is the same as saying `doCheck = doCheck;`. We have a
few extra things we want to ignore this time around, but otherwise we're
defining our source directory the same way that we defined the root in our
`builder`.

The build inputs are the packages that we need to build our site. Remember that
our builder is the compiler for our site, so we don't directly need the haskell
infrastructure here, just our builder itself. We're also going to depend on
~linkchecker~ so we can validate the links in our site:

```nix
    buildInputs = [ builder pkgs.linkchecker ];
```

As before, we're also going to set some locale information into the
environment. `mkDerivation` adds any otherwise unused fields into the
environment, so we don't need to use overrides here to get them added:

```haskell
    LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
    LC_ALL = "C.UTF-8";
```

Next, we need to tell nix how to build our site. We'll tell the `buildPhase` to
call our builder to build our site. The `checkPhase` will call lintchecker to
check the links in the site, and finally the `installPhase` will copy the build
output to the output directory.

```nix
    buildPhase = ''
      ${builder}/bin/builder build
    '';
    checkPhase = ''
      linkchecker _site
    '';
    installPhase = ''
      cp -r _site $out
    '';
  };
```

The last thing we'll do in our derivation is check to see if we're in a nix
shell. If so, we'll defer to the builder's shell environment, and otherwise
we'll create a new derivation that combines both the builder along with our
`site` derivation. This will let us build either the site or the builder with
`nix-build` from the top level of our project:

```nix
if pkgs.lib.inNixShell then builder
else { inherit builder site; inherit (pkgs) linkchecker; }
```

The final code for our top-level derivation is:

```nix
{ doCheck ? true
, pkgs ? import <nixpkgs> {}
}:

let
  builder = import ./builder { inherit pkgs; };
  site = pkgs.stdenv.mkDerivation {
    name = "rebeccaskinner.net";
    inherit doCheck;
    src = pkgs.nix-gitignore.gitignoreSourcePure [
      ./.gitignore
      ".git"
      "*.cabal"
      "*.hs"
      ".github"
      "builder"
      "dist"
      "dist-newstyle"
      ".#*"
    ] ./.;
    buildInputs = [ builder pkgs.linkchecker ];
    LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
    LC_ALL = "C.UTF-8";
    buildPhase = ''
      ${builder}/bin/rebeccaskinner-net-site build
    '';
    checkPhase = ''
      linkchecker _site
    '';
    installPhase = ''
      cp -r _site $out
    '';
  };
in
if pkgs.lib.inNixShell then builder
else { inherit builder site; inherit (pkgs) linkchecker; }
```

Now we can try to build our site:

```sh
/tmp/rebeccaskinner.github.io λ nix-build -A site
error: Package ‘hakyll-4.14.0.0’ in /nix/store/j0cv2zra282hp92wl2j9rfvrgrz3zc2a-nixos-21.11pre292868.7e9e1b6351b/nixos/pkgs/development/haskell-modules/hackage-packages.nix:114755 is marked as broken, refusing to evaluate.

a) To temporarily allow broken packages, you can use an environment variable
   for a single invocation of the nix tools.

     $ export NIXPKGS_ALLOW_BROKEN=1

b) For `nixos-rebuild` you can set
  { nixpkgs.config.allowBroken = true; }
in configuration.nix to override this.

c) For `nix-env`, `nix-build`, `nix-shell` or any other Nix command you can add
  { allowBroken = true; }
to ~/.config/nixpkgs/config.nix.

(use '--show-trace' to show detailed location information)
```

Unfortunately, it appears that we've encountered a problem with a broken
package. Hakyll does not currently seem to build. In the next section, we'll dig
into hakyll and diagnose the problem, and bring a fix back into our nix
derivation so that we can successfully build our site.

# Patching Hakyll

To diagnose why hakyll is broken, we'll need to start by cloning [the hakyll repository](https://github.com/jaspervdj/hakyll)
and creating a nix environment where we can start to reproduce the build
failures. To do that, after cloning the repository, we'll start by checking out
the tag corresponding to the release that the nixpkg is pointing at, in our case
~4.14.0.0~:

~~~sh
~/projects/hakyll λ git checkout v4.14.0.0
HEAD is now at a35e1c3 Bump version to 4.14.0.0
~~~

Next we need to get a nix environment that we can use to try to build the
package. The easiest way to do this is with ~cabal2nix~, which will generate a
~shell.nix~ for us:

~~~sh
~/projects/hakyll λ cabal2nix --shell . > shell.nix
~/projects/hakyll λ nix-shell
[nix-shell:~/projects/hakyll]$
~~~

Now we can start by trying to build hakyll and see what goes wrong:

~~~nix
[nix-shell:~/projects/hakyll]$ cabal new-build
Warning: The package list for 'hackage.haskell.org' does not exist. Run 'cabal
update' to download it.RemoteRepo {remoteRepoName = RepoName
"hackage.haskell.org", remoteRepoURI = http://hackage.haskell.org/,
remoteRepoSecure = Just True, remoteRepoRootKeys =
["fe331502606802feac15e514d9b9ea83fee8b6ffef71335479a2e68d84adc6b0","1ea9ba32c526d1cc91ab5e5bd364ec5e9e8cb67179a471872f6e26f0ae773d42","2c6c3627bd6c982990239487f1abd02e08a02e6cf16edb105a8012d444d870c3","0a5c7ea47cd1b15f01f5f51a33adda7e655bc0f0b0615baa8e271f4c3351e21d","51f0161b906011b52c6613376b1ae937670da69322113a246a09f807c62f6921"],
remoteRepoKeyThreshold = 3, remoteRepoShouldTryHttps = True}
Resolving dependencies...
cabal: Could not resolve dependencies:
[__0] trying: hakyll-4.14.0.0 (user goal)
[__1] trying: hakyll:+usepandoc
[__2] next goal: pandoc (dependency of hakyll +usepandoc)
[__2] rejecting: pandoc-2.13/installed-E74Gb9SwyqzDDzSkDaPq28 (conflict:
hakyll +usepandoc => pandoc>=2.11 && <2.12)
[__2] fail (backjumping, conflict set: hakyll, pandoc, hakyll:usepandoc)
After searching the rest of the dependency tree exhaustively, these were the
goals I've had most trouble fulfilling: hakyll, hakyll:usepandoc, pandoc
~~~

It appears that hakyll depends on a version of pandoc between 2.11 and
2.12, but the only version available in our current nix package set is 2.13. At
this point, one of the quickest and easiest things to do is to simply modify the
dependency to see if the hakyll will still work with the newer version of
pandoc. We can do this quite easily by editing the cabal file to change
occurrences of:

~~~cabal
pandoc >= 2.11  && < 2.12
~~~

to instead read

~~~cabal
pandoc == 2.13
~~~

If you try to build again, this time you should get an error with ~cryptonite~,
which you can also change. Continuing the pattern eventually you will update all
of the necessary dependencies and see that hakyll builds and works well with
newer dependencies.

What can we do with this knowledge that we can build hakyll with newer
dependencies? One option is to wait until a new version of hakyll is released
and is available in the package set. Presumably this newer version will use
updated dependencies and we can successfully build then. This is a fine option
if you aren't in a hurry, but you also risk future breakage for the same
reason. A second option is to fork the repository and commit a branch with the
changes. This is a great solution for fixes that you might eventually want to
upstream, where you would already be creating a branch and making a PR. That
doesn't apply in our situation, since we're using a historic release of hakyll,
not the current release.

The last option at our disposal, and the one we'll use in this post, is to
generate some patch files and apply them as an overlay in our nix derivation. To
do that, we first need to generate a patch file. Commit the changes you made to
the cabal file, and then you can use `git format-patch` to generate a patch file
with the changes you've made:

```sh
[nix-shell:~/projects/hakyll]$ git format-patch -1
0001-patch-dependencies.patch
```

The contents of the patch file contain the information about the changes that
we've made:

```patch
From 1ff887ac0a5e800d63c0ba54908fef2eb4cbd1fc Mon Sep 17 00:00:00 2001
From: rebecca skinner <rebecca at rebeccaskinner dot n et>
Date: Mon, 7 Jun 2021 01:15:12 -0500
Subject: [PATCH] patch dependencies

---
 hakyll.cabal | 8 ++++----
 1 file changed, 4 insertions(+), 4 deletions(-)

diff --git a/hakyll.cabal b/hakyll.cabal
index c582934..ef7e565 100644
--- a/hakyll.cabal
+++ b/hakyll.cabal
@@ -175,7 +175,7 @@ Library
     blaze-markup         >= 0.5.1    && < 0.9,
     bytestring           >= 0.9      && < 0.11,
     containers           >= 0.3      && < 0.7,
-    cryptonite           >= 0.25     && < 0.28,
+    cryptonite           == 0.28,
     data-default         >= 0.4      && < 0.8,
     deepseq              >= 1.3      && < 1.5,
     directory            >= 1.2.7.0  && < 1.4,
@@ -185,7 +185,7 @@ Library
     memory               >= 0.14.18  && < 0.16,
     mtl                  >= 1        && < 2.3,
     network-uri          >= 2.6      && < 2.7,
-    optparse-applicative >= 0.12     && < 0.16,
+    optparse-applicative == 0.16.1.0,
     parsec               >= 3.0      && < 3.2,
     process              >= 1.6      && < 1.7,
     random               >= 1.0      && < 1.3,
@@ -237,7 +237,7 @@ Library
     Other-Modules:
       Hakyll.Web.Pandoc.Binary
     Build-Depends:
-      pandoc >= 2.11 && < 2.12
+      pandoc == 2.13
     Cpp-options:
       -DUSE_PANDOC

@@ -333,4 +333,4 @@ Executable hakyll-website
     base      >= 4     && < 5,
     directory >= 1.0   && < 1.4,
     filepath  >= 1.0   && < 1.5,
-    pandoc    >= 2.11  && < 2.12
+    pandoc    == 2.13
--
2.31.1
```

Now that we have a patch, we can return to our blog and make use of the patch to
build a custom version of hakyll that will work in our nix environment.

# Adding An Overlay

## Building an Override

To build our overlay, we need to start by creating an override. We could do this
inside of our top level `default.nix`, but I find that it's helpful for
readability to move individual overrides into separate files, so I created a new
file at `nix/hakyll/default.nix`. Inside of the file we need to create a new
override.

A nix override is a function of two arguments to a set of packages, or more
precisely we can think of it as a function with the type:

```
PackageSet -> PackageSet -> PackageSet
```

The first package set is the "final" package set that we get from applying all
of the overlays. The second argument is the package set before application of
the function. The traditional names for these arguments are `self` and `super`,
but I prefer to call them `fixedPoint` and `pkgs`.

```nix
fixedPoint: pkgs: {
```

Hakyll isn't in the top-level package group, instead it's part of of nix's
haskell package set. These are stored on a per-compiler-version level under
nixpkgs in `haskell.packages.<compiler>.packagename`. The `haskellPackages`
package set is an alias for the package set for the current stable compiler.

This means that we have a choice to either override the package in a specific
compiler version, or else to override it in the `haskellPackages`. As you'll see
in the next section, we're going to be pinning the version of nix to a specific
commit, so for our purposes for now it doesn't matter as much, so we'll work
directly with `haskellPackages`.

An overlay is essentially a set of packages that will supersede the packages in
the set that is to be overlaid. In this case, we want to replace the existing
`haskellPackages` set with a different version that is using our patched version
of hakyll, so we'll start by setting `haskellPackages` to over overridden
version:

```nix
haskellPackages = pkgs.haskellPackages.override {
```

The override function in `haskellPackages` takes a record with an `overrides`
argument.  This is going to be another function that takes a fixed point and a
package set, just like the top level function we've built. We'll call these
`haskellFixedPoint` and `haskellPkgs` to differentiate them from our top level
`fixedPoint` and `pkgs` parameters, although it's worth remembering that
technically we could shadow our variable names here:

```nix
overrides = haskellFixedPoint: haskellPkgs: {
```

Again here we're going to be providing a set of packages that should replace the
default package set inside of `haskellPackages`. This time we're going to be
replacing `hakyll` inside of the haskell package set:

```nix
hakyll = haskellPkgs.hakyll.overrideAttrs (oldAttrs: rec {
  allowBroken = true;
  patches = [./deps.patch];
```

We're replacing two attributes here: `allowBroken` is a boolean that tells nix
to not try to build the derivation. We're fixing the derivation in our
overridden version, so we want to tell nix to allow this version. We're also
setting the `patches` attribute, which provides a set of patch files that should
be applied to the source code before building. When our new patch is applied, it
will fix the dependencies that have caused hakyll to fail to build.

Our nix expression ends up looking like this:

```nix
fixedPoint: pkgs: {
  haskellPackages = pkgs.haskellPackages.override {
    overrides = haskellFixedPoint: haskellPkgs: {
      hakyll = haskellPkgs.hakyll.overrideAttrs (oldAttrs: rec {
        allowBroken = true;
        patches = [./deps.patch];
      });
    };
  };
}
```

Finally, we need to copy the patch file that we generated earlier into this
directory, and name it `deps.patch`. If you decide to pick a different name, be
sure to update the patches list in the nix expression to refer to the correct
filename.

## Adding The Overlay

Now that we have an overlay we need to actually apply it when we import our
package set. To do that, we need to set the `overlays` parameter when we import
`<nixpkgs>`.

To do that, we'll remove the pkgs as a parameter to our top-level
expression, and import it in a let statement where we can pass in a parameter:

```nix
{doCheck ? true}:

let
  pkgs = import <nixpkgs> { overlays = [(import ./nix/hakyll)]; };
```

By setting the `overlays` argument when we import `nixpkgs`, we tell nix to add
in our hakyll overlay. This causes us to replace the standard `haskellPackages`
with one where we're using our patched version of `hakyll`.

# Pinning Nixpkgs

Unfortunately, there's one sticking point that we still need to deal with. After
all of the work we've done to nixify our application, we face the possibility
that another update might cause more dependencies to move out of the range of
compatibility, meaning that we'd need to add more patches. Worse, we might find
ourselves with a set of haskell packages that actually break hakyll.

To resolve this, we can choose to stick with the version of nixpkgs we were
using at the time that we got everything working. This is referred to as
_pinning_.

Remember that `<nixpkgs>` isn't anything magical in the nix world- it's just a
pointer to some particular commit into the `nixpkgs` repo, depending on the
channel you are following. You can think of it as being analogous to a git
branch name. When we pin a package, instead of referring to this name which
might point to a different commit over time, we instead tell nix how to download
precisely the package set that we want. We'll do that using the
`builtins.fetchTarball` function to pull a tarball from github.

To fetch a tarball we need a name, a URL, and a sha256 value. The name can be
anything we like, but it's helpful to make it something that lets you easily
understand what the package set should be, like the date it was fetch:

```nix
pkgs = import (builtins.fetchTarball {
  name = "nixos-unstable-2020-06-06";
```

The URL should be a URL to the tarball with the repository contents. To get
that, you need the revision of the version of nixpkgs that you're on. You can
get the revision from `nix repl` by calling `lib.version`:

```nix
~/projects/rebeccaskinner.github.io λ nix repl '<nixpkgs>'
Welcome to Nix version 2.3.11. Type :? for help.

Loading '<nixpkgs>'...
Added 14253 variables.

nix-repl> lib.version
"21.11pre292868.7e9e1b6351b"
```

The pattern to get an archive from github is:
`github.com/<org>/<project>/archive/<sha>.tar.gz`, so in this case:

```nix
    url = "https://github.com/nixos/nixpkgs/archive/7e9e1b6351b.tar.gz";
```

The last thing we need is the sha256 of the unpacked archive. We can get this
with `nix-prefetch-url`:

```sh
~/projects/rebeccaskinner.github.io λ nix-prefetch-url --unpack https://github.com/nixos/nixpkgs/archive/7e9e1b6351b.tar.gz
unpacking...
[23.1 MiB DL]
path is '/nix/store/10mqgnhpj661jlqm7hbhdv2s597wcc5x-7e9e1b6351b.tar.gz'
1ga7zkkzksgpvymkblj31m55zdrn1ak2iqnisk177x5mgd9vvcqp
```

When we set the `sha256` field the final version of our new top-level
default.nix becomes:

```nix
{doCheck ? true}:

let
  pkgs = import (builtins.fetchTarball {
    name = "nixos-unstable-2020-06-06";
    url = "https://github.com/nixos/nixpkgs/archive/7e9e1b6351b.tar.gz";
    sha256 = "1ga7zkkzksgpvymkblj31m55zdrn1ak2iqnisk177x5mgd9vvcqp";
  }){
    config = { allowBroken = true; };
    overlays = [(import ./nix/hakyll)];
  };

  builder = import ./builder { inherit pkgs; };
  site = pkgs.stdenv.mkDerivation {
    name = "rebeccaskinner.net";
    inherit doCheck;
    src = pkgs.nix-gitignore.gitignoreSourcePure [
      ./.gitignore
      ".git"
      "*.cabal"
      "*.hs"
      ".github"
    ] ./.;
    buildInputs = [ builder pkgs.linkchecker ];
    LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
    LC_ALL = "C.UTF-8";
    buildPhase = ''
      ${builder}/bin/builder build
    '';
    checkPhase = ''
      linkchecker _site
    '';
    installPhase = ''
      cp -r _site $out
    '';
  };
in
if pkgs.lib.inNixShell then builder
else { inherit builder site; inherit (pkgs) linkchecker; }
```

With the new default.nix in place and everything patched, we can easily build
our site:

```sh
~/projects/rebeccaskinner.githug.io λ nix-build -A site
```
