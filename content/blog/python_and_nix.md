+++
author = "Rebecca Skinner"
title = "Building Python Applications with Nix"
categories = ["python", "nix"]
tags = ["python", "nix", "practice"]
date = "2018-05-09"
description = "An overview of using nix to manage python development environments"
type = "post"
+++

## Introduction

Nix is a package general purpose package manager available for *nix systems,
including NixOS (a Linux distrobution), Linux, *bsd, and MacOS.  Nix provides
several novel capabilities, specifically immutability and reproducability.  A
nix package defines exact versions of all of it's dependencies, and a nix
installation manages each individual application independently.  In this post
we'll look at using Nix during the development process, comparing it to
virtualenv and pip for developing a python application.

## Background

I recently joined a new team.  Like most teams that I've worked on over the last
decade, my new team is a polyglot shop with tools written in many different
languages.  In this case of this particular team, one of the core applications
that I found myself working with is written in Python.  Although I've easily
worked in a dozen languages in my career, this was my first time working with
python in earnest.  Every new language brings a unique set of challenges as you
adopt to the idioms, quirks, and warts of both the language and it's ecosystem.
One of the warts that I found most troublesome when I started working with
python was the tooling around managing package installation and package
versioning.  Virtualenv and pip in particular caused me a great deal of
frustration during my python onboarding process.

To understand my frustration it's helpful to understand both my background and
the languages and tools I work with day-to-day.  For most of my first decade in
the industry I worked primarily on appliances

## Setting Up the Environment

### Install Nix

[Nix](https://nixos.org/nix/) can be installed on most *nix systems from source or via a curl-bash
command.

```
curl https://nixos.org/nix/install | sh
```

Once you've installed nix, you'll want to configure your local environment to
use nix.  Add the following into your `${HOME}/.bashrc`:

```
source /home/rebecca/.nix-profile/etc/profile.d/nix.sh
```

You can check that everything is working by running:

```
rebecca@debian:~$ which hello
rebecca@debian:~$ echo $? # demonstrate that "hello" wasn't installed
rebecca@debian:~$ nix-shell -p hello # start nix-shell including hello
[nix-shell:~]$ which hello # now it's found!
/nix/store/6mab2znnw7j96k3vsdw9vyckady29r46-hello-2.10/bin/hello
[nix-shell:~]$ exit
```

### Create a development Environment

The sample project we'll be using is a small [Python 3 web
appliation](https://github.com/rebeccaskinner/py-todo) built using
[Flask](http://flask.pocoo.org/).  We'll have minimal dependencies, in this case
just python3 and the flask library.

We'll start by creating a nixpkg called `default.nix`.  The first thing we'll
want to do is import the list of nixpkgs that we know about in our environment:

```
with import<nixpkgs> {};
```

The `with` keyword allows us to bring a set of variables into scope in a
statement.  The `import` allows us to import another nix package.  In our case,
`<nixpkgs>` is an nix variable defined in the `$NIX_PATH` environment variable:

```
[nix-shell:~/projects/py-todo]$ echo ${NIX_PATH}
nixpkgs=/home/rebecca/.nix-defexpr/channels/nixpkgs
```

The import statement allows us to specify parameters that will be passed into
the nixpkgs that we import.  In our case, we don't want to pass anything in so
we simply use `{}` in our import statement.

#### Derivations

Derivations are the core of building nixpkgs.  A derivation is, in short, a nix
statement that tells us how to derive the environment for a given package.  In
our case, we want to derive our environment starting with the default standard
environment, which will give us things like a bash shell and standard tools like
`make` and `gcc`:

```
stdenv.mkDerivation rec {
  name = "env";
}
```

The `rec` keyword allows us to have recrusively defined references.  This means
that individual fields in our derivation can reference on another.  Allowing
these greatly simplifies the process of writing our derivations, at the cost of
having nix expressions that may not terminate.

#### Declaring Dependencies

To declare our dependencies, we'll first create a list of nix package names that
we need for our environment.  We can find the names of the packages that we want
to install with `nix-env`.

Looking up the available python 3 packages is straightforward:
```
rebecca@debian:~/projects/py-todo$ nix-env -qaP python3
nixpkgs.python34      python3-3.4.8
nixpkgs.python34Full  python3-3.4.8
nixpkgs.python35      python3-3.5.5
nixpkgs.python35Full  python3-3.5.5
nixpkgs.python3       python3-3.6.5
nixpkgs.python36Full  python3-3.6.5
nixpkgs.python3Full   python3-3.6.5
```
Looking for the name of our Flask package is a bit more difficult since we're
not entirely sure what it might be called.  We can use grep to broaden our search:
```
rebecca@debian:~/projects/py-todo$ nix-env -qaP | grep Flask | grep python3.6
nixpkgs.python36Packages.flask                                           python3.6-Flask-0.12.2
nixpkgs.python36Packages.flask_assets                                    python3.6-Flask-Assets-0.12
nixpkgs.python36Packages.flask-autoindex                                 python3.6-Flask-AutoIndex-0.6
nixpkgs.python36Packages.flaskbabel                                      python3.6-Flask-Babel-0.11.1
nixpkgs.python36Packages.flask-babel                                     python3.6-Flask-Babel-0.11.2
nixpkgs.python36Packages.flask_cache                                     python3.6-Flask-Cache-0.13.1
nixpkgs.python36Packages.flask-common                                    python3.6-Flask-Common-0.2.0
nixpkgs.python36Packages.flask-compress                                  python3.6-Flask-Compress-1.4.0
nixpkgs.python36Packages.flask-cors                                      python3.6-Flask-Cors-3.0.3
nixpkgs.python36Packages.flask_elastic                                   python3.6-Flask-Elastic-0.2
nixpkgs.python36Packages.flask-limiter                                   python3.6-Flask-Limiter-1.0.1
nixpkgs.python36Packages.flask_login                                     python3.6-Flask-Login-0.4.1
nixpkgs.python36Packages.flask_mail                                      python3.6-Flask-Mail-0.9.1
nixpkgs.python36Packages.flask_migrate                                   python3.6-Flask-Migrate-2.1.1
nixpkgs.python36Packages.flask_oauthlib                                  python3.6-Flask-OAuthlib-0.9.3
nixpkgs.python36Packages.flask_principal                                 python3.6-Flask-Principal-0.4.0
nixpkgs.python36Packages.flask-pymongo                                   python3.6-Flask-PyMongo-0.5.1
nixpkgs.python36Packages.flask-restful                                   python3.6-Flask-RESTful-0.3.6
nixpkgs.python36Packages.flask_script                                    python3.6-Flask-Script-2.0.6
nixpkgs.python36Packages.flask-silk                                      python3.6-Flask-Silk-0.2
nixpkgs.python36Packages.flask_sqlalchemy                                python3.6-Flask-SQLAlchemy-2.1
nixpkgs.python36Packages.flask_testing                                   python3.6-Flask-Testing-0.7.1
nixpkgs.python36Packages.flask_wtf                                       python3.6-Flask-WTF-0.14.2
```
Now that we know what packages we need to import, we can declare them in our
list of dependencies, which We'll call `dependencies`:
```
dependencies = [
  python3
  python36Packages.flask
]
```

Notice the pattern of our package names.  `python3` refers to the `python3`
package, which we can see is pinned to `python3-3.6.5`.  Likewise, our Flask
package, `python36Packages.flask` is pinned to `python3.6-Flask-0.12.2`.  For
python packages, the general pattern for package names will be
`pythonXYPackages.PackageName`

#### Configuring our environment

The `stdenv` derivation will import several additional nix packages for us.
Among them is the `buildenv` package, which allows us to setup a build
environment.  This build environment is what we'll use for interactive
development in our application.

The `buildEnv` package has several variables that we can set to fine-tune our
build environment.  For our purposes we are concerned with setting the name of
our environment and the paths that we want to import into our working
environment.

We set variables inside of a nix package using braces.  In the snippet below we
are bringing in the `buildEnv` package with our custom name and import paths:

```
env = buildEnv {
  name = name;
  paths = dependencies;
};
```

#### Our Finished nixpkg

Putting all of the above together, we find ourselves with a rather small nixpkg
that brings in everything we need for our application:

```
with import<nixpkgs> {};
stdenv.mkDerivation rec {
  name = "env";
  dependencies = [
    python3
    python36Packages.Flask
  ];
  env = buildEnv {
    name = name;
    paths = dependencies;
  };
}
```

## Running Our Application

Once we've set up our `default.nix` file, running our application inside of our
nix environment is as simple as calling `nix-shell`.  This will set up a local
environment with our requested packages available in our `$PATH` so that all of
our dependencies are available.

```
rebecca@debian:~/projects/py-todo$ nix-shell
[nix-shell:~/projects/py-todo]$ which python3
/nix/store/96wn2gz3mwi71gwcrvpfg39bsymd7gqx-python3-3.6.5/bin/python3
[nix-shell:~/projects/py-todo]$ which flask
/nix/store/swamiw3ygj9wws3spa44wgmjxhlldj3y-python3.6-Flask-0.12.2/bin/flask
[nix-shell:~/projects/py-todo]$ ./run_server
 * Serving Flask app "todo_server"
 * Running on http://127.0.0.1:5000/ (Press CTRL+C to quit)
```

## Credits and Further Reading

- [How I Develop with NixOS](https://ocharles.org.uk/blog/posts/2014-02-04-how-i-develop-with-nixos.html)
- [Isolated Development Environment using Nix](https://ariya.io/2016/06/isolated-development-environment-using-nix)
- [The Nix Manual](https://nixos.org/nix/manual/)
