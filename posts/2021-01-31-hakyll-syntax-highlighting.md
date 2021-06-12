---
author: Rebecca Skinner
title: Syntax Highlighting in Hakyll
date: 2021-01-31
summary: "Learn how to generate a source doe highlighting stylesheet from hakyll with pandoc"
categories: ["haskell", "hakyll"]
description: "
Hakyll popular among haskell developers for building websites and personal
blogs, but building a code-heavy hakyll site can be tricky if you're not skilled
at design (like me) since the default hakyll configuration doesn't come with
support for syntax highlighting of code blocks. In this article you'll learn an
easy way to get syntax highlighting for code in your hakyll blogs using pandoc.
"
---

[Hakyll](https://jaspervdj.be/hakyll/) is a [static site
generator](https://jamstack.org/what-is-jamstack/)(SSG). Like most SSGs, hakyll
lets you author sites quickly, often by writing nothing more than some
markdown. SSGs are particularly popular among people writing about software, and
so as you would expect syntax highlighting support is a popular requirement.

Hakyll, which makes use of [pandoc](https://pandoc.org/) for translating
documents to HTML, has first class support for generating markup with syntax
highlighting information, but it does not automatically generate the stylesheets
necessary for the code to appear with highlighting on your generated page. The
official documentation provides some information on how to manually curate this
syntax highlighting stylesheet, but in this article you'll learn how to instead
have pandoc automatically generate a stylesheet from a given pandoc color
scheme.

TL;DR: If you want to skip the exposition, you can [jump here](#solution) to get
to the solution to having hakyll auto-generate CSS for syntax highlighting your
embedded code.

# Syntax Highlighting with Pandoc and Markdown

Pandoc's markdown processor is able recognize source code embedded in markdown
files using code blocks:

````markdown
# Printing Hello World

Consider this haskell function that will print some text to the screen:

```haskell
printHello :: IO ()
printHello = putStrLn "Hello, World"
```

In this example, we'll print the message "Hello, World" to the screen
````

```haskell
printHello :: IO ()
printHello = putStrLn "Hello, World"
```

In this example, we've embedded a haskell source code block into our
markdown. The backticks (\`) start a code block, and the text
(`haskell`) tell pandoc what language to use when generating higlighting.

# Reviewing the Generated Source

If you were to build the markdown snippet above and look at the output file,
you'll see that pandoc has generated a new `div` with the `sourceCode`
attribute. Within this `div`, as you can see, we have a number of different CSS
classes, each representing some part of our code that should be given some
particular highlighting.

```html
<div class="sourceCode" id="cb2">
  <pre class="sourceCode haskell">
    <code class="sourceCode haskell">
      <span id="cb2-1">
        <a href="#cb2-1" aria-hidden="true">
        </a>
        <span class="ot">printHello ::
        </span>
        <span class="dt">IO
        </span> ()
      </span>
      <span id="cb2-2">
        <a href="#cb2-2" aria-hidden="true">
          </a>printHello
          <span class="ot">=
          </span>
          <span class="fu">putStrLn
          </span>
          <span class="st">&quot;Hello, World&quot;
          </span>
      </span>
    </code>
  </pre>
</div>
```

If you use hakyll to build the site and load it up in a browser, you'll see that
although pandoc has helpfully annotated the source code, we still don't have any
highlighting on the text.

<a id="solution"/>

# Using Hakyll Pandoc to Auto-Generate Your Syntax CSS File

To get hakyll to auto-generate your syntax highlighting CSS you'll need to edit
the code for your site. If you're using the defaults from `hakyll-init` open up
`site.hs`. We'll need to start by adding a couple of new imports:

```haskell
import Text.Pandoc.Highlighting (Style, breezeDark, styleToCss)
import Text.Pandoc.Options      (ReaderOptions (..), WriterOptions (..))
```

Next up, we'll want to settle on a color theme that we want to have pandoc use
when generating our CSS syntax file. I'll call this `pandocCodeStyle`.

```haskell
pandocCodeStyle :: Style
pandocCodeStyle = breezeDark
```

To explicitly enable source code highlighting, or indeed to do any other sort of
configuration to pandoc from within hakyll, we'll make use of the
`pandocCompilerWith` function. This has the type:

```haskell
pandocCompilerWith :: ReaderOptions -> WriterOptions -> Compiler (Item String)
```

where `ReaderOptions` and `WriterOptions` are the pandoc options types that
we imported earlier.  For now we'll stick with the default reader options. For
the writer, we will override the `writerHighlightStyle` field to pass in our
site's style:

```haskell
pandocCompiler' :: Compiler (Item String)
pandocCompiler' =
  pandocCompilerWith
    defaultHakyllReaderOptions
    defaultHakyllWriterOptions
      { writerHighlightStyle   = Just pandocCodeStyle
      }
```

Update any existing calls to `pandocCompiler` to use your new function. For
example, if you are using a default `hakyll-init` site:

```haskell
-- In main you should have some code that matches "posts/*" and compiles the
-- items there with pandoc.
match "posts/*" $ do
    route $ setExtension "html"
    compile $ pandocCompiler'
        >>= loadAndApplyTemplate "templates/post.html"    postCtx
        >>= loadAndApplyTemplate "templates/default.html" postCtx
        >>= relativizeUrls
```

Now that we've made sure we're building our markup with the appropriate pandoc
options, we need to actually generate our syntax file. To do that, we need to
create a new CSS file with by calling `create`. We'll populate it by calling
pandoc's `styleToCss` function, which will generate the contents of our new CSS
file:

```haskell
create ["css/syntax.css"] $ do
  route idRoute
  compile $ do
    makeItem $ styleToCss pandocCodeStyle
```

Now that our CSS file will be generated, we just need to add one final change:
we have to link to it.

The most straighforward way to do this is to add a new link in your top-level
page template (`default.html` if you are using the standard hakyll setup):

```html
<!doctype html>
<html lang="en">
    <head>
        <meta charset="utf-8">
        <meta http-equiv="x-ua-compatible" content="ie=edge">
        <meta name="viewport" content="width=device-width, initial-scale=1">
        <link rel="stylesheet" href="/css/default.css" />
        <link rel="stylesheet" href="/css/syntax.css" />
    <!-- The rest of your default page goes here-->
```

Finally, rebuild your hakyll site generator, and use it to generate the
site. You should be able to load it up locally and see your colorful new source
code snippets in action.

```
user@host:~/projects/blog$ cabal new-build
Up to date
user@host:~/projects/blog$ cabal exec site -- build
Initialising...
  Creating store...
  Creating provider...
  Running rules...
Checking for out-of-date items
Compiling
Success
user@host:~/projects/blog$ cabal exec site -- watch
Listening on http://127.0.0.1:8000
Initialising...
  Creating store...
  Creating provider...
  Running rules...
Checking for out-of-date items
Compiling
Success
```
