# SitePipe

### Contents:

-   [What is it?](#what-is-it)
    -   [What's it look like?](#whats-it-look-like)
    -   [Wait, another static site generator? What about
        Hakyll/Jekyll?](#wait-another-static-site-generator-what-about-hakylljekyll)
-   [Getting Started](#getting-started)
    -   [Quick Start](#quick-start)
    -   [Tutorial](#tutorial)
-   [Concepts](#concepts)
    -   [How is SitePipe different from other
        solutions?](#how-is-sitepipe-different-from-other-solutions)
    -   [Data/Metadata](#datametadata)
    -   [Templating](#templating)
    -   [Loaders](#loaders)
    -   [Reader](#reader)
    -   [Writers](#writers)
    -   [Loader/Writers](#loaderwriters)
    -   [Utilities](#utilities)
-   [Issues/Troubleshooting](#issuestroubleshooting)

## What is it?

It's a simple to understand static site generator for making blogs, personal
websites, etc.

## What's it look like?

Here's a dead-simple blog generated from markdown files, you can see it in action in
[examples/starter-template](./examples/starter-template), or build on it in the [tutorial](./docs/tutorial.md)

```haskell
{-# language OverloadedStrings #-}
module Main where

import SitePipe

main :: IO ()
main = site $ do
  -- Load all the posts from site/posts/
  posts <- resourceLoader markdownReader ["posts/*.md"]

  -- Build up a context for our index page
  let indexContext :: Value
      indexContext = object [ "posts" .= posts
                            -- The url is where the index page will be written to
                            , "url" .= ("/index.html" :: String)
                            ]

  -- write out index page and posts via templates
  writeTemplate "templates/index.html" [indexContext]
  writeTemplate "templates/post.html" posts
```

## Wait, another static site generator? What about Hakyll/Jekyll?

Yup, yet another static site generator. The reason for it is that I tried using
Hakyll and Jekyll on different occasions and found there was too much magic
going on for me to understand how to customize things for my use-cases. They were
too opinionated without giving me escape hatches to wire in my own functionality.

When I tried Hakyll specifically I got really bogged down; what was a
`Compiler` monad? How does an `Item` work? How do I add a custom field? Why
couldn't I just edit data directly like I'm used to doing in Haskell?

# Getting Started

## Quick Start

The easiest way to get started is to clone this repo and try out the examples in the
[examples](./examples) directory. There's a starter-template which is a barebones
starting point, and also a slightly more complex blog with tags and an rss feed.
You can build either of the examples using [Stack](http://seanhess.github.io/2015/08/04/practical-haskell-getting-started.html)
by `cd`ing into the directory and running `stack build && stack exec build-site`.
This creates a 'dist' folder with the results of the build. A quick way to serve
the site is to use [Serve](https://www.npmjs.com/package/serve).

Serving a site with [Serve](https://www.npmjs.com/package/serve):
- `npm install -g serve`
- `serve dist`
- Navigate to the port which is serving (usually http://localhost:3000)

## Tutorial

Read the walkthrough of the system [HERE](./docs/tutorial.md); it'll run you through the basics
of how the system works and how to make your own customizations!

# Concepts

How is SitePipe different from other solutions?
-----------------------------------------------

Instead of dealing with complex contexts SitePipe works with *values*. Values
are loaded from files and can be rendered into html. What happens to the values
in-between is up to you!

SitePipe provides a bunch of helpers for you, but at the end of the day you can
fit the pipes together however you like.

## Data/Metadata

Metadata for posts and content is parsed from yaml into [Aeson's `Value`
type](https://hackage.haskell.org/package/aeson); Unlike Hakyll which depends
on Pandoc's metadata blocks which can only accept Strings as values, Aeson can
easily represent nested objects or lists inside your metadata, and there's a
rich ecosystem for working with Aeson types! You can load resources in as any
object which implements `FromJSON` (or just leave them as Aeson Values) and you
have the option to edit the objects directly without worrying about monadic or
external context.

## Templating

SitePipe has built-in support for [Mustache
Templates](https://mustache.github.io/mustache.5.html), specifically [Justus
Adam's implementation](https://hackage.haskell.org/package/mustache) in
Haskell. This lets you use a well established templating system in your site,
complete with template functions, partials, and iteration. Since the underlying
data is based on It's clear how templates will behave since resources are based
on Aeson's JSON types.

## Loaders

You can load resources in to work on them using a `Loader`, A loader simply
finds and loads files into resources by employing a `Reader` on some files. A
basic `resourceLoader` loader is provided, which will load all of the files
matching a set of file-globs through the provided reader and will return an
Aeson Value containing the relevant metadata and content. You should be able to
use resourceLoader for most things by customizing the reader function which you
pass it.

## Reader

A reader is a function of the type `String -> IO String`; the input is the file
contents which remain after a yaml header has been stripped off (if it exists).
The most common reader is the provided `markdownReader` which runs a markdown
document through pandoc's markdown processor and outputs html. You can write
your own readers if you like, either by making a function which operates over
the content of the document and matches `String -> IO String` or by using
the provided Pandoc helpers (`mkPandocReader`, `mkPandocReaderWith`) which
allow you to use any of Pandoc's provided document formats, and optionally specify
transformations over the pandoc document before it is rendered to html or some other
output format.

## Writers

Writers take a list of resources (anything with a ToJSON instance, often an
Aeson Value) and will write them to the output where the static site will be.
The most common writer is `writeTemplate` which will render the given resource
through a given template, but you can also use `textWriter`, or write your own
writer; either writing to disk using `liftIO` or by using the provided
`writeWith` combinator which given a transformation from a resource to a String
`(a -> SiteM String)` will write the result of the transformation to the place
specified by the resource's url.

## Loader/Writers

Some things don't fit into the previous categories. For example `copyFiles` and
`copyFilesWith` are simple tools which just copy the specified files over as-is
into the output directory. You pass either of them a list of file globs and the
resulting files will be copied over. `copyFiles` sends them to the same
relative filepath from the source directory to the output directory, while
`copyFilesWith` allows you to transform the filepath to specify a new location
for each file.

## Utilities

Sitepipe includes a few utilities which simply make working with sites easier.
The included utilities will expand as time goes on.

# Issues/Troubleshooting

Feel free to file an [issue](https://github.com/chrispenner/sitepipe/issues) if you run into any trouble!
