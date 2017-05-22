# SitePipe

## What is it?

It's a simple to understand static site generator for making blogs, personal
websites, etc.

## Wait, another static site generator? What about Hakyll/Jekyll?

Yup, yet another static site generator. The reason for it is that I tried using
Hakyll and Jekyll on different occasions and found there was too much magic
going on for me to understand how to customize things for my use-cases. They were
too opinionated without giving me escape hatches to wire in my own functionality.

When I tried Hakyll specifically I got really bogged down; what was a
`Compiler` monad? How does an `Item` work? How do I add a custom field? Why
couldn't I just edit data directly like I'm used to doing in Haskell?

# How is SitePipe different?

Sitepipe is based around the idea that everything is just data; it's a library
that provides a bunch of helpers for you, but at the end of the day; the way
you fit the pipes together is up to you! There are only a few main concepts to
understand in SitePipe, and hopefully you already know them from other projects
you've worked on!

# Concepts

Data/Metadata
-------------

Metadata for posts and content is parsed from yaml into [Aeson's `Value`
type](https://hackage.haskell.org/package/aeson); Unlike Hakyll which depends
on Pandoc's metadata blocks which can only accept Strings as values, Aeson can
easily represent nested objects or lists inside your metadata, and there's a
rich ecosystem for working with Aeson types! You can load resources in as any
object which implements `FromJSON` (or just leave them as Aeson Values) and you
have the option to edit the objects directly without worrying about monadic or
external context.

##   Templating

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
