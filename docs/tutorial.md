# Getting started with SitePipe

Hey there! This is a quick walkthrough of building a simple blog with SitePipe.
Don't let it fool you though; SitePipe can build any type of static site,
blogs, project pages, resumes, whatever!

A SitePipe site takes the form of a Haskell *executable*. What this means is
that SitePipe itself is just a *library* which helps you build a
site-generator. The downside is that this means you need to build an executable
each time you make changes, but the benefit is that you can customize the site
builder however you like! Let's get started!

# Setting up

We're going to be using [Haskell
Stack](http://seanhess.github.io/2015/08/04/practical-haskell-getting-started.html)
to manage our Haskell project for us, so go ahead and set that up if you
haven't already. Next I'll get you to clone down this repository somewhere on your
computer:

- `git clone https://github.com/ChrisPenner/SitePipe sitepipe`

We'll start our site from the provided _*starter-template*_, so let's copy that
into a new directory for our project;

- `cp -r ./sitepipe/examples/starter-template blog`
- `cd blog`
- `ls`

Inside the `blog` directory we'll see a few things of interest;

- `app`
    - Inside `app` there's a `Main.hs` is where the code for our site-generator lives
- `dist`
    - This file is the output for our generator and contains the code for serving our site!
    - Don't bother changing any files in here, they'll get overwritten the next time you build the site.
- `site`
    - This is where the source-files for our site live, everything from templates, images, css, and posts lives in
        here!
- `stack.yaml`
    - This is a file which tells stack how to build our project, it should be set up for you already.
- `starter-template.cabal`
    - This a file which represents the haskell project, it states the dependencies you need and the version of Haskell
        you're relying on. If you want to include other haskell libraries you'll add them in here.

# Building Our Site

Okay awesome! Let's first make sure that everything is set up properly; let's delete the output directory and run
the generator to build the site, make sure you're inside the `blog` directory for these!

- `rm -rf ./dist`
- `stack build && stack exec build-site`

The stack commands will build our site generator, then execute it from the current directory to build the site!

The first time we run it stack will download a few dependencies and build them too, so it'll probably take a little
while, but subsequent builds will be faster don't worry!

Hopefully you'll now notice that the `dist` folder has been resurrected! Let's take a look at what was built for us:

```
$ ls dist/
index.html posts
$ ls dist/posts
article.html
```

Cool stuff, looks like it added an index.html which is the table of contents
for the site, and it added a directory which contains all of the posts!

If you open up `index.html` in your browser (`open dist/index.html` if you're
on a mac) you'll notice that the main page loads up (and looks hideous) but
that the links don't work! This is because most links in SitePipe are *absolute
links* which tend to work better on the web, but unfortunately don't work so
well when you're testing locally. For our testing purposes let's set up a tool
to help us see the result. We're going to get the [Serve](https://www.npmjs.com/package/serve) tool which boots up
a lightweight server to serve files from the given directory.

- Make sure nodejs is installed: `brew install node`
- Install serve: `npm install -g serve`
- Serve the site: `serve ./dist/`

Sweet, once serve boots up it should tell us which port to visit in the browser; usually [http://localhost:3000](http://localhost:3000),
and if we go there we can see our site, and now the links should work!

Legit! We're on our way, now let's get to the cool stuff!

# Functionality Walkthrough

In your favourite text editor go ahead and open up the `app/Main.hs` file inside of the blog directory,
you'll see something vaguely similar to this:


```haskell
{-# language OverloadedStrings #-}
module Main where

import SitePipe

main :: IO ()
main = site $ do
  -- Load all the posts from site/posts/
  posts <- resourceLoader markdownReader ["posts/*.md"]

  let indexContext :: Value
      indexContext = object [ "posts" .= posts
                            , "url" .= ("/index.html" :: String)
                            ]

  -- Render index page and posts
  writeTemplate "templates/index.html" [indexContext]
  writeTemplate "templates/post.html" posts
 ```

 Let's walk through it!

Don't worry too much about the `OverloadedStrings` bit at the top, that tells
Haskell to enable an extension which intelligently converts any strings we type
into the right type for us, let's move past it.

The next few lines set up our file as a module, and most importantly imports the `SitePipe` library that we'll be
using to build our site-generator.

Then we reach `main`; This is where the magic happens, the first thing we have
is `site`, which sets up a context where we can use SitePipe rules to do our
bidding. Everything that follows is wrapped up in do-notation inside the
`SiteM` monad. `SiteM` isn't all that important really, it just provides some
global settings to parts of the app, if you need to use IO inside the
do-notation you can just wrap it using `liftIO` and carry on your way.

Anyways, hope that didn't confuse you too much, let's keep moving!

## Loaders

The first important bit is 

```haskell
  posts <- resourceLoader markdownReader ["posts/*.md"]
```

We're using the `resourceLoader` helper from SitePipe which... loads resources...
what a surprise! Inside SitePipe things that load resources from files are called *Loaders*. Let's look at it's type: 

```haskell
resourceLoader :: (String -> IO String) -> [GlobPattern] -> SiteM [Value]
```

Okay cool; The `(String -> IO String)` is called a `Reader`; it processes the
contents of a file (excluding the metadata block) and returns a string
containing the processed result. What's this good for? Well as we can see in
the example we're passing it a `markdownReader` which reads markdown content
into html! There's a few simple readers provided with SitePipe (like
`textReader`), and you can make your own too! To learn about using pandoc
readers or making your own, check out the hackage docs under `SitePipe.Readers`.

Okay, next in the signature is `[GlobPattern]`, If we click through in the
hackage docs we can see that `GlobPattern` is actually an alias to `String`. So
basically we can pass `resourceLoader` a list of globbing patterns to match
files within the `site` directory. This follows standard shell globbing syntax
(NOT a regex). Here we just match any markdown files in the posts directory of
`site`. Note that the glob must be RELATIVE to the `site` directory.

Lastly in the signature is `SiteM [Value]`, we can pretty much ignore `SiteM`
here and focus on the `[Value]`. We'll be seeing a lot of `Value` as we go
through this tutorial so you might as well get used to it! It's a type from the
`Data.Aeson` library, a very popular Haskell library for working with JSON or Yaml
formats. We use them in SitePipe because they're well supported, and are able to 
represent data that might be missing certain keys or values. They can be a bit of
a pain to work with, but there's some tools that can really help us out with that!
It's likely that you'll get a bit hung up on an issue with them at some point when
you're working on your site, and that's okay!  Here's a [guide](https://artyom.me/aeson)
you can take a look at if you get stuck!

## Meta Blocks

Okay, so we've got a list of the loaded resources, but what's inside those Aeson objects?
Great question! The `resourceLoader` will take care of parsing a meta-block from each resource
you load, and the resulting resource will contain data matching the meta-block; alongside a few
generated keys; probably best to look at an example. Here's a sample `my-post.md` file:

```markdown
---
title: My Post
author: Me myself and I
tags: [writing, blogging]
---

# My Title

- point 1
- point 2

```

You'll see at the top of the post that we've included a block of meta-information in-between `---`.
SitePipe understands like this block and you can use them to add info to any resources you load in.
It's in a `yaml` format, so regular yaml rules apply. 

So let's assume that our globbing patterns match this post, here's what happens next:

-   Haskell will be read in the file from the file-system
-   The `resourceLoader` will pull off the yaml block (if it exists) and will
    parse the yaml into an Aeson Value.
-   The remaining string containing the contents of the file will be run
    through the provided **Reader** function.
-   SitePipe calculates a `url` parameter by taking the relative filepath of
    the file, making it absolute, and adding an '.html' extension. If this post
    as at `posts/my-post.md` then `url` will be set to `/posts/my-post.html`.
-   Lastly everything is combined into a single Aeson Value containing the
    metadata, the `url` parameter, and a `content` key which contains the
    output from the reader function.

Nice! So looking back at what we ran:

```haskell
  posts <- resourceLoader markdownReader ["posts/*.md"]
```

We've now got a list of posts encoded as Aeson Values.

## Writers

Let's jump down a little bit (we'll come back to the `index` stuff soon), looking at:

```haskell
  writeTemplate "templates/post.html" posts
```

We're calling `writeTemplate` with the location of a template relative to the `site` directory.
After the template location we give it a list of resources to render through the template. In this case
the list of posts that we loaded! Let's talk through what happens here:

-   `writeTemplate` loads up the template and parses it as a [Mustache
    Template](https://mustache.github.io/mustache.5.html), it'll print any
    template parsing errors if they occur.
-   Next it iterates through each post and renders the template using that post
    as the context for the template. You can use any values stored in the post
    metadata inside your template! Any warnings or errors will be printed.
-   After the template has rendered `writeTemplate` will save the result in
    file referred to by the `url` property, so if the `url` is
    `/posts/my-post.html` it will be saved to `dist/posts/my-post.html`.

So after that line we'd see that each post has been rendered through its template into the dist directory.

## Custom Contexts

Let's jump back to the parts we skipped now; 

```haskell
  let indexContext :: Value
      indexContext = object [ "posts" .= posts
                            , "url" .= ("/index.html" :: String)
                            ]

  -- Render index page and posts
  writeTemplate "templates/index.html" [indexContext]
```

So here we're looking at how we handle the table of contents for the site. If we want to use a template with
`writeTemplate` for our index page we'll need to pass it a list of resources, but the context for an index
page isn't something we can directly load from a file; it's a combination of things we've already loaded!
No problem though, we can build up a context ourselves using combinators from `Data.Aeson`; `Data.Aeson` is
re-exported from SitePipe, so we don't even need to worry about importing.

We're using `object` from `Data.Aeson` to build our page context as an Aeson
Value from a list of key-value pairs. We create the key-value pairs by using
`"key" .= value`, where `value` is some value that Aeson knows how to
serialize. Aeson can handle primitives like strings, text, lists, maps,
numbers, etc. so it can handle most things you'd want to throw at it. In our case we just need our context to
have the posts we loaded (as a list) and we need a `url` to tell `writeTemplate` where to save the result.

Once we've built the context we just pass it to `writeTemplate`, wrapping it up in
a list because that's what `writeTemplate` expects.

## Templates

Let's look at the template we're using for the posts so we can understand how that works!

```html
<!DOCTYPE html>
<html>
    <head>
        <meta charset="utf-8" />
        <meta name="viewport" content="width=device-width" />
        <title>{{title}}</title>
    </head>
    <body>
        <article>
            <h1>{{title}}</h1>
            <h2>By <i>{{author}}</i></h2>
            {{{content}}}
        </article>
    </body>
</html>
```

This is about as simple as it looks; the `{{}}` blocks let us write out any String or Text values straight into the
html. Note that we use triple braces `{{{}}}` for the `content` tag, which prevents html escaping. Our content is html
from the `markdownReader` so we want it to be inserted as-is.

Let's take a look at `index.html` too!

```html
<!DOCTYPE html>
<html>
    <head>
        <meta charset="utf-8" />
        <meta name="viewport" content="width=device-width" />
        <title>My Blog</title>
    </head>
    <body>
        <h1>Posts</h1>
        <ul>
        {{#posts}}
            <li><a href="{{url}}">{{title}}</a></li>
        {{/posts}}
        </ul>
    </body>
</html>
```

The only thing of note here is `{{#posts}}` and `{{/posts}}` which are the opening and closing tags for Mustache,
they do different things based on the context they're used in. In this case they tell Mustache to iterate over the list
of `posts` and to bring each one in scope and render the inside of the tag, so the `{{url}}` and `{{title}}` come from
each post.

We can also use `{{#thing}}{{/thing}}` tags to conditionally render something based on the existence of the value,
(i.e. if `thing` doesn't exist then the inside of the tag won't be rendered), or you can use them to run a function
over the rendered contents of the tag; see `siteWithGlobals` in the hackage docs for SitePipe for more information.

Awesome! We now understand how to render a basic site! Let's try customizing something!

# Custom Functionality

## Reading Time Estimate

I've always thought it was cool when blogs give you an estimate of how long it takes to read an article at the top.
Let's add that into our site!

We'll start from the same starter template:

```haskell
{-# language OverloadedStrings #-}
module Main where

import SitePipe

main :: IO ()
main = site $ do
  -- Load all the posts from site/posts/
  posts <- resourceLoader markdownReader ["posts/*.md"]

  let indexContext :: Value
      indexContext = object [ "posts" .= posts
                            , "url" .= ("/index.html" :: String)
                            ]

  -- Render index page and posts
  writeTemplate "templates/index.html" [indexContext]
  writeTemplate "templates/post.html" posts
 ```

For each post in the list of `posts` we want to compute an estimate of reading time and store it with that post. 
Effectively this is just a transformation over each post, which sounds like a `map` operation to me! Since we want
to apply this to every post we might as well just tack it onto the `resourceLoader`;


```haskell
  posts <- fmap addReadingTime <$> resourceLoader markdownReader ["posts/*.md"]
```

Note that since the `resourceLoader` is of type `SiteM [Value]` we need to fmap twice to get all the way to Value,
so we use `fmap` once and then `<$>` to get all the way to running our function over `Value`. Now let's write the 
`addReadingTime` funciton!


```haskell
addReadingTime :: Value -> Value
addReadingTime post = ???
```

Hrmm, we know that `post` is an Aeson Value, but how can we actually access and alter its contents? There are
a few options here, there are combinators provided with `Data.Aeson`, but they're rather inconvenient. I'm going
to recommend we go with lenses for this particular task. Lenses can take a while to learn and are quite a complex
concept, so you may need to familiarize yourself with the basics as we go forward but I'll try my best to explain
what we're doing at each step.

## Working with Values

One nice thing is that SitePipe re-exports both `Control.Lens` and `Data.Aeson.Lens`, so we don't actually need to 
add any new imports for these next steps, but take note that this is where the lens combinators come from.

Let's go!

First we need to get the content of the post so that we can get a measure of its length:

```haskell
-- Add this new import at the top
import qualified Data.Text as T

-- Back to the new function:
addReadingTime :: Value -> Value
addReadingTime post = ???
    where
      content :: T.Text
      content = post ^. key "content" . _String
```

Okay! First things first, Aeson works primarily with `Text` instead of
`String`s, so we'll need to import that. So we're using a few new things here,
first up is the `^.` combinator from `Control.Lens`, this is pronounced 'view'
and means we want to access and return the target of some lens or traversal.

Next we introduce the `key` traversal. Basically you can give `key` a key
of an object that you want to drill into and you can then access what's inside.
But remember! Not all `Value` objects are `Object`s; an Aeson `Value` could be
an `Array`, or a `String`, or even `Null`! When we say that `key` is a
traversal what we mean is that the desired target may or may not exist, and
everything that happens after we drill into that potentially missing target is
affected by that. We start by saying that we want to access the "content" key
of the post if post is an object and if the "content" key exists. After that we
use `_String` to say that we expect text to be stored in the value at
"content", again if something else was stored there then the whole traversal
would fall to a failure case. In the case that there IS text stored there we'll
get the result out and store it in the 'content' binding. BUT if any of our
assumptions fail along the way, then the whole traversal fails. 

We don't like dealing with runtime exceptions in Haskell, so instead of
throwing an exception the traversal will try to return a 'default' value for
the type we expected. It finds this default by using the `mempty` value from
the type you're accessing if it's a Monoid. In our case Text is a monoid, so if
anything was missing we would return an empty Text object.

Ugh, that sounds complicated, but the result is pretty simple, we'll get the
content of the post if it exists (which it should for all posts), if content is
missing for any of our posts we just get an empty Text; which is a pretty
reasonable fallback.

Next let's make an estimation of reading time based on the content! 
For the purposes of this tutorial I'm going to make the very naive
estimation that people can read a word every second.

```haskell
addReadingTime :: Value -> Value
addReadingTime post = ???
    where
      content :: T.Text
      content = post ^. key "content" . _String
      readingTime :: T.Text
      readingTime = T.pack (show (div (length $ T.words content) 60) ++ " mins")
```

Okay! We've computed the estimated reading time, now let's add that as a key
into the object. Here's what most people would probably try first but this
WON'T WORK:

```haskell
post & key "readingTime" .~ String readingTime
```

This is a good thought, but this actually won't affect a post at all, remember
earlier how we said that the `key` can fail if the key doesn't exist? Well in this
case we're telling lens to access the key `readingTime` IF it exists, then set something
into it, but the key doesn't exist so the whole thing fails and nothing gets set!

Here's how we fix it:

```haskell
addReadingTime :: Value -> Value
addReadingTime post = post & _Object . at "readingTime" .~ Just (String readingTime)
    where
      content :: T.Text
      content = post ^. key "content" . _String
      readingTime :: T.Text
      readingTime = T.pack (show (div (length $ T.words content) 60) ++ " mins")
```

So the trick here is that first we assert that post should be an object, if
it's not then we want the whole thing to fail, but once we drill into the
Object we can use lenses that work over normal haskell Maps; `at` is one such
lens which returns a `Just value` at the key if it exists, or `Nothing` if it
doesn't. It's a bit of a nuance, but this means we can also SET the key to
`Just value` and it'll apply. Don't worry about the details, but this is how we
need to set the value for it to take.

And that's it! We've added reading time to every post object; here's what the
whole thing looks like:

```haskell
{-# language OverloadedStrings #-}
module Main where

import SitePipe
import qualified Data.Text as T

addReadingTime :: Value -> Value
addReadingTime post = post & _Object . at "readingTime" .~ Just (String readingTime)
    where
      content :: T.Text
      content = post ^. key "content" . _String
      readingTime :: T.Text
      readingTime = T.pack (show (div (length $ T.words content) 60) ++ " mins")

main :: IO ()
main = site $ do
  -- Load all the posts from site/posts/
  posts <- fmap addReadingTime <$> resourceLoader markdownReader ["posts/*.md"]

  let indexContext :: Value
      indexContext = object [ "posts" .= posts
                            , "url" .= ("/index.html" :: String)
                            ]

  -- Render index page and posts
  writeTemplate "templates/index.html" [indexContext]
  writeTemplate "templates/post.html" posts
 ```

Last thing we need to do is add it to the template so it shows up! Here's `post.html`

```html
<!DOCTYPE html>
<html>
    <head>
        <meta charset="utf-8" />
        <meta name="viewport" content="width=device-width" />
        <title>{{title}}</title>
    </head>
    <body>
        <article>
            <h1>{{title}}</h1>
            <h2>By <i>{{author}}</i></h2>
            <div><i>Reading Time: {{readingTime}} </i></div>
            {{{content}}}
        </article>
    </body>
</html>
```

That's it! We've made modifications to our Value objects representing posts and it showed up in the template!
Nice job!


Remember that resources in SitePipe are just values that you can edit however you want!

That wraps up the basics of the tutorial, good luck!
