# Plog — A Dynamic Prolog Blog Server

[![SWI-Prolog](https://img.shields.io/badge/SWI--Prolog-9.x-6B2FBF?logo=prolog&logoColor=white)](https://www.swi-prolog.org/)
[![License: Apache-2.0](https://img.shields.io/badge/License-Apache--2.0-blue.svg)](LICENSE)

What does a blog engine look like if you write it in logic? 

### Dynamic Markdown -> HTML server in pure Prolog. 

Yes, you can write a web server with Prolog. Plog is a blog engine written in pure Prolog that dynamically reads Markdown files, parses them at request time, and serves clean HTML using a minimal prolog HTTP server. 

**No frameworks. No dependencies. No JavaScript. Just Prolog.**

To add a new blog entry, simply write it in markdown and add it as prolog file to the contents folder. The prolog engine will dynamically parse the markdown into html recursively for display. 

Check the site live: https://blauanarchy.org 

---

## Features

- Dynamic Markdown parsing on request
- HTTP server and router
- HTML rendering via `http/html_write`
- Built-in RSS feed
- Simple content model

### Supported Markdown constructs

- Headings
- Paragraphs
- Links
- Images
- Inline code and code blocks
- **Bold**, _italic_
- Blockquotes
- Horizontal rules

---

## Getting started

Install the pack:
```
swipl pack install plog
```

To overwrite the blog tile, description and site link, modify `my_prolog_config.pl`, and load it into swipl REPL:

```
[my_prolog_config].
```
Load the pack, then run the server:
```
use_module(library(plog)).
```

Run the server
```
server(Port).
```

Open the index page, you will see it lists all posts. Each post is served dynamically.

```
http://localhost:Port
```

Writing posts: Add post as a Prolog file in the `contents/` directory:

```
content("Your markdown content here").
```

If you want to add images as markdown, add the corresponding images to `/images` directory

That is the entire authoring model.

## Design philosophy

Plog intentionally avoids abstraction and indirection.

- No template language

- No hidden build step

- No configuration DSL

- No static export pipeline

The goal is to keep the system inspectable, traceable, and understandable to someone who reads Prolog.

## Authorship

Built by Zhongying Qiao.

If you find this project interesting, you’re welcome to star the repository or reuse it as a base for your own Prolog systems.

## License

Apache License 2.0  
Copyright © Zhongying Qiao 2025
