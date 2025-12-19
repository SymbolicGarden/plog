# Plog — A Dynamic Prolog Blog Server

[![SWI-Prolog](https://img.shields.io/badge/SWI--Prolog-9.x-6B2FBF?logo=prolog&logoColor=white)](https://www.swi-prolog.org/)
[![License: Apache-2.0](https://img.shields.io/badge/License-Apache--2.0-blue.svg)](LICENSE)

What does a blog engine look like if you write it in logic? 

### Dynamic Markdown -> HTML server in pure Prolog. 

Yes, you can write a web server with Prolog. Plog is a blog engine written in pure Prolog that dynamically reads Markdown files, parses them at request time, and serves clean HTML using a minimal prolog HTTP server. 

**No frameworks. No dependencies. No JavaScript. Just Prolog.**

To add a new blog entry, simply write it in markdown and add it as prolog file to the contents folder. The prolog engine will dynamically parse the markdown into html recursively for display. 

Check the site live: https://blauanarchy.org 


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


## Getting started

- Install the pack:
```
swipl pack install plog
```

- To overwrite the blog title, description and site link, create `my_prolog_config.pl`, or modify `my_prolog_config.pl` from the installed location:

```
:- module(my_plog_config, []).
:- multifile plog:site_title/1, plog:site_link/1, plog:site_description/1.

plog:site_title('BlauAnarchy\'s Blogs'). % Change this with your own blog title
plog:site_link('https://blauanarchy.org'). % Change this with your own blog link
plog:site_description('A Blog site on Symbolic Coherence, written in pure prolog.'). % Change this with your own blog description
```

- To add blog contents, create a contents directory: `contents` and put your blogs files e.g. `my_first_blog.pl` inside. Checkout `contents` folder for examples. The blog files should be written in markdown and look like this:

```
content("Put your markdown contents here").
```

- If you would like to add images to your blog markdown, you can create an images folder `images` and drop the files there.

- Load the configuration file, the pack, and run the server:
```
[my_prolog_config].
use_module(library(plog)).
server(Port).
```

- Open the index page, you will see it lists all posts. Each post is served dynamically.

```
http://localhost:Port
```

That is the entire authoring model.

## Design philosophy

This project intentionally avoids complexity. No abstractions unless justified. Everything is visible and understandable at a glance.

## Authorship

Built by Zhongying Qiao.

If you find this project interesting, you’re welcome to star the repository or reuse it as a base for your own Prolog systems.

## License

Apache License 2.0  
Copyright © Zhongying Qiao 2025
