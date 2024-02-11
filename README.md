# denote-graph

This library adds graphing capabilities to your `denote` knowledge base.

It generates a [DOT](https://graphviz.org/) source code file, which can then be compiled to PNG, SVG, PDF and many others via your favourite engine ([dot](https://graphviz.org/docs/layouts/dot/), [neato](https://graphviz.org/docs/layouts/neato/), [sfdp](https://graphviz.org/docs/layouts/sfdp/), ...)

# Installation

Download it via git or via Melpa (coming soon).

Then add this to your `init` file:

```
(require 'denote-graph)
```


# Usage

Simply `M-x denote-graph-generate-dot-file`. 

The output DOT file will be named according to the value of `denote-graph-output-filename` (default is `zettelkasten.dot`).



