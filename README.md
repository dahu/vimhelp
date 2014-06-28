## vimhelp

Emulates Vim's `:help tag` lookup system.

**Note:** Requires [newLISP](http://www.newlisp.org/)

**Usage:**

```
> ./vimhelp '^x'
```

Will return a URL for appspot's online Vim documentation for CTRL-X

To run the bundled unit tests:

```
> newlisp ./test-vimhelp.lsp
```
