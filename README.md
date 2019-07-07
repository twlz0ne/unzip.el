[![Build Status](https://travis-ci.com/twlz0ne/unzip.el.svg?branch=master)](https://travis-ci.com/twlz0ne/unzip.el)

# unzip.el

Unzip wrapper for emacs

## Installation

Clone this repository to `~/.emacs.d/site-lisp/unzip`. Add the following to your `.emacs`:

```elisp
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/unzip"))
```

## Usage

```elisp
(unzip from-url-or-local-file
       output-dir
       
       ;; -- optional --
       
       :strip-component 1 ;; Remove leading path from ouput
       :overwrite t       ;; Overwrite output dir
       :async-finish-fn   ;; Start async and callback when finishes (or just give a `t'
       (lambda (_result)  ;; if there is nothing todo)
         ...))
```
