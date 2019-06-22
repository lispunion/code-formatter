A scheme code formatter

## Usage
```sh
$ scheme-format path/to/my/scheme/file.scm
```
Formatted scheme code is output to Standard Out. 

It is recommended that you integrate this with your favorite editor such that it can replace the current version of your code with the formatted one produced by this.

See [the GitHub Issues](https://github.com/masukomi/code-formatter/issues) for future enhancements.

## Features
* Runs from the command line
* Makes decisions about line breaks as well as indentation
* Sorts terms such as function definitions where this will not change the meaning of the program

## Current Status
A straight port of [code-formatter](https://github.com/lispunion/code-formatter) (previously known as "scheme-format") to Chicken 5.x with a minor amount of cleanup.


## Building From Source
Requires Chicken Scheme 5.x

Just run the `build.sh` file and add the resulting `scheme-format` file to your `$PATH`

If you edit the `etc.scm` or `format.scm` files you'll want to run

```
csc -s etc.scm -j etc
csc -s format.scm -j format
```
To regenerate the `.import.scm` and .so` files before running the bulid script,  or manually running `csc -static main.scm -o scheme-formatter`
