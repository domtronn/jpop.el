# projectable.el #

A lightweight project framework I created for the main purpose of integration with my `JS-DEPENDENCY-INJECTOR` package, which requires associative lists of files with project ids and their locations.

# Installation #

Clone this repository and add it to Emac's `load-path`. Then require the file:
```
(add-to-list 'load-path "/path/to/cloned/repo")
(require 'projectable)
```
Then enable it globally to load the key mappings
```
(projectable-global-mode)
```

# Defining a project #

This package *(currently only)* defines projects using JSON files.
It defaults to looking in `projectable-project-directory` first.

If you load a *directory*, instead of a JSON file, it will just cache all of the files recursively within that directory.

An example JSON project file would look like this:
```JSON
{
	"id":"My Project",
	"dirs":[
		{ "dir":"~/path/to/project", "create-tags": true},
		{ "dir":"~/path/to/another/project", "create-tags":false }
	],
    "libs":[
        { "dir":"~/path/to/library", "create-tags": true, "id":"library-id"},
    ],
	"testing":{
		"path": "tests/specs",
		"extension": "Spec",
		"sourcePath": "src"          //Optional if files don't have an explicit source
	},
    "style":{
        "tabs":false,
        "indent":2
    }
}
```
Where the `id` property and the the `dirs/dir` property define paths required in that project. This is useful if you have multiple respositories for a single project.

The `testing` block defines how to find your tests, *e.g.* they are found in `tests/specs` and would look like `originalFileSpec.js`

The `style` block, at the moment, defines how to indent the files within the project.
The `tabs` property defines whether this projects uses **tabs** or **spaces** and the `indent` property sets the tab width/indent level of the project.

# Usage #
If you enable `projectable-global-mode` you will have access to the following keybindings:

Key Binding | Command | Effect 
--- | --- | ---
`C-c p c` | `projectable-change` | Change to a new project file/directory 
`C-c p r` | `projectable-refresh` | Refresh the list of files cached 
`C-c p f` | `projectable-ido-find-file` | Open a file from the cache 
`C-c p t` | `projectable-toggle-open-test` | Try and find the related test to the current class
`C-c p l` | `projectable-reformat-file` | Reformat the current file to use appropiate indentation
`C-c p p` | `projectable-visit-project-file` | Open the current project file for editing

It is well worth looking through `customize-grou RET projectable` to see what you can customise.

# Ctags and JavaScript #

Version 5.8 of Ctags doesn't really parse javascript correctly, and won't work properly, so to fix this, you can add a `.ctags` file in your home directory and add the following matchers
```
--tag-relative=yes
--exclude=.git,.svn

--langdef=js
--langmap=js:.js
--regex-js=/([A-Za-z0-9._$]+)[ \t]*[:=][ \t]*\{/\1/,object/
--regex-js=/([A-Za-z0-9._$()]+)[ \t]*[:=][ \t]*function[ \t]*\(/\1/,function/
--regex-js=/function[ \t]+([A-Za-z0-9._$]+)[ \t]*\(([^)])\)/\1/,function/
--regex-js=/([A-Za-z0-9._$]+)[ \t]*[:=][ \t]*\[/\1/,array/
--regex-js=/([^= ]+)[ \t]*=[ \t]*[^"]'[^']*/\1/,string/
--regex-js=/([^= ]+)[ \t]*=[ \t]*[^']"[^"]*/\1/,string/

--langdef=coffee
--langmap=coffee:.coffee
--regex-coffee=/^[ \t]*([A-Za-z.]+)[ \t]+=.*->.*$/\1/f,function/
--regex-coffee=/^[ \t]*([A-Za-z.]+)[ \t]+=[^->\n]*$/\1/v,variable/
--regex-coffee=/^[ \t]*((class ){1}[A-Za-z.]+)[ \t]+=[^->\n]*$/\1/v,object/
```
Which will allow for proper JavaScript tags creation. You can also copy `.ctags` from this project into your `HOME` directory.
These were provided by [@jackcviers's gist](https://gist.github.com/jackcviers/2128247). Please :star: it if you found it useful!

# Planned Features #
I'm trying to get this project in a nicer state for my own purposes, future features will include
- [x] `.gitignore` integration for filtering
- [x] Better integration for test
    - [x] opening
    - [ ] running
- [x] Tags generation Integration
    - [x] Ctags
    - [ ] Etags
- [ ] Hosting on [Melpa](http://melpa.org/#/)

#### Why not Projectile ####
I'm aware that [projectile](https://github.com/bbatsov/projectile) does a lot of this already and I would actively encourage you to try that package out as well!

The _main reason_ I wrote my own version was that I work on projects comprised of multiple directories that use [RequireJS 0.15.0](https://libraries.io/bower/rjs/0.15.0) and I was fed up of _manually_ typing the require paths for modules across multiple projects. So I wrote my own _JS Dependency Injector_ which uses an associative list of project ids _(the ones used in the require js mapping definition)_ to a list of files with their containing directories. This way I could build the require paths for Classes I wanted to include.

I made a plugin for [Sublime Text](http://www.sublimetext.com/) which does a similar thing, see [JSDependencyInjector.py](https://github.com/domtronn/jsdependencyinjector) for a better example of what I mean.
