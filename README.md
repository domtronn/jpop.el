# projectable.el #

**Projectable** is a lightweight _declarative_ project interaction package for Emacs. It is designed to let users _define_ their projects in json files in a similar fashion to _Sublime_ and _Atom_. It allows you to group a few directories together as a project so that your project can be composed of multiple repositories/directories.

On top of this, it provides a nice set of features for interacting with your projects including styling _(the ole' tabs vs spaces debate)_, running/opening test files and the creation of [ctags](http://ctags.sourceforge.net/). It also gives you the ability to quickly open directories _as a project_ and make all the files within easily accessible.

It was built from a need for a particular data structure to describe a project for use with my `JS-DEPENDENCY-INJECTOR` package for require.js projects.

The mechanism for reading the operating system is a Python script for efficiencies sake.

### Features ###

* Jump to a file within the project
* Toggle between code and its test file ( `main.js` <=> `mainSpec.js` )
* Reformat a file with proper tabs/spaces indentation levels
* Project specific styling/formatting
* Create throw-away project instances from directories
* Switch between buffers in a project

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
```javascript
{
	"id":"My Project",
	"dirs":[
		{ "dir":"~/path/to/project", "create-tags": true},
		{ "dir":"~/path/to/another/project", "create-tags":false }
	],
    "libs":[
        { "dir":"~/path/to/library", "create-tags": true, "id":"library-id"},
    ],
    "style":{
        "tabs":false,
        "indent":2
    }
}
```
Where the `id` property and the the `dirs/dir` property define paths required in that project. This is useful if you have multiple respositories for a single project.

The `style` block, at the moment, defines how to indent the files within the project.
The `tabs` property defines whether this projects uses **tabs** or **spaces** and the `indent` property sets the tab width/indent level of the project.

You can also override the use of `gitignore` on a project level, add a `gitignore: true/false` property to the project file, and this will take priority over the global variable.

# Usage #
If you enable `projectable-global-mode` you will have access to the following keybindings:

Key Binding | Command | Effect 
--- | --- | ---
`C-x p c` | `projectable-change` | Change to a new project file/directory 
`C-x p r` | `projectable-refresh` | Refresh the list of files cached 
`C-x p f` | `projectable-find-file` | Open a file from the cache 
`C-x p F` | `projectable-find-file-other-window` | Open a file from the cache in the other window
`C-x p E` | `projectable-extended-find-file` | Open a file from the any declared library cache
`C-x p E` | `projectable-extended-find-file-other-window` | Open a file from the any declared library cache in the other window
`C-x p t` | `projectable-toggle-open-test` | Try and find the related test file
`C-x p T` | `projectable-toggle-open-test-other-window` | Try and find the related test file in the other window
`C-x p l` | `projectable-reformat-file` | Reformat the current file to use appropiate indentation
`C-x p p` | `projectable-visit-project-file` | Open the current project file for editing
`C-x p b` | `projectable-switch-buffer` | Switch between buffers in the current project
`C-x p B` | `projectable-switch-buffer-other-window` | Switch between buffers in the current project and open in the other window
`C-x p k` | `projectable-kill-project-buffers` | Kill all of the buffers related to the current project
    
It is well worth looking through `customize-group RET projectable` to see what you can customise.

# Ctags and JavaScript #

Version 5.8 of Ctags doesn't really parse javascript correctly, and won't work properly with things like `etags-select`.

To fix this, you can add a `.ctags` file in your `HOME` directory and add the following matchers
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

__These were taken from [@jackcviers's gist](https://gist.github.com/jackcviers/2128247). Please :star: it if you found it useful!__

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
- [x] Project grouped buffers

#### Why not Projectile ####
I'm aware that [projectile](https://github.com/bbatsov/projectile) does a lot of this already and I would actively encourage you to try that package out as well!

The _main reason_ I wrote my own version was that I work on projects comprised of multiple directories that use [RequireJS 0.15.0](https://libraries.io/bower/rjs/0.15.0) and I was fed up of _manually_ typing the require paths for modules across multiple projects. So I wrote my own _JS Dependency Injector_ which uses an associative list of project ids _(the ones used in the require js mapping definition)_ to a list of files with their containing directories. This way I could build the require paths for Classes I wanted to include.

I made a plugin for [Sublime Text](http://www.sublimetext.com/) which does a similar thing, see [JSDependencyInjector.py](https://github.com/domtronn/jsdependencyinjector) for a better example of what I mean.
