# projectable.el #

A lightweight project framework I created for the main purpose of integration with my `JS-DEPENDENCY-INJECTOR` package, which requires associative lists of files with project ids and there locations.

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
	"projectId":"My Project",
	"project":[
		{ "dir":"~/path/to/project" },
		{ "dir":"~/path/to/another/project" }
	],
	"testing":{
		"path": "tests/specs",
		"extension": "Spec.js"
	},
	"tabs":false,
	"indent":2,
}
```
Where the two properties in the `project` array define paths required in that project. This is useful if you have multiple respositories for a single project.

The `testing` block defines how to find your tests, *e.g.* they are found in `tests/specs` and would look like `originalFileSpec.js`

The `tabs` property defines whether this projects uses **tabs** or **spaces**

# Usage #
If you enable `projectable-global-mode` you will have access to the following keybindings:

Key Binding | Command | Effect 
--- | --- | ---
C-c p c | `projectable-change` | Change to a new project file/directory 
C-c p r | `projectable-refresh` | Refresh the list of files cached 
C-c p f | `projectable-ido-find-file` | Open a file from the cache 

## Planned Features ##
I'm trying to get this project in a nicer state for my own purposes, but this will eventually _(hopefully)_ come with
* `.gitignore` integration for filtering
* Better integration for test running/finding/creation
* Hosting on [Melpa](http://melpa.org/#/)
* Ability to define projects as elisp objects _e.g._
```elisp
(:project "My Project"
  :dirs '("path/one" "path/two") ...)
```
