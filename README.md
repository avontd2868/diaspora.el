# diaspora(dot)el --- Simple Emacs-based client for diaspora*

You can find us: 
https://joindiaspora.com/u/tca
https://joindiaspora.com/u/cnngimenez

*WARNING*

This version may not be compatible with the master branch dir structure and the earlier test version. Sorry about that. Rename the old backup file .diaspora in order to make things work.

This is in a very early stage of development...it will probably break some where. If that is the case: **Contact us!**

# Libraries Needed

* [markdown-mode](http://jblevins.org/projects/markdown-mode/)
* markdown-translator : I haven't submitted into GNA! yet... waiting for authorizations! :P
* [htmlr](https://github.com/emacsmirror/htmlr/tree/)

All the libraries comes with this version of diaspora.el and are in the "libraries" directory! :)


# Installing

Download and unpack diaspora.el, then add this code into your init (.emacs) file:

    (setq load-path (cons "/PATH/TO/DIASPORA/EL/DIRECTORY" load-path))
	(setq load-path (cons "/PATH/TO/DIASPORA/EL/DIRECTORY/libraries" load-path))
	(require 'diaspora)

If you want to use your own libraries versions, then erase the second line.

## Before anything else:

    M-x diaspora
    
This wil create a dir structure and ask for user name and password. Be sure to set the var `diaspora-pod` to your pod, by default is set to joindiaspora.com

## See diaspora stream:

    M-x diaspora-get-entry-stream

Images **aren't** displayed by default.

If you like to always see images at startup customize `diaspora-show-images-by-default` and set it to t.

### To Get All Images

	M-x diaspora-get-all-images
	
### to Remove Images From Buffer 

    M-x diaspora-unshow-images

### To Show Images Again

    M-x diaspora-show-images

### Post To Diaspora:

    M-x diaspora-post-to

### You Also Have 

    M-x diaspora-post-clipboard

For posting from clipboard :)

### Check Keybindings

    C-c C-h


## Learning More

For more explanation or see a list of available commands take a look at the [Wiki](https://github.com/cnngimenez/diaspora.el/wiki).

# Thanks!

* **Tiago Charters de Azevedo** (tca@joindiaspora.com - http://diale.org )for taking part of this! 
* **Phil Hudson** (philhudson@joindiaspora.com) for helping me testing and hunting some bugs!


# License

    README.md
    Copyright (C) 2013  Giménez, Christian N.

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.

    Miércoles 05 De Junio Del 2013


See the [COPYING](./COPYING.txt) file for the full license explanation or [the GNU project web page](http://www.gnu.org).

---

This project still needs some work.

Comments/suggestions are welcome.


*Cheers!*



