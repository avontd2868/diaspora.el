## diaspora(dot)el --- Simple Emacs-based client for diaspora*

You can find us: 
https://joindiaspora.com/u/tca
https://joindiaspora.com/u/cnngimenez

*WARNING*

This version may not be compatible with the master branch dir structure and the earlier test version. Sorry about that. Rename the old backup file .diaspora in order to make things work.

This is in a very early stage of development...it will probably break some where. If that is the case: **Contact us!**

## Libraries Needed

* [markdown-mode](http://jblevins.org/projects/markdown-mode/)
* markdown-translator : I haven't submitted into GNA! yet... waiting for authorizations! :P
* [htmlr](https://github.com/emacsmirror/htmlr/tree/)

The library markdown-translator comes with this version of diaspora.el! :)


### Before anything else:

    M-x diaspora
    
this wil create a dir structure and ask for user name and password. Be sure to set the var `diaspora-pod` to your pod, by default is set to joindiaspora.com

### See diaspora stream:

    M-x diaspora-get-entry-stream

Images **aren't** displayed by default.

If you like to always see images at startup customize `diaspora-show-images-by-default` and set it to t.

#### To Get All Images

	M-x diaspora-get-all-images
	
#### to Remove Images From Buffer 

    M-x diaspora-unshow-images

#### To Show Images Again

    M-x diaspora-show-images

### Post To Diaspora:

    M-x diaspora-post-to

#### You Also Have 

    M-x diaspora-post-clipboard

For posting from clipboard :)

#### Check Keybindings

    C-c C-h


#### Learning More

For more explanation or see a list of available commands take a look at the [Wiki](https://github.com/cnngimenez/diaspora.el/wiki).

## Thanks!

* **Tiago Charters de Azevedo** (tca@joindiaspora.com - http://diale.org )for taking part of this! 

* **Phil Hudson** (philhudson@joindiaspora.com) for helping me testing and hunting some bugs!


---

This project still needs some work.

Comments/suggestions are welcome.


*Cheers!*



