## diaspora(dot)el --- Simple Emacs-based client for diaspora*

You can find me: https://joindiaspora.com/u/tca

*WARNING*

This version is not compatible with the master branch dir structure and the earlier test version. Sorry about that. Rename the old backup file .diaspora in order to make things work.

This is in a very early stage of development...it will probably break some where. 

### Before anything else:

M-x diaspora

this wil create a dir structure and ask for user name and password. Be sure to set the var `diaspora-pod' to your pod, by default is set to joindiaspora.com

### See diaspora stream:

M-x diaspora-get-entry-stream

images are displayed 

to remove images from buffer 

M-x diaspora-unshow-images

to show them again

M-x diaspora-unshow-images

### Post to diaspora:

M-x diaspora-post-to

#### You also have 

M-x diaspora-post-clipboard

for posting from clipboard :)

#### Check keybindings

C-c C-h

---

... It still needs some work.

Comments/suggestions are welcome.

Cheers.


