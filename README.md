## diaspora(dot)el --- Simple Emacs-based client for diaspora*

You can find me: https://joindiaspora.com/u/tca

*WARNING*

This version is not compatible with the master branch dir structure and the earlier test version. Sorry about that. Rename the old backup file .diaspora in order to make things work.

This is in a very early stage of development...it will probably break some where. 



### Before anything else:

    M-x diaspora
    
this wil create a dir structure and ask for user name and password. Be sure to set the var `diaspora-pod` to your pod, by default is set to joindiaspora.com

### See diaspora stream:

    M-x diaspora-get-entry-stream

images are displayed by default

#### to remove images from buffer 

    M-x diaspora-unshow-images

#### to show images  again

    M-x diaspora-show-images

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

## The Unlicense

This is free and unencumbered software released into the public domain.

Anyone is free to copy, modify, publish, use, compile, sell, or
distribute this software, either in source code form or as a compiled
binary, for any purpose, commercial or non-commercial, and by any
means.

In jurisdictions that recognize copyright laws, the author or authors
of this software dedicate any and all copyright interest in the
software to the public domain. We make this dedication for the benefit
of the public at large and to the detriment of our heirs and
successors. We intend this dedication to be an overt act of
relinquishment in perpetuity of all present and future rights to this
software under copyright law.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.

For more information, please refer to <http://unlicense.org/>

## References: 

See also the contribution: https://github.com/cnngimenez/diaspora.el/
