# Design 

I will try to implement a new design maked by levels. 

For now I will concentrate to implement the two lowers levels, but I have identified three:

* Connection or streamming
* Medium or control level 
* Interface

Each level has to have a common API no matter what kind of library we're using. 

<br/>

For instance:

If we use HTTP for connecting D* the API should be the same as if we use cliaspora client as a backend.

Both we should have a `connect` function, a `get-stream`, etc.


<br/>

*Remember:*

Each level should use the inmediate lower level API and structures(if needed).

## Connection Level
This is the "lower level".

This level will have everything needed for connecting into D*. You can use HTTP, cliaspora or other options. The user or the developer can select any of these backends.

## Control Level
This level should implement how D*.el should behave and use every data. If we should show them, process them, inhibit some post or not, etc.

Also should select which elements should be loaded.

## Interface
This is the "upper level".

The visible part of D* should be here. Major and minnor modes, keys, etc.

## Common Libraries 
There may be common libraries, these are needed for defining objects or structures for communicating between levels. This may be necessary, if not each level has to implement the structure necessary for use as interface for the upper level.


# License

    Design.md
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


