# `deku` `dyn` test

This project created to test `deku`  `dyn` feature as I ran into some issues with it.

There are 4 `Main` modules only one of them can active at one moment.

## Purpose
I wanted to create a table from an Array of data. Every row of a table has both edit and remove buttons. Table has an add button to add a new item.

Edit button changes the row to edit mode (not implemented) but at the same time disables all buttons except the editing row.

Remove removes the item. This causes problems such that removes the *next* (more accuretly all following items) item instead of the current one.

Add button adds a new item in the top position which seems fine but if the cancel button clicked the item should be removed. This does not work also.

## `Main` modules

You will probably care only `Main-updating.purs-x` file because this is the original idea. Other `*-updating-*` files try to implement same idea differently. `Main-array-render.purs` is the current imlementation in the application at the moment.

## Need of hints

As I am a self learner and relatively new to functional programming and Purescript please inform me other efficient and idimatic ways to implement same ideas if you don't mind.

Also I think I don't understand Alt.do usage in order to use addition `Event`s in `dyn` block. Pointers are welcome.

