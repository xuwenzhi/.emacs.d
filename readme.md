emacs configure files
======================

This is a emacs configure files , contains packages.

* php-mode
* scala-mode2
* solarized
* markdown-mode


Installation
---------------


* install emacs 24.3

If you are using macos, and you installed MacPort, you may install from MacPort

Doing search with 

```
	sudo port search emacs
```

you may find the way to installed it .

if you are using debian, you may install emacs from :


http://emacs.naquadah.org/



* clone this repository and init submodule

```
    git clone https://github.com/ancientrock/.emacs.d.git ~/.emacs.d
    cd ~/.emacs.d && git submodule init && git submodule update
```

Update exists installation
--------------------------

```
    cd ~/.emacs.d && git pull --rebase && git submodule update --init && git submodule sync
```

Mail list
-----------

Users can subscribe to your list by sending email to ar-request@freelists.org with 'subscribe' in the Subject field OR by visiting your list page at http://www.freelists.org/list/ar The web-based administration suite allows you to add subscribers manually.

If you have any question please feel free to join mailist to have a chat.
