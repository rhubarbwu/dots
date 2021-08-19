#!/bin/sh

git clone "https://github.com/xmonad/xmonad" xmonad-git
git clone "https://github.com/xmonad/xmonad-contrib" xmonad-contrib-git
git clone "https://github.com/jaor/xmobar" xmobar-git
chmod a+x build
xmonad --recompile # && xmonad --restart
stack install

# reference: https://brianbuccola.com/how-to-install-xmonad-and-xmobar-via-stack/
