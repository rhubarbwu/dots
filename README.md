# Personal Computer Configuration

## Packages

| Package                                                                   | Purpose        |
| ------------------------------------------------------------------------- | -------------- |
| [autorandr](https://archlinux.org/packages/community/any/autorandr/)      | Displays       |
| [rofi](https://archlinux.org/packages/community/x86_64/rofi/)             | Window Manager |
| [xorg-xmodmap](https://archlinux.org/packages/extra/x86_64/xorg-xmodmap/) | Input          |
| [xorg-xrandr](https://archlinux.org/packages/extra/x86_64/xorg-xrandr/)   | Displays       |

### Arch User Repository

| Package                                                              | Purpose  |
| -------------------------------------------------------------------- | -------- |
| [rofi-autorandr](https://aur.archlinux.org/packages/rofi-autorandr/) | Displays |

## Keyboard

Add the following keymappings to `~/.config/X11/Xmodmap` (create it if it doesn't exist) or whatever keymapping file you `xmodmap` on.

- The ThinkPad T470(s)/T480(s) keyboard has `PrtSc` where a "normal" keyboard might have a right "Super" key. If you want to use it as a mod key for your window manager (like XMonad), remap `keycode 107 = Super_L`.

```sh
mkdir -p ~/.config/X11
echo "keycode 107 = Super_L" > ~/.config/X11/Xmodmap
```

## Touchpad

The following will reverse the scrolling direction. Run the following with superuser privileges.

```sh
$ cat 30-touchpad.conf >> /etc/X11/xorg.conf.d/
```
