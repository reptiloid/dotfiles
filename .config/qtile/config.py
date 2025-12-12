from libqtile import bar, layout, widget, hook
# from qtile_extras import widget
# from qtile_extras.widget.decorations import RectDecoration
# from qtile_extras.widget.decorations import PowerLineDecoration

from libqtile.config import (
    Click,
    Drag,
    Group,
    Key,
    KeyChord,
    Match,
    Screen,
    ScratchPad,
    DropDown,
)
from libqtile.lazy import lazy

from qtile_extras.layout.decorations import ConditionalBorder, GradientBorder

import importlib
import os
import platform
import subprocess

ctrl  = "control"
alt   = "mod1"
win   = "mod4"
shift = "shift"
enter = "Return"

betterlockscreen = "betterlockscreen -l"
st_terminal = "st"
myEmacs = "emacsclient -c -a 'emacs' " # The space at the end is IMPORTANT!
kitty_terminal = "/usr/bin/kitty"
# kitty_terminal = "/usr/bin/kitty fish"
qutebrowser = "/home/rep/.local/bin/qutebrowser"
qtbrowser_launcher = "/home/rep/.local/bin/qtbrowser_launcher"
# rofi = "/home/rep/.config/rofi/launchers/type-1/launcher.sh"
rofi = "rofi -show drun"
shutdown = "/home/rep/.config/rofi/powermenu/type-2/powermenu.sh"
change_bg = "feh --bg-fill -z /home/rep/Pictures/walls"
wallpaper_select = "bash /home/rep/.local/debinstall/wallpaper.sh select"

def go_to_group(qtile, index):
    qtile.current_group.use_layout(index)

keys = [
    # Switch between windows
    # Key([win], "h", lazy.layout.left(), desc="Move focus to left"),
    Key([win], "h", lazy.next_screen(), desc="Next monitor"),
    Key([win], "g", lazy.to_screen(1), desc="Next monitor"),
    Key([win], "a", lazy.to_screen(0), desc="Next monitor"),
    # Key([win], "l", lazy.layout.right(), desc="Move focus to right"),
    Key([win], "j", lazy.layout.down(), desc="Move focus down"),
    Key([win], "k", lazy.layout.up(), desc="Move focus up"),
    Key([win], "space", lazy.layout.next(), desc="Move window focus to other window"),
    # Key([alt], "Tab", lazy.layout.next(), desc="Move window focus to other window"),

    # Move windows between left/right columns or move up/down in current stack.
    # Moving out of range in Columns layout will create new column.
    Key([win, shift], "h", lazy.layout.shuffle_left(), desc="Move window to the left"),
    Key([win, shift], "l", lazy.layout.shuffle_right(), desc="Move window to the right"),
    Key([win, shift], "j", lazy.layout.shuffle_down(), desc="Move window down"),
    Key([win, shift], "k", lazy.layout.shuffle_up(), desc="Move window up"),

    # Grow windows. If current window is on the edge of screen and direction
    # will be to screen edge - window would shrink.
    Key([win, ctrl], "h", lazy.layout.grow_left(), desc="Grow window to the left"),
    Key([win, ctrl], "l", lazy.layout.grow_right(), desc="Grow window to the right"),
    Key([win, ctrl], "j", lazy.layout.grow_down(), desc="Grow window down"),
    Key([win, ctrl], "k", lazy.layout.grow_up(), desc="Grow window up"),
    # Key([win], "n", lazy.layout.normalize(), desc="Reset all window sizes"),

    # Toggle between split and unsplit sides of stack.
    # Split = all windows displayed
    # Unsplit = 1 window displayed, like Max layout, but still with
    # multiple stack panes
    Key([win, shift], enter, lazy.layout.toggle_split(), desc="Toggle between split and unsplit sides of stack",),
    Key([win], "c", lazy.spawn(rofi), desc="Rofi Launcher"),
    # Key([ctrl], "~", lazy.spawn(rofi), desc="Rofi Launcher"),
    Key([win], "n", lazy.spawn(change_bg), desc="Change Bg Wallpaper"),
    Key([win, shift], "n", lazy.spawn(wallpaper_select), desc="Change Bg Wallpaper"),
    # Key([win], "p", lazy.spawn(change_bg_unsplash), desc="Wallpaper Unsplash"),
    # Key([win], "s", lazy.spawn("sxpape --set"), desc="Select Bg Wallpaper"),
    Key([win], "0", lazy.spawn(shutdown), desc="Shutdown Menu"),
    Key([alt], "f4", lazy.spawn(shutdown), desc="Shutdown Menu"),
    # Key([win], enter, lazy.spawn(st_terminal), desc="Launch st terminal"),
    Key([win], enter, lazy.spawn(kitty_terminal), desc="Launch kitty terminal"),
    Key([alt], "space", lazy.widget["keyboardlayout"].next_keyboard(), desc="Next keyboard layout"),
    # Key([win], "е", lazy.widget["keyboardlayout"].next_keyboard(), desc="Next keyboard layout"),
    Key([win], "l", lazy.spawn(betterlockscreen), desc="Lock the Screen"),

    # Toggle between different layouts as defined below
    Key([win], "Tab", lazy.next_layout(), desc="Toggle between layouts"),
    Key([win], "w", lazy.window.kill(), desc="Kill focused window"),
    Key([win, ctrl], "r", lazy.reload_config(), desc="Reload the config"),
    Key([win, ctrl], "q", lazy.shutdown(), desc="Shutdown Qtile"),
    Key([win], "r", lazy.spawncmd(), desc="Spawn a command using a prompt widget"),
    Key([win], "p", lazy.hide_show_bar(position="bottom")),
    Key([win], "f", lazy.function(go_to_group, 0)),
    Key([win], "m", lazy.function(go_to_group, 1)),
    Key([win], "t", lazy.function(go_to_group, 2)),
    
    # Emacs programs launched using the key chord SUPER+e followed by 'key'
    KeyChord([win],"e", [
        Key([], "e", lazy.spawn(myEmacs), desc='Emacs Dashboard'),
        Key([], "d", lazy.spawn(myEmacs + "--eval '(dired nil)'"), desc='Emacs Dired'),
        Key([], "s", lazy.spawn(myEmacs + "--eval '(eshell)'"), desc='Emacs Eshell'),
        Key([], "v", lazy.spawn(myEmacs + "--eval '(vterm)'"), desc='Emacs Vterm'),
        Key([], "k", lazy.spawn("killall emacs"),
                      lazy.spawn("/usr/local/bin/emacs --daemon"),
                      desc='Kill/restart the Emacs daemon')
    ]),

    # Qt Browser
    KeyChord([win], "b", [
        Key([], "q", lazy.spawn(qutebrowser)),
        Key([], "s", lazy.spawn(qtbrowser_launcher)),
        Key([], "f", lazy.spawn("floorp")),
        Key([], "b", lazy.spawn("brave")),
    ])
]

groups = [
    Group(
        "",
        layout="max",
        matches=[
            Match(wm_class=["thunderbird-default", "rofi", "librewolf", "chromium", "brave", "floorp"])
        ],
    ),
    Group(
        "",
        layout="monadtall",
        matches=[Match(wm_class=["virt-manager", "emacs", "obsidian", "pcmanfm"]),
                 Match(title="FAF ICE adapter - Debugger - Build: SNAPSHOT")],
    ),
    Group(
        "",
        layout="max",
        matches=[Match(wm_class=["qpdfview", "thunar", "nemo", "caja"])],
    ),
    Group(
        "󱂬",
        layout="max",
        matches=[
            Match(wm_class=["spotify", "pragha", "clementine", "deadbeef", "audacious"]),
            # Match(title=["VLC media player"]),
        ],
    ),
    # Group("󰎞", layout="tile"),
]

for k, group in zip(["y", "u", "i", "o", ], groups):
    keys.extend(
        [
            Key([win], k, lazy.group[group.name].toscreen()),
            Key([win, shift], k, lazy.window.togroup(group.name, switch_group=False)),
        ]
    )

groups.append(
    ScratchPad(
        "scratchpad",
        [
            DropDown(
                "st term",
                "st",
                x=0.005,
                y=0.,
                opacity=.88,
                width=.99,
                height=0.475,
                on_focus_lost_hide=False,
            ),
        ],
    )
)

keys.extend(
    [
        Key([win], "s", lazy.group["scratchpad"].dropdown_toggle("st term")),
    ]
)

layouts = [
    # layout.Columns(
    #     border_focus_stack=["#d75f5f", "#8f3d3d"],
    #     border_width=4,
    #     margin=[15, 15, 15, 15],
    # ),
    layout.Max(),
    layout.MonadTall(
        border_focus=ConditionalBorder(
                matches=[
                        (Match(wm_class="emacs"), GradientBorder(colours=["e85e00", "E0CA3C", "4a874a", "F34213"])),
                        (Match(wm_class="qutebrowser"), GradientBorder(colours=["e85e00", "EFEA5A", "4a874a"])),
                        (Match(wm_class="kitty"), GradientBorder(colours=["8E78BA", "e80000", "F1E7A7", "3FDEC9"])),
                        # (Match(wm_class="firefox"), "f0f"),
                ],
                fallback="8f3d3d"),
                border_width=5,
                margin=15,
    ),
    # layout.Stack(num_stacks=2),
    # layout.Bsp(),
    # layout.Matrix(),
    # layout.MonadTall(margin=5),
    # layout.MonadWide(),
    # layout.RatioTile(),
    layout.Tile(
        border_focus="#8f3d3d",
        border_width=2,
    ),
    # layout.TreeTab(),
    # layout.VerticalTile(),
    # layout.Zoomy(),
]

widget_defaults = dict(
    font="JetBrainsMono Nerd Font",
    fontsize=11,
    padding=3,
)
extension_defaults = widget_defaults.copy()


screens = [
    Screen(
        bottom=bar.Bar(
            [
                widget.GroupBox(
                    fontsize=13,
                    margin_x=14,
                    padding_x=11,
                    rounded=True,
                    spacing=2,
                    highlight_color="#611C35",
                    highlight_method="line",
                ),
                widget.WindowName(),
                widget.Chord(),
                widget.Systray(),
                widget.KeyboardLayout(configured_keyboards=['us', 'ru'], fmt="[ {} ]", padding=11),
                widget.CPUGraph(),
                widget.NetGraph(graph_color="#e23456"),
                widget.Clock(format="%Y-%m-%d, %A [ %H:%M ]", padding=11),
            ],
            size=20,
            # margin=[8,12,8,12],
        ),
    ),
    Screen()
]

mouse = [
    Drag(
        [win],
        "Button1",
        lazy.window.set_position_floating(),
        start=lazy.window.get_position(),
    ),
    Drag(
        [win], "Button3", lazy.window.set_size_floating(), start=lazy.window.get_size()
    ),
    Click([win], "Button2", lazy.window.bring_to_front()),
]

dgroups_key_binder = None
dgroups_app_rules = []  # type: list
follow_mouse_focus = True
bring_front_click = False
cursor_warp = False

floating_layout = layout.Floating(
    border_focus="#A6A867",
    border_normal="#262729",
    border_width=2,
    float_rules=[
        # Run the utility of `xprop` to see the wm class and name of an X client.
        *layout.Floating.default_float_rules,
        Match(wm_class="confirmreset"),  # gitk
        Match(wm_class="makebranch"),  # gitk
        Match(wm_class="maketag"),  # gitk
        Match(wm_class="ssh-askpass"),  # ssh-askpass
        Match(wm_class="org.cryptomator.launcher.Cryptomator$MainApp"),
        Match(title="branchdialog"),  # gitk
        Match(title="pinentry"),  # GPG key password entry
        Match(title="FAF ICE adapter - Debugger - Build: SNAPSHOT"), # faf
    ],
)

auto_fullscreen = True
focus_on_window_activation = "smart"
reconfigure_screens = True

# If things like steam games want to auto-minimize themselves when losing
# focus, should we respect this or not?
auto_minimize = True

# When using the Wayland backend, this can be used to configure input devices.
wl_input_rules = None

wmname = "LG3D"

@hook.subscribe.startup_once
def autostart():
    script = os.path.expanduser("~/.config/qtile/scripts/autostart.sh")
    subprocess.call([script])
