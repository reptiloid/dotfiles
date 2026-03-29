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

from qtile_extras.layout.decorations import ConditionalBorder, GradientBorder, GradientFrame

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
# kitty_terminal = "/home/rep/.local/kitty.app/bin/kitty"
kitty_terminal = "/usr/bin/kitty"
qutebrowser = "/home/rep/.local/bin/qutebrowser"
qtbrowser_launcher = "/home/rep/.local/bin/qtbrowser_launcher"
# rofi = "/home/rep/.config/rofi/launchers/type-1/launcher.sh"
rofi = "rofi -show drun"
shutdown = "/home/rep/.config/rofi/powermenu/type-2/powermenu.sh"
change_bg = "feh --bg-fill -z /home/rep/Pictures/walls"

keys = [
    Key([win], "c", lazy.spawn(rofi), desc="Rofi Launcher"),
    Key([win], "n", lazy.spawn(change_bg), desc="Change Bg Wallpaper"),
    
    Key([win], "0", lazy.spawn(shutdown), desc="Shutdown Menu"),
    Key([alt], "f4", lazy.spawn(shutdown), desc="Shutdown Menu"),
    
    Key([win], enter, lazy.spawn(kitty_terminal), desc="Launch kitty terminal"),
    Key([win], "l", lazy.spawn(betterlockscreen), desc="Lock the Screen"),
    
    Key([alt], "space", lazy.widget["keyboardlayout"].next_keyboard(), desc="Next keyboard layout"),

    Key([win, ctrl], "r", lazy.reload_config(), desc="Reload the config"),
    Key([win, ctrl], "q", lazy.shutdown(), desc="Shutdown Qtile"),
]

keys.extend([
    Key([win], "h", lazy.next_screen(), desc="Next monitor"),
    Key([win], "g", lazy.to_screen(1), desc="Monitor 2"),
    Key([win], "a", lazy.to_screen(0), desc="Monitor 1"),
])

def go_to_group(qtile, index):
    qtile.current_group.use_layout(index)
    
keys.extend([
    Key([win], "Tab", lazy.next_layout(), desc="Toggle between layouts"),
    Key([win], "f", lazy.function(go_to_group, 0), desc="Set Max Layout"),
    Key([win], "m", lazy.function(go_to_group, 1), desc="Set MonadTall Layout"),
    Key([win], "t", lazy.function(go_to_group, 2), desc="Set Tile Layout"),
])

keys.extend(
    [
        Key([win, shift], "w", lazy.window.kill(), desc="Kill focused window"),
        
        Key([win], "space", lazy.layout.next(), desc="Move window focus to next window"),
        Key([win], "j", lazy.layout.down(), desc="Move focus down"),
        Key([win], "k", lazy.layout.up(), desc="Move focus up"),
        
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
    ]
)

keys.append(
    KeyChord([win],"e", [
        Key([], "e", lazy.spawn(myEmacs), desc='Emacs Dashboard'),
        Key([], "d", lazy.spawn(myEmacs + "--eval '(dired nil)'"), desc='Emacs Dired'),
        Key([], "k", lazy.spawn("killall emacs"),
                     lazy.spawn("/usr/local/bin/emacs --daemon"),
                     desc='Kill/restart the Emacs daemon')
    ])

)

keys.append(
    KeyChord([win], "b", [
        # Key([], "q", lazy.spawn(qutebrowser)),
        # Key([], "s", lazy.spawn(qtbrowser_launcher)),
        Key([], "f", lazy.spawn("floorp")),
        Key([], "b", lazy.spawn("brave")),
        Key([], "o", lazy.spawn("buku_run"), desc='Bookmarks'),
    ])
)

keys.append(
    KeyChord([win], "p", [
        Key([], "h", lazy.spawn("dm-hub -r"), desc='List all dmscripts'),
        Key([], "a", lazy.spawn("dm-sounds -r"), desc='Choose ambient sound'),
        Key([], "b", lazy.spawn("dm-setbg -r"), desc='Set background'),
        Key([], "c", lazy.spawn("dtos-colorscheme -r"), desc='Choose color scheme'),
        Key([], "e", lazy.spawn("rep-configs"), desc='Choose a config file to edit'),
        Key([], "i", lazy.spawn("dm-maim -r"), desc='Take a screenshot'),
        Key([], "k", lazy.spawn("dm-kill -r"), desc='Kill processes'),
        Key([], "m", lazy.spawn("dm-man -r"), desc='View manpages'),
        Key([], "n", lazy.spawn("dm-documents -r"), desc='Store and copy notes'),
        Key([], "o", lazy.spawn("buku_run"), desc='Browser bookmarks'),
        Key([], "p", lazy.hide_show_bar(position="bottom")),
        Key([], "q", lazy.spawn("dm-logout -r"), desc='Logout menu'),
        Key([], "r", lazy.spawn("dm-radio -r"), desc='Listen to online radio'),
        Key([], "s", lazy.spawn("dm-websearch -r"), desc='Search various engines'),
        Key([], "t", lazy.spawn("dm-translate -r"), desc='Translate text'),
        Key([], "u", lazy.spawn("dm-music -r"), desc='Toggle music mpc/mpd')
    ])
)

groups = [
    Group(
        "",
        layout="monadtall",
        matches=[
            Match(wm_class=["thunderbird-default", "chromium", "brave", "floorp", "steam_app_9420"])
        ],
        label="",
    ),
    Group(
        "",
        layout="tile",
        matches=[Match(wm_class=["virt-manager", "emacs", "obsidian", "pcmanfm"]),
                 Match(title="FAF ICE adapter - Debugger - Build: SNAPSHOT")],
        label="",
    ),
    Group(
        "",
        layout="tile",
        matches=[
            Match(wm_class=["qpdfview", "thunar", "Happ", "caja"]),
            Match(title="FAF ICE adapter"),
        ],
        label="",
    ),
    Group(
        "󱂬",
        layout="max",
        matches=[
            Match(wm_class=["spotify", "pragha", "clementine", "deadbeef", "audacious"]),
            # Match(title=["VLC media player"]),
        ],
        label="",
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

orange1 = "E85E00"
orange2 = "F34213"
yellow1 = "E0CA3C"
yellow2 = "EFEA5A"
blue1   = "4A874A"
blue2   = "1974E0"
green1  = "19E659"
green2  = "3FDEC9"
red1    = "E80000"
red2    = "E61919"
red3    = "8F3D3D"
red4    = "F45C4E"
grey1   = "234111"

color_set1 = [orange1, yellow1, blue1, orange2, blue2]
color_set2 = [green1, yellow2, red1, blue1, green2, orange1]
color_set3 = [red2, blue2, green1]
color_set4 = [grey1, red2, yellow1, blue2]

layouts = [
    # layout.Columns(
    #     border_focus_stack=["#D75F5F", "#8F3D3D"],
    #     border_width=4,
    #     margin=[15, 15, 15, 15],
    # ),
    layout.Max(),
    layout.MonadTall(
        border_focus=ConditionalBorder(matches=[
            (Match(wm_class="emacs"), GradientBorder(colours=color_set1)),
            (Match(wm_class="qutebrowser"), GradientFrame(colours=["00f", "0ff"])),
            (Match(wm_class="kitty"), GradientBorder(colours=color_set2)),
            (Match(wm_class="floorp"), GradientBorder(colours=color_set3)),
            (Match(wm_class="thunderbird-default"), GradientBorder(colours=color_set4)),
        ], fallback=red4),
        border_width=4,
        margin=13,
    ),
    # layout.Stack(num_stacks=2),
    # layout.Bsp(),
    # layout.Matrix(),
    # layout.MonadWide(),
    # layout.RatioTile(),
    layout.Tile(
        border_focus=GradientBorder(colours=color_set3),
        border_width=2,
        # margin=6,
    ),
    # layout.TreeTab(),
    # layout.VerticalTile(),
    # layout.Zoomy(),
]

widget_defaults = dict(
    font="JetBrainsMono Nerd Font",
    fontsize=13,
    padding=2,
)
extension_defaults = widget_defaults.copy()


screens = [
    Screen(
        bottom=bar.Bar(
            [
                widget.GroupBox(
                    active="#59414a",
                    fontsize=10,
                    margin_x=11,
                    padding_x=11,
                    spacing=2,
                    # highlight_color="#773d54",
                    highlight_method="text",
                    hide_unused=True,
                ),
                widget.WindowName(),
                # widget.Chord(),
                widget.Systray(),
                widget.KeyboardLayout(configured_keyboards=['us', 'ru'], fmt="[ {} ]", padding=11),
                # widget.CPUGraph(),
                # widget.NetGraph(graph_color="#e23456"),
                widget.Clock(format="%Y-%m-%d, %A [ %H:%M ]", padding=11),
            ],
            size=22,
            # margin=[4,6,4,6],
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
    # no_reposition_rules=[
    #     Match(wm_class="com.faforever.iceadapter.debug.DebugWindow"),
    #     # Match(title="FAF ICE adapter - Debugger - Build: SNAPSHOT"),
    # ],
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

# @subscribe.client_new 
# def new_clinet(client):
#     if "com.faforever.iceadapter.debug.DebugWindow" in client.get_wm_class():
#         client.set_position_floating(200,200)


@hook.subscribe.startup_once
def autostart():
    script = os.path.expanduser("~/.config/qtile/scripts/autostart.sh")
    subprocess.call([script])
