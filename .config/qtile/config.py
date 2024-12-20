from libqtile import bar, layout, widget, hook
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
# from libqtile.utils import guess_terminal


# Import additional modules
import importlib
import os
import platform
import subprocess

ctrl  = "control"
alt   = "mod1"
win   = "mod4"
shift = "shift"
enter = "Return"


st_terminal = "st"
kitty_terminal = "/home/rep/.local/kitty.app/bin/kitty"
yazi_filemanager = "/home/rep/.local/kitty.app/bin/kitty yazi"
# rofi = "/home/rep/.config/rofi/launchers/type-1/launcher.sh"
rofi = "rofi -show drun"
shutdown = "/home/rep/.config/rofi/powermenu/type-2/powermenu.sh"
change_bg = "feh --bg-fill -z /home/rep/Pictures/walls"
# change_bg = "bash /home/rep/.local/debinstall/wallpaper.sh select"
# change_bg = "feh --bg-fill -z /home/rep/Pictures/walls"
# change_bg_unsplash = "feh --bg-fill -z /home/rep/Pictures/walls/"




keys = [
    # A list of available commands that can be bound to keys can be found
    # at https://docs.qtile.org/en/latest/manual/config/lazy.html
    # Switch between windows
    # Key([win], "h", lazy.layout.left(), desc="Move focus to left"),
    Key([win], "h", lazy.next_screen(), desc="Next monitor"),
    Key([win], "g", lazy.to_screen(1), desc="Next monitor"),
    Key([win], "a", lazy.to_screen(0), desc="Next monitor"),
    Key([win], "l", lazy.layout.right(), desc="Move focus to right"),
    Key([win], "j", lazy.layout.down(), desc="Move focus down"),
    Key([win], "k", lazy.layout.up(), desc="Move focus up"),
    Key([win], "space", lazy.layout.next(), desc="Move window focus to other window"),
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
    # Key([win], "p", lazy.spawn(change_bg_unsplash), desc="Wallpaper Unsplash"),
    # Key([win], "s", lazy.spawn("sxpape --set"), desc="Select Bg Wallpaper"),
    Key([win], "0", lazy.spawn(shutdown), desc="Shutdown Menu"),
    Key([alt], "f4", lazy.spawn(shutdown), desc="Shutdown Menu"),
    Key([win], enter, lazy.spawn(st_terminal), desc="Launch st terminal"),
    Key([win], "t", lazy.spawn(kitty_terminal), desc="Kitty"),

    # Toggle between different layouts as defined below
    Key([win], "Tab", lazy.next_layout(), desc="Toggle between layouts"),
    Key([win], "w", lazy.window.kill(), desc="Kill focused window"),
    Key([win, ctrl], "r", lazy.reload_config(), desc="Reload the config"),
    Key([win, ctrl], "q", lazy.shutdown(), desc="Shutdown Qtile"),
    Key([win], "r", lazy.spawncmd(), desc="Spawn a command using a prompt widget"),
    Key([win], "b", lazy.hide_show_bar(position="bottom")),
]

groups = [
    Group(
        "",
        layout="max",
        matches=[
            Match(wm_class=["thunderbird-default", "Rofi", "librewolf", "chromium", "brave", "floorp"])
        ],
    ),
    Group(
        "",
        layout="monadtall",
        matches=[Match(wm_class=["virt-manager", "nomacs", "ristretto", "nitrogen"])],
    ),
    Group(
        "󱋊",
        layout="max",
        matches=[Match(wm_class=["qpdfview", "thunar", "nemo", "caja", "pcmanfm"])],
    ),
    Group(
        "󱂬",
        layout="max",
        matches=[
            Match(wm_class=["spotify", "pragha", "clementine", "deadbeef", "audacious"]),
            Match(title=["VLC media player"]),
        ],
    ),
    # Group("󰎞", layout="tile"),
]


for k, group in zip(["y", "u", "i", "o", ], groups):
# for k, group in zip(["1", "2", "3", "4", ], groups):
    keys.extend(
        [
            Key([win], k, lazy.group[group.name].toscreen()),
            Key([win, shift], k, lazy.window.togroup(group.name, switch_group=True)),
        ]
    )



groups.append(
    ScratchPad(
        "scratchpad",
        [
            DropDown(
                "st term",
                "st",
                x=0.,
                y=0.,
                opacity=1,
                width=1.,
                height=0.425,
                on_focus_lost_hide=False,
            ),
            # DropDown(
            #     "yazi_filemanager", yazi_filemanager, x=0.05, y=0.05, width=0.9, height=0.9, opacity=0.99, on_focus_lost_hide=True,
            # ),

            # DropDown(
            #     "pcmanfm", "pcmanfm", x=0.02, y=0.02, width=0.95, height=0.95, opacity=1, on_focus_lost_hide=True,
            # ),
        ],
    )
)

keys.extend(
    [
        # KeyChord([win], "s", [
        #     Key([], 't', lazy.group['scratchpad'].dropdown_toggle('term')),
        #     Key([], 'f', lazy.group['scratchpad'].dropdown_toggle('filemanager')),
        # ]),
        # Key([win], 'g', lazy.group['scratchpad'].dropdown_toggle('pcmanfm')),
        # Key([alt], enter, lazy.group["scratchpad"].dropdown_toggle("yazi_filemanager")),
        Key([ctrl], enter, lazy.group["scratchpad"].dropdown_toggle("st term")),
    ]
)


layouts = [
    layout.Columns(
        border_focus_stack=["#d75f5f", "#8f3d3d"],
        border_width=4,
        margin=[15, 15, 15, 15],
    ),
    layout.Max(),
    # Try more layouts by unleashing below layouts.
    # layout.Stack(num_stacks=2),
    # layout.Bsp(),
    # layout.Matrix(),
    # layout.MonadTall(margin=5),
    # layout.MonadWide(),
    # layout.RatioTile(),
    # layout.Tile(),
    # layout.TreeTab(),
    # layout.VerticalTile(),
    # layout.Zoomy(),
]

widget_defaults = dict(
    font="JetBrainsMono Nerd Font",
    fontsize=12,
    padding=3,
)
extension_defaults = widget_defaults.copy()

screens = [
    Screen(
        bottom=bar.Bar(
            [
                # widget.CurrentLayout(),
                widget.GroupBox(
                    fontsize=13, 
                    margin_x=14,
                    margin_y=4,
                    padding_x=11,
                    padding_y=2,
                    borderwidth=2,
                    rounded=True,
                    spacing=2,
                    highlight_color="#611C35",
                    highlight_method="line",
                    # visible_groups=["", "", "",],
                ),
                # widget.TextBox(" 🔥 ", name="default", foreground="#d75f5f"),
                widget.TextBox("    ", name="default", foreground="#d75f5f"),
                widget.Prompt(),
                widget.WindowName(),
                widget.Chord(
                    chords_colors={
                        "launch": ("#ff0000", "#ffffff"),
                    },
                    name_transform=lambda name: name.upper(),
                ),
                # widget.TextBox("default config", name="default"),
                # widget.TextBox("Press &lt;M-r&gt; to spawn", foreground="#d75f5f"),
                # NB Systray is incompatible with Wayland, consider using StatusNotifier instead
                # widget.StatusNotifier(),
                widget.Systray(),
                widget.Clock(format="%Y-%m-%d, %A [ %H:%M ]  "),
                # widget.QuickExit(),
            ],
            24,
            # border_width=[2, 0, 2, 0],  # Draw top and bottom borders
            # border_color=[
                # "ff00ff",
                # "000000",
                # "ff00ff",
                # "000000",
            # ],  # Borders are magenta
        ),
    ),
    Screen()
]

# Drag floating layouts.
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

# XXX: Gasp! We're lying here. In fact, nobody really uses or cares about this
# string besides java UI toolkits; you can see several discussions on the
# mailing lists, GitHub issues, and other WM documentation that suggest setting
# this string if your java app doesn't work correctly. We may as well just lie
# and say that we're a working one by default.
#
# We choose LG3D to maximize irony: it is a 3D non-reparenting WM written in
# java that happens to be on java's whitelist.
wmname = "LG3D"


# Set wallpaper
@hook.subscribe.startup_once
def autostart():
    wallpaper = os.path.expanduser("~/.config/qtile/scripts/wallpaper.sh")
    # subprocess.call("screenlayout")
    subprocess.call([wallpaper])


# @hook.subscribe.screen_change
# def restart_on_randr(qtile, ev):
#     qtile.cmd_restart()
