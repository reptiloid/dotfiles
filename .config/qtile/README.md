
# Table of Contents

1.  [Config](#org552cb82)
    1.  [imports](#org0b6902f)
    2.  [keys vars](#org9344bfb)
    3.  [cmds vars](#orge46c975)
    4.  [keys](#org7032754)
    5.  [groups](#orgf8b816b)
        1.  [groups keys](#org78cda5c)
    6.  [scratchpad](#org54c3a51)
    7.  [layouts](#orge20f356)
    8.  [screens](#org6bdb409)
    9.  [mouse](#orgaad14e2)
    10. [options](#org1b1c651)
        1.  [floating](#orgaa66027)
        2.  [more options](#orgf3f525c)
    11. [autostart](#orgddb3e37)
        1.  [set wallpaper](#orgd0e0521)
        2.  [restart on randr screen change](#orgc869bd0)



<a id="org552cb82"></a>

# Config


<a id="org0b6902f"></a>

## imports


<a id="org9344bfb"></a>

## keys vars

    ctrl  = "control"
    alt   = "mod1"
    win   = "mod4"
    shift = "shift"
    enter = "Return"


<a id="orge46c975"></a>

## cmds vars

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


<a id="org7032754"></a>

## keys

A list of available commands that can be bound to keys can be found
at <https://docs.qtile.org/en/latest/manual/config/lazy.html>

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">key</th>
<th scope="col" class="org-left">action</th>
</tr>
</thead>
<tbody>
<tr>
<td class="org-left">win-h</td>
<td class="org-left">next monitor</td>
</tr>

<tr>
<td class="org-left">win-a</td>
<td class="org-left">to screen 1</td>
</tr>

<tr>
<td class="org-left">win-g</td>
<td class="org-left">to screen 2</td>
</tr>

<tr>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
</tr>
</tbody>
</table>


<a id="orgf8b816b"></a>

## groups

    
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


<a id="org78cda5c"></a>

### groups keys

    
    for k, group in zip(["y", "u", "i", "o", ], groups):
        keys.extend(
            [
                Key([win], k, lazy.group[group.name].toscreen()),
                Key([win, shift], k, lazy.window.togroup(group.name, switch_group=True)),
            ]
        )


<a id="org54c3a51"></a>

## scratchpad

    
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
            Key([win], "s", lazy.group["scratchpad"].dropdown_toggle("st term")),
        ]
    )


<a id="orge20f356"></a>

## layouts

Try more layouts by unleashing below layouts.

    layouts = [
        layout.Columns(
            border_focus_stack=["#d75f5f", "#8f3d3d"],
            border_width=4,
            margin=[15, 15, 15, 15],
        ),
        layout.Max(),
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


<a id="org6bdb409"></a>

## screens

    
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


<a id="orgaad14e2"></a>

## mouse

Drag floating layouts.

    
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


<a id="org1b1c651"></a>

## options

    
    dgroups_key_binder = None
    dgroups_app_rules = []  # type: list
    follow_mouse_focus = True
    bring_front_click = False
    cursor_warp = False


<a id="orgaa66027"></a>

### floating

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


<a id="orgf3f525c"></a>

### more options

    auto_fullscreen = True
    focus_on_window_activation = "smart"
    reconfigure_screens = True
    
    # If things like steam games want to auto-minimize themselves when losing
    # focus, should we respect this or not?
    auto_minimize = True
    
    # When using the Wayland backend, this can be used to configure input devices.
    wl_input_rules = None
    
    wmname = "LG3D"


<a id="orgddb3e37"></a>

## autostart


<a id="orgd0e0521"></a>

### set wallpaper

    @hook.subscribe.startup_once
    def autostart():
        wallpaper = os.path.expanduser("~/.config/qtile/scripts/wallpaper.sh")
        subprocess.call([wallpaper])


<a id="orgc869bd0"></a>

### restart on randr screen change

    # @hook.subscribe.screen_change
    # def restart_on_randr(qtile, ev):
    #     qtile.cmd_restart()

