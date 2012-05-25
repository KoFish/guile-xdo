#include <libguile.h>
#include <xdo.h>

#include "xdo_guile_smobs.h"

#define make_pair(x, y) scm_cons(x, scm_cons(y, SCM_EOL))
#define assert_xdo_window(w) { if (!is_xdo_window(w)) (scm_throw(scm_from_locale_symbol("xdo-window-error"), scm_from_locale_string("not a window"))); }

void xdo_free_pointer(void *xdo)
{
    xdo_free((xdo_t *)xdo);
}

SCM
xdo_new_wrapper(SCM disp)
{
    if (disp == SCM_UNDEFINED)
    {
        char *display = getenv("DISPLAY");
        return scm_from_pointer(xdo_new(display), xdo_free_pointer);
    }
    else
        return scm_from_pointer(xdo_new(scm_to_locale_string(disp)), xdo_free_pointer);
}

SCM
xdo_version_wrapper()
{
    return scm_from_locale_string(xdo_version());
}

SCM
xdo_mousemove_wrapper(SCM xdo, SCM x, SCM y, SCM screen)
{
    return scm_from_int(xdo_mousemove(SCM_POINTER_VALUE(xdo),
                                      scm_to_int(x),
                                      scm_to_int(y),
                                      scm_to_int(screen)));
}

SCM
xdo_mousemove_rel_to_win_wrapper(SCM xdo, SCM window, SCM x, SCM y)
{
    assert_xdo_window(window);
    return scm_from_int(xdo_mousemove_relative_to_window(SCM_POINTER_VALUE(xdo),
                        scm_to_ulong(window),
                        scm_to_int(x),
                        scm_to_int(y)));
}

SCM
xdo_mousemove_rel_wrapper(SCM xdo, SCM x, SCM y)
{
    return scm_from_int(xdo_mousemove_relative(SCM_POINTER_VALUE(xdo),
                        scm_to_int(x),
                        scm_to_int(y)));
}

SCM
xdo_mousedown_wrapper(SCM xdo, SCM window, SCM button)
{
    assert_xdo_window(window);
    return scm_from_int(xdo_mousedown(SCM_POINTER_VALUE(xdo),
                                      scm_to_ulong(window),
                                      scm_to_int(button)));
}

SCM
xdo_mouseup_wrapper(SCM xdo, SCM window, SCM button)
{
    assert_xdo_window(window);
    return scm_from_int(xdo_mouseup(SCM_POINTER_VALUE(xdo),
                                    scm_to_ulong(window),
                                    scm_to_int(button)));
}

SCM
xdo_mouselocation_wrapper(SCM xdo, SCM with_window)
{
    int x, y, screen;
    Window window = -1;
    if ((with_window != SCM_UNDEFINED) && (scm_is_true(with_window)))
    {
        xdo_mouselocation2(SCM_POINTER_VALUE(xdo), &x, &y, &screen, &window);
    }
    else
    {
        xdo_mouselocation(SCM_POINTER_VALUE(xdo), &x, &y, &screen);
    }
    return make_mouselocation(x, y, screen, window);
}

SCM
xdo_mouse_wait_for_move_wrapper(SCM xdo, SCM x, SCM y, SCM to)
{
    if ((to != SCM_UNDEFINED) && (scm_is_true(to)))
    {
        return scm_from_int(xdo_mouse_wait_for_move_to(SCM_POINTER_VALUE(xdo), scm_to_int(x), scm_to_int(y)));
    }
    else
    {
        return scm_from_int(xdo_mouse_wait_for_move_from(SCM_POINTER_VALUE(xdo), scm_to_int(x), scm_to_int(y)));
    }
}

SCM
xdo_click_wrapper(SCM xdo, SCM window, SCM button, SCM repeat, SCM delay)
{
    assert_xdo_window(window);
    if (repeat != SCM_UNDEFINED)
    {
        useconds_t d;
        if (delay != SCM_UNDEFINED) d = scm_to_uint32(delay);
        else d = 1000;
        return scm_from_int(xdo_click_multiple(SCM_POINTER_VALUE(xdo),
                                               SCM_SMOB_DATA(window),
                                               scm_to_int(button),
                                               scm_to_int(repeat),
                                               d));
    }
    else
    {
        return scm_from_int(xdo_click(SCM_POINTER_VALUE(xdo),
                                      SCM_SMOB_DATA(window),
                                      scm_to_int(button)));
    }
}

SCM
xdo_type_wrapper(SCM xdo, SCM window, SCM string, SCM delay)
{
    assert_xdo_window(window);
    return scm_from_int(xdo_type(SCM_POINTER_VALUE(xdo),
                                 SCM_SMOB_DATA(window),
                                 scm_to_locale_string(string),
                                 (delay == SCM_UNDEFINED ?  1000 : scm_to_uint32(delay))));
}

SCM
xdo_keysequence_wrapper(SCM xdo, SCM window, SCM keysequence, SCM delay, SCM mode)
{
    useconds_t d = 1000;
    assert_xdo_window(window);
    if (delay != SCM_UNDEFINED)
    {
        d = scm_to_uint32(delay);
    }
    if (mode != SCM_UNDEFINED)
    {
        SCM mstr = scm_symbol_to_string(mode);
        if (mstr != SCM_UNDEFINED && scm_is_true(scm_string_locale_ci_eq(mstr, scm_from_locale_string("up"), SCM_UNDEFINED)))
        {
            return scm_from_int(xdo_keysequence_up(SCM_POINTER_VALUE(xdo),
                                                   SCM_SMOB_DATA(window),
                                                   scm_to_locale_string(keysequence),
                                                   d));
        }
        else if (mstr != SCM_UNDEFINED && scm_is_true(scm_string_locale_ci_eq(mstr, scm_from_locale_string("down"), SCM_UNDEFINED)))
        {
            return scm_from_int(xdo_keysequence_down(SCM_POINTER_VALUE(xdo),
                                SCM_SMOB_DATA(window),
                                scm_to_locale_string(keysequence),
                                d));
        }
    }
    return scm_from_int(xdo_keysequence(SCM_POINTER_VALUE(xdo),
                                        SCM_SMOB_DATA(window),
                                        scm_to_locale_string(keysequence),
                                        d));
}

SCM
xdo_active_keys_to_keycode_list_wrapper(SCM xdo)
{
    charcodemap_t *keys;
    int i,nkeys;
    SCM lst = SCM_EOL;
    xdo_active_keys_to_keycode_list(SCM_POINTER_VALUE(xdo), &keys, &nkeys);
    for (i = 0 ; i < nkeys ; i++)
    {
        lst = scm_cons(make_charcodemap(keys[i]), lst);
    }
    free(keys);
    return lst;
}

SCM
xdo_window_move_wrapper(SCM xdo, SCM window, SCM x, SCM y)
{
    assert_xdo_window(window);
    return scm_from_int(xdo_window_move(SCM_POINTER_VALUE(xdo),
                                        SCM_SMOB_DATA(window),
                                        scm_to_int(x),
                                        scm_to_int(y)));
}

SCM
xdo_window_translate_with_sizehint_wrapper(SCM xdo, SCM window, SCM width, SCM height)
{
    int new_width, new_height, ret;
    assert_xdo_window(window);
    ret = xdo_window_translate_with_sizehint(SCM_POINTER_VALUE(xdo),
            SCM_SMOB_DATA(window),
            scm_to_int(width),
            scm_to_int(height),
            &new_width, &new_height);
    if (ret)
    {
        return scm_cons(scm_from_int(new_width), scm_cons(scm_from_int(new_height), SCM_EOL));
    }
    else
    {
        return scm_from_int(ret);
    }
}

SCM
xdo_window_setsize_wrapper(SCM xdo, SCM window, SCM width, SCM height, SCM flags)
{
    assert_xdo_window(window);
    return scm_from_int(xdo_window_setsize(SCM_POINTER_VALUE(xdo),
                                           SCM_SMOB_DATA(window),
                                           scm_to_int(width),
                                           scm_to_int(height),
                                           scm_to_int(flags)));
}

SCM
xdo_window_setprop_wrapper (SCM xdo, SCM wid, SCM property, SCM value)
{
    assert_xdo_window(wid);
    return scm_from_int(xdo_window_setprop(SCM_POINTER_VALUE(xdo),
                                           SCM_SMOB_DATA(wid),
                                           scm_to_locale_string(property),
                                           scm_to_locale_string(value)));
}

SCM
xdo_window_setclass_wrapper (SCM xdo, SCM wid, SCM name, SCM class)
{
    assert_xdo_window(wid);
    return scm_from_int(xdo_window_setclass(SCM_POINTER_VALUE(xdo),
                                            SCM_SMOB_DATA(wid),
                                            scm_to_locale_string(name),
                                            scm_to_locale_string(class)));
}

SCM
xdo_window_seturgency_wrapper (SCM xdo, SCM wid, SCM urgency)
{
    assert_xdo_window(wid);
    return scm_from_int(xdo_window_seturgency(SCM_POINTER_VALUE(xdo),
                        SCM_SMOB_DATA(wid),
                        scm_to_int(urgency)));
}

SCM
xdo_window_set_override_redirect_wrapper (SCM xdo, SCM wid, SCM override_redirect)
{
    assert_xdo_window(wid);
    return scm_from_int(xdo_window_set_override_redirect(SCM_POINTER_VALUE(xdo),
                        SCM_SMOB_DATA(wid),
                        scm_to_int(override_redirect)));
}

SCM
xdo_window_focus_wrapper (SCM xdo, SCM wid)
{
    assert_xdo_window(wid);
    return scm_from_int(xdo_window_focus(SCM_POINTER_VALUE(xdo),
                                         SCM_SMOB_DATA(wid)));
}

SCM
xdo_window_raise_wrapper (SCM xdo, SCM wid)
{
    assert_xdo_window(wid);
    return scm_from_int(xdo_window_raise(SCM_POINTER_VALUE(xdo),
                                         SCM_SMOB_DATA(wid)));
}

SCM
xdo_window_get_focus_wrapper (SCM xdo)
{
    Window w;
    xdo_window_get_focus(SCM_POINTER_VALUE(xdo), &w);
    return wrap_xdo_window(w);
}

SCM
xdo_window_wait_for_focus_wrapper (SCM xdo, SCM window, SCM want_focus)
{
    assert_xdo_window(window);
    return scm_from_int(xdo_window_wait_for_focus(SCM_POINTER_VALUE(xdo),
                        SCM_SMOB_DATA(window),
                        scm_to_int(want_focus)));
}

SCM
xdo_window_get_pid_wrapper (SCM xdo, SCM window)
{
    assert_xdo_window(window);
    return scm_from_int(xdo_window_get_pid(SCM_POINTER_VALUE(xdo), SCM_SMOB_DATA(window)));
}

SCM
xdo_window_sane_get_focus_wrapper (SCM xdo)
{
    Window w;
    xdo_window_sane_get_focus(SCM_POINTER_VALUE(xdo), &w);
    return wrap_xdo_window(w);
}

SCM
xdo_window_activate_wrapper (SCM xdo, SCM wid)
{
    assert_xdo_window(wid);
    return scm_from_int(xdo_window_activate(SCM_POINTER_VALUE(xdo),
                                            SCM_SMOB_DATA(wid)));
}

SCM
xdo_window_wait_for_active_wrapper (SCM xdo, SCM window, SCM active)
{
    assert_xdo_window(window);
    return scm_from_int(xdo_window_wait_for_active(SCM_POINTER_VALUE(xdo),
                        SCM_SMOB_DATA(window), scm_to_int(active)));
}

SCM
xdo_window_map_wrapper (SCM xdo, SCM wid)
{
    assert_xdo_window(wid);
    return scm_from_int(xdo_window_map(SCM_POINTER_VALUE(xdo),
                                       SCM_SMOB_DATA(wid)));
}

SCM
xdo_window_unmap_wrapper (SCM xdo, SCM wid)
{
    assert_xdo_window(wid);
    return scm_from_int(xdo_window_unmap(SCM_POINTER_VALUE(xdo),
                                         SCM_SMOB_DATA(wid)));
}

SCM
xdo_window_minimize_wrapper (SCM xdo, SCM wid)
{
    assert_xdo_window(wid);
    return scm_from_int(xdo_window_minimize(SCM_POINTER_VALUE(xdo),
                                            SCM_SMOB_DATA(wid)));
}

SCM
xdo_window_reparent_wrapper (SCM xdo, SCM wid_source, SCM wid_target)
{
    assert_xdo_window(wid_source);
    assert_xdo_window(wid_target);
    return scm_from_int(xdo_window_reparent(SCM_POINTER_VALUE(xdo),
                                            SCM_SMOB_DATA(wid_source), SCM_SMOB_DATA(wid_target)));
}

SCM
xdo_get_window_location_wrapper ( SCM xdo, SCM wid)
{
    int x,y;
    assert_xdo_window(wid);
    xdo_get_window_location(SCM_POINTER_VALUE(xdo), SCM_SMOB_DATA(wid), &x, &y, NULL);
    return make_pair(scm_from_int(x), scm_from_int(y));
}

SCM
xdo_get_window_size_wrapper ( SCM xdo, SCM wid)
{
    unsigned int w,h;
    assert_xdo_window(wid);
    xdo_get_window_size(SCM_POINTER_VALUE(xdo), SCM_SMOB_DATA(wid), &w, &h);
    return make_pair(scm_from_uint(w), scm_from_uint(h));
}

SCM
xdo_window_get_active_wrapper(SCM xdo)
{
    Window w;
    xdo_window_get_active(SCM_POINTER_VALUE(xdo), &w);
    return wrap_xdo_window(w);
}

SCM
xdo_window_select_with_click_wrapper ( SCM xdo)
{
    Window w;
    xdo_window_select_with_click(SCM_POINTER_VALUE(xdo), &w);
    return wrap_xdo_window(w);
}

SCM
xdo_set_number_of_desktops_wrapper ( SCM xdo, SCM ndesktops)
{
    return scm_from_int(xdo_set_number_of_desktops (SCM_POINTER_VALUE(xdo), scm_to_long(ndesktops)));
}

SCM
xdo_get_number_of_desktops_wrapper ( SCM xdo)
{
    long n;
    xdo_get_number_of_desktops(SCM_POINTER_VALUE(xdo), &n);
    return scm_from_long(n);
}

SCM
xdo_set_current_desktop_wrapper ( SCM xdo, SCM desktop)
{
    return scm_from_int(xdo_set_current_desktop(SCM_POINTER_VALUE(xdo), scm_to_long(desktop)));

}

SCM
xdo_get_current_desktop_wrapper ( SCM xdo)
{
    long n;
    xdo_get_current_desktop(SCM_POINTER_VALUE(xdo), &n);
    return scm_from_long(n);
}

SCM
xdo_set_desktop_for_window_wrapper ( SCM xdo, SCM wid, SCM desktop)
{
    assert_xdo_window(wid);
    return scm_from_int(xdo_set_desktop_for_window(SCM_POINTER_VALUE(xdo), SCM_SMOB_DATA(wid), scm_to_long(desktop)));
}

SCM
xdo_get_desktop_for_window_wrapper ( SCM xdo, SCM wid)
{
    long d;
    assert_xdo_window(wid);
    xdo_get_desktop_for_window(SCM_POINTER_VALUE(xdo), SCM_SMOB_DATA(wid), &d);
    return scm_from_long(d);
}

SCM xdo_window_search_wrapper ( SCM xdo,  SCM search)
{
    Window *win_lst;
    int length, i;
    SCM lst = SCM_EOL;
    xdo_window_search(SCM_POINTER_VALUE(xdo), unwrap_xdo_search(search), &win_lst, &length);
    for (i = length ; i != 0 ; i--)
    {
        lst = scm_cons(wrap_xdo_window(win_lst[i-1]), lst);
    }
    free(win_lst);
    return lst;
}

/*
 *SCM xdo_getwinprop_wrapper ( SCM xdo, SCM window, SCM atom, SCM nitems, SCM type, SCM size)
 *{
 *  xdo_getwinprop
 *}
 */

SCM
xdo_get_input_state_wrapper ( SCM xdo)
{
    return scm_from_uint(xdo_get_input_state(SCM_POINTER_VALUE(xdo)));
}

/*
 *SCM xdo_keysym_charmap_wrapper (void)
 *{
 *  xdo_keysym_charmap
 *}
 */

SCM xdo_symbol_map_wrapper(void)
{
    const char **map = xdo_symbol_map();
    int i;
    SCM lst = SCM_EOL;
    for (i = 0 ; map[i+1] != NULL ; )
    {
        lst = scm_cons(make_pair(scm_from_locale_string(map[i]), scm_from_locale_string(map[i+1])), lst);
        i += 2;
    }
    return lst;
}

SCM xdo_get_active_modifiers_wrapper ( SCM xdo)
{
    SCM lst = SCM_EOL;
    int i;
    xdo_active_mods_t *mods;
    mods = xdo_get_active_modifiers(SCM_POINTER_VALUE(xdo));
    for (i = 0 ; i < mods->nkeymods ; i++)
    {
        lst = scm_cons(make_charcodemap(mods->keymods[i]), lst);
    }
    xdo_free_active_modifiers(mods);
    return lst;
}

SCM xdo_clear_active_modifiers_wrapper ( SCM xdo, SCM window, SCM active_mods)
{
    SCM head, tail;
    int i = 0,ret=1;
    assert_xdo_window(window);
    if (scm_pair_p(active_mods))
    {
        xdo_active_mods_t mods;
        mods.nkeymods = scm_to_int(scm_length(active_mods));
        mods.keymods = (charcodemap_t *)malloc(sizeof(charcodemap_t)*mods.nkeymods);
        head = scm_car(active_mods);
        tail = scm_cdr(active_mods);
        do
        {
            charcodemap_t *cm = (charcodemap_t *)SCM_SMOB_DATA(head);
            memcpy(&mods.keymods[i++], cm, sizeof(charcodemap_t));

            if (tail == SCM_EOL) break;
            head = scm_car(tail);
            tail = scm_cdr(tail);
        }
        while(i < mods.nkeymods);
        ret = xdo_clear_active_modifiers(SCM_POINTER_VALUE(xdo), SCM_SMOB_DATA(window), &mods);
        free(mods.keymods);
    }
    else
    {
        scm_throw(scm_from_locale_symbol("bad-argument"),
                  scm_from_locale_string("argument three must be an pair"));
    }
    return scm_from_int(ret);
}

SCM xdo_set_active_modifiers_wrapper ( SCM xdo, SCM window,  SCM active_mods)
{
    SCM head, tail;
    int i = 0,ret=1;
    assert_xdo_window(window);
    if (scm_pair_p(active_mods))
    {
        xdo_active_mods_t mods;
        mods.nkeymods = scm_to_int(scm_length(active_mods));
        mods.keymods = (charcodemap_t *)malloc(sizeof(charcodemap_t)*mods.nkeymods);
        head = scm_car(active_mods);
        tail = scm_cdr(active_mods);
        do
        {
            charcodemap_t *cm = (charcodemap_t *)SCM_SMOB_DATA(head);
            memcpy(&mods.keymods[i++], cm, sizeof(charcodemap_t));

            if (tail == SCM_EOL) break;
            head = scm_car(tail);
            tail = scm_cdr(tail);
        }
        while(i < mods.nkeymods);
        ret=xdo_set_active_modifiers(SCM_POINTER_VALUE(xdo), SCM_SMOB_DATA(window), &mods);
        free(mods.keymods);
    }
    else
    {
        scm_throw(scm_from_locale_symbol("bad-argument"),
                  scm_from_locale_string("argument three must be an pair"));
    }
    return scm_from_int(ret);
}

SCM
xdo_get_desktop_viewport_wrapper ( SCM xdo)
{
    int x,y;
    xdo_get_desktop_viewport(SCM_POINTER_VALUE(xdo), &x, &y);
    return scm_cons(scm_from_int(x), scm_cons(scm_from_int(y), SCM_EOL));
}

SCM
xdo_set_desktop_viewport_wrapper ( SCM xdo, SCM x, SCM y)
{
    return scm_from_int(xdo_set_desktop_viewport (SCM_POINTER_VALUE(xdo), scm_to_int(x), scm_to_int(y)));
}

SCM
xdo_window_kill_wrapper ( SCM xdo, SCM window)
{
    assert_xdo_window(window);
    return scm_from_int(xdo_window_kill(SCM_POINTER_VALUE(xdo), SCM_SMOB_DATA(window)));
}

SCM
xdo_window_find_client_wrapper ( SCM xdo, SCM window, SCM direction)
{
    Window c;
    int dir = (direction == SCM_UNDEFINED ? 0 : scm_to_int(direction));
    assert_xdo_window(window);
    xdo_window_find_client(SCM_POINTER_VALUE(xdo), SCM_SMOB_DATA(window), &c, dir);
    return wrap_xdo_window(c);
}

SCM
xdo_get_window_name_wrapper ( SCM xdo, SCM window)
{
    unsigned char *name;
    int len, type;
    assert_xdo_window(window);
    xdo_get_window_name(SCM_POINTER_VALUE(xdo), SCM_SMOB_DATA(window), &name, &len, &type);
    return scm_cons(scm_take_locale_stringn((char *)name, len), scm_cons(scm_from_int(type), SCM_EOL));
}

SCM
xdo_disable_feature_wrapper (SCM xdo, SCM feature)
{
    xdo_disable_feature(SCM_POINTER_VALUE(xdo), scm_to_int(feature));
    return SCM_UNDEFINED;
}

SCM
xdo_enable_feature_wrapper (SCM xdo, SCM feature)
{
    xdo_enable_feature(SCM_POINTER_VALUE(xdo), scm_to_int(feature));
    return SCM_UNDEFINED;
}

SCM
xdo_has_feature_wrapper (SCM xdo, SCM feature)
{
    return scm_from_int(xdo_has_feature(SCM_POINTER_VALUE(xdo), scm_to_int(feature)));
}

SCM
xdo_get_viewport_dimensions_wrapper (SCM xdo, SCM screen)
{
    unsigned int w,h;
    xdo_get_viewport_dimensions(SCM_POINTER_VALUE(xdo), &w, &h, scm_to_int(screen));
    return make_pair(scm_from_uint(w),scm_from_uint(h));
}

void
init_xdo_libxdo(void *unused)
{
    setup_smobs();
    scm_c_define_gsubr("xdo-new", 0, 1, 0, xdo_new_wrapper);
    scm_c_define_gsubr("xdo-version", 0, 0, 0, xdo_version_wrapper);
    scm_c_define_gsubr("xdo-mousemove", 4, 0, 0, xdo_mousemove_wrapper);
    scm_c_define_gsubr("xdo-mousemove-relative-to-window", 4, 0, 0, xdo_mousemove_rel_to_win_wrapper);
    scm_c_define_gsubr("xdo-mousemove-relative", 3, 0, 0, xdo_mousemove_rel_wrapper);
    scm_c_define_gsubr("xdo-mousedown", 3, 0, 0, xdo_mousedown_wrapper);
    scm_c_define_gsubr("xdo-mouseup", 3, 1, 0, xdo_mouseup_wrapper);
    scm_c_define_gsubr("xdo-mouselocation", 1, 0, 0, xdo_mouselocation_wrapper);
    scm_c_define_gsubr("xdo-mouse-wait-for", 3, 1, 0, xdo_mouse_wait_for_move_wrapper);
    scm_c_define_gsubr("xdo-click", 3, 2, 0, xdo_click_wrapper);
    scm_c_define_gsubr("xdo-type", 3, 1, 0, xdo_type_wrapper);
    scm_c_define_gsubr("xdo-keysequence", 3, 2, 0, xdo_keysequence_wrapper);
    scm_c_define_gsubr("xdo-active-keys-to-keycode", 1, 0, 0, xdo_active_keys_to_keycode_list_wrapper);
    scm_c_define_gsubr("xdo-window-move", 4, 0, 0, xdo_window_move_wrapper);
    scm_c_define_gsubr("xdo-window-translate-with-sizehint", 4, 0, 0, xdo_window_translate_with_sizehint);
    scm_c_define_gsubr("xdo-window-setsize", 5, 0, 0, xdo_window_setsize_wrapper);
    scm_c_define_gsubr("xdo-window-setprop", 4, 0, 0, xdo_window_setprop_wrapper);
    scm_c_define_gsubr("xdo-window-setclass", 4, 0, 0, xdo_window_setclass_wrapper);
    scm_c_define_gsubr("xdo-window-seturgency", 3, 0, 0, xdo_window_seturgency_wrapper);
    scm_c_define_gsubr("xdo-window-set-override-redirect", 3, 0, 0, xdo_window_set_override_redirect_wrapper);
    scm_c_define_gsubr("xdo-window-focus", 2, 0, 0, xdo_window_focus_wrapper);
    scm_c_define_gsubr("xdo-window-raise", 2, 0, 0, xdo_window_raise_wrapper);
    scm_c_define_gsubr("xdo-window-get-focus", 1, 0, 0, xdo_window_get_focus_wrapper);
    scm_c_define_gsubr("xdo-window-wait-for-focus", 3, 0, 0, xdo_window_wait_for_focus_wrapper);
    scm_c_define_gsubr("xdo-window-get-pid", 2, 0, 0, xdo_window_get_pid_wrapper);
    scm_c_define_gsubr("xdo-window-sane-get-focus", 1, 0, 0, xdo_window_sane_get_focus_wrapper);
    scm_c_define_gsubr("xdo-window-activate", 2, 0, 0, xdo_window_activate_wrapper);
    scm_c_define_gsubr("xdo-window-wait-for-active", 3, 0, 0, xdo_window_wait_for_active_wrapper);
    scm_c_define_gsubr("xdo-window-map", 2, 0, 0, xdo_window_map_wrapper);
    scm_c_define_gsubr("xdo-window-unmap", 2, 0, 0, xdo_window_unmap_wrapper);
    scm_c_define_gsubr("xdo-window-minimize", 2, 0, 0, xdo_window_minimize_wrapper);
    scm_c_define_gsubr("xdo-window-reparent", 3, 0, 0, xdo_window_reparent_wrapper);
    scm_c_define_gsubr("xdo-get-window-location", 2, 0, 0, xdo_get_window_location_wrapper);
    scm_c_define_gsubr("xdo-get-window-size", 2, 0, 0, xdo_get_window_size_wrapper);
    scm_c_define_gsubr("xdo-window-get-active", 1, 0, 0, xdo_window_get_active_wrapper);
    scm_c_define_gsubr("xdo-window-select-with-click", 1, 0, 0, xdo_window_select_with_click_wrapper);
    scm_c_define_gsubr("xdo-set-number-of-desktops", 2, 0, 0, xdo_set_number_of_desktops_wrapper);
    scm_c_define_gsubr("xdo-get-number-of-desktops", 1, 0, 0, xdo_get_number_of_desktops_wrapper);
    scm_c_define_gsubr("xdo-set-current-desktop", 2, 0, 0, xdo_set_current_desktop_wrapper);
    scm_c_define_gsubr("xdo-get-current-desktop", 1, 0, 0, xdo_get_current_desktop_wrapper);
    scm_c_define_gsubr("xdo-set-desktop-for-window", 2, 0, 0, xdo_set_desktop_for_window_wrapper);
    scm_c_define_gsubr("xdo-get-desktop-for-window", 1, 0, 0, xdo_get_desktop_for_window_wrapper);
    scm_c_define_gsubr("xdo-window-search", 2, 0, 0, xdo_window_search_wrapper);
    /*scm_c_define_gsubr("xdo-getwinprop", 6, 0, 0, xdo_getwinprop_wrapper);*/
    scm_c_define_gsubr("xdo-get-input-state", 1, 0, 0, xdo_get_input_state_wrapper);
    /*scm_c_define_gsubr("xdo-keysym-charmap", 0, 0, 0, xdo_keysym_charmap_wrapper);*/
    scm_c_define_gsubr("xdo-symbol-map", 0, 0, 0, xdo_symbol_map_wrapper);
    scm_c_define_gsubr("xdo-get-active-modifiers", 1, 0, 0, xdo_get_active_modifiers_wrapper);
    scm_c_define_gsubr("xdo-clear-active-modifiers", 3, 0, 0, xdo_clear_active_modifiers_wrapper);
    scm_c_define_gsubr("xdo-set-active-modifiers", 0, 0, 0, xdo_set_active_modifiers_wrapper);
    scm_c_define_gsubr("xdo-get-desktop-viewport", 1, 0, 0, xdo_get_desktop_viewport_wrapper);
    scm_c_define_gsubr("xdo-set-desktop-viewport", 3, 0, 0, xdo_set_desktop_viewport_wrapper);
    scm_c_define_gsubr("xdo-window-kill", 2, 0, 0, xdo_window_kill_wrapper);
    scm_c_define_gsubr("xdo-window-find-client", 3, 0, 0, xdo_window_find_client_wrapper);
    scm_c_define_gsubr("xdo-get-window-name", 2, 0, 0, xdo_get_window_name_wrapper);
    scm_c_define_gsubr("xdo-disable-feature", 2, 0, 0, xdo_disable_feature_wrapper);
    scm_c_define_gsubr("xdo-enable-feature", 2, 0, 0, xdo_enable_feature_wrapper);
    scm_c_define_gsubr("xdo-has-feature", 2, 0, 0, xdo_has_feature_wrapper);
    scm_c_define_gsubr("xdo-get-viewport-dimensions", 2, 0, 0, xdo_get_viewport_dimensions_wrapper);
    scm_c_export(
        "xdo-new",
        "xdo-version",
        "xdo-mousemove",
        "xdo-mousemove-relative-to-window",
        "xdo-mousemove-relative",
        "xdo-mousedown",
        "xdo-mouseup",
        "xdo-mouselocation",
        "xdo-mouse-wait-for",
        "xdo-click",
        "xdo-type",
        "xdo-keysequence",
        "xdo-active-keys-to-keycode",
        "xdo-window-move",
        "xdo-window-translate-with-sizehint",
        "xdo-window-setsize",
        "xdo-window-setprop",
        "xdo-window-reparent",
        "xdo-window-minimize",
        "xdo-window-unmap",
        "xdo-window-map",
        "xdo-window-wait-for-active",
        "xdo-window-activate",
        "xdo-window-sane-get-focus",
        "xdo-window-get-pid",
        "xdo-window-wait-for-focus",
        "xdo-window-get-focus",
        "xdo-window-raise",
        "xdo-window-focus",
        "xdo-window-set-override-redirect",
        "xdo-window-seturgency",
        "xdo-window-setclass",
        "xdo-get-window-location",
        "xdo-get-window-size",
        "xdo-window-get-active",
        "xdo-window-select-with-click",
        "xdo-set-number-of-desktops",
        "xdo-get-number-of-desktops",
        "xdo-set-current-desktop",
        "xdo-get-current-desktop",
        "xdo-set-desktop-for-window",
        "xdo-get-desktop-for-window",
        "xdo-window-search",
        "xdo-getwinprop",
        "xdo-get-input-state",
        "xdo-keysym-charmap",
        "xdo-symbol-map",
        "xdo-get-active-modifiers",
        "xdo-clear-active-modifiers",
        "xdo-set-active-modifiers",
        "xdo-get-desktop-viewport",
        "xdo-set-desktop-viewport",
        "xdo-window-kill",
        "xdo-window-find-client",
        "xdo-get-window-name",
        "xdo-disable-feature",
        "xdo-enable-feature",
        "xdo-has-feature",
        "xdo-get-viewport-dimensions",
        EXPORT_SMOB_FUNCTIONS
        NULL);
}

void
scm_init_xdo_libxdo_module()
{
    scm_c_define_module("xdo libxdo", init_xdo_libxdo, NULL);
}
