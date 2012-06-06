/* Thin-wrapper of libxdo for Guile 
 * Copyright (C) 2012  Krister Svanlund <krister.svanlund@gmail.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#ifndef __XDO_SMOBS__
#define __XDO_SMOBS__

static scm_t_bits xdo_window_tag;

static int
print_xdo_window(SCM window_smob, SCM port, scm_print_state *pstate)
{
    Window window = (Window)SCM_SMOB_DATA(window_smob);
    scm_puts("#<window [", port);
    scm_display(scm_from_ulong(window), port);
    scm_puts("]>", port);

    return 1;
}

SCM
wrap_xdo_window(Window window)
{
    if (window >= 0)
    {
        SCM smob;
        SCM_NEWSMOB(smob, xdo_window_tag, window);
        return smob;
    }
    else return SCM_UNDEFINED;
}

SCM
is_xdo_window(SCM exp)
{
    return scm_from_bool(SCM_SMOB_PREDICATE(xdo_window_tag, exp));
}

static scm_t_bits charcodemap_tag;
//typedef struct charcodemap {
//  wchar_t key;
//  KeyCode code;
//  KeySym symbol;
//  int index;
//  int modmask;
//  int needs_binding;
//} charcodemap_t;

SCM
make_charcodemap_s(wchar_t key, KeyCode code, KeySym symbol, int index, int modmask, int needs_binding)
{
    SCM smob;
    charcodemap_t *ccm = (charcodemap_t *)scm_gc_malloc(sizeof(charcodemap_t), "charcodemap");
    ccm->key = key;
    ccm->code = code;
    ccm->symbol = symbol;
    ccm->index = index;
    ccm->modmask = modmask;
    ccm->needs_binding = needs_binding;
    SCM_NEWSMOB(smob, charcodemap_tag, ccm);
    return smob;
}

SCM
make_charcodemap(charcodemap_t org)
{
    SCM smob;
    charcodemap_t *ccm = (charcodemap_t *)scm_gc_malloc(sizeof(charcodemap_t), "charcodemap");
    ccm->key = org.key;
    ccm->code = org.code;
    ccm->symbol = org.symbol;
    ccm->index = org.index;
    ccm->modmask = org.modmask;
    ccm->needs_binding = org.needs_binding;
    SCM_NEWSMOB(smob, charcodemap_tag, ccm);
    return smob;
}

SCM
get_charcodemap_key(SCM ccm_smob)
{
    charcodemap_t *ccm = (charcodemap_t *)SCM_SMOB_DATA(ccm_smob);
    return scm_from_char(ccm->key);
}

SCM
get_charcodemap_code(SCM ccm_smob)
{
    charcodemap_t *ccm = (charcodemap_t *)SCM_SMOB_DATA(ccm_smob);
    return scm_from_uchar(ccm->code);
}

SCM
get_charcodemap_symbol(SCM ccm_smob)
{
    charcodemap_t *ccm = (charcodemap_t *)SCM_SMOB_DATA(ccm_smob);
    return scm_from_uint(ccm->symbol);
}

SCM
get_charcodemap_index(SCM ccm_smob)
{
    charcodemap_t *ccm = (charcodemap_t *)SCM_SMOB_DATA(ccm_smob);
    return scm_from_int(ccm->index);
}

SCM
get_charcodemap_modmask(SCM ccm_smob)
{
    charcodemap_t *ccm = (charcodemap_t *)SCM_SMOB_DATA(ccm_smob);
    return scm_from_int(ccm->modmask);
}

SCM
get_charcodemap_needs_binding(SCM ccm_smob)
{
    charcodemap_t *ccm = (charcodemap_t *)SCM_SMOB_DATA(ccm_smob);
    return scm_from_int(ccm->needs_binding);
}

static int
print_charcodemap(SCM ccm_smob, SCM port, scm_print_state *pstate)
{
    scm_puts("#<charcodemap key:", port);
    scm_display(get_charcodemap_key(ccm_smob), port);
    scm_puts(" code:", port);
    scm_display(get_charcodemap_code(ccm_smob), port);
    scm_puts(" symbol:", port);
    scm_display(get_charcodemap_symbol(ccm_smob), port);
    scm_puts(" index:", port);
    scm_display(get_charcodemap_index(ccm_smob), port);
    scm_puts(" modmask:", port);
    scm_display(get_charcodemap_modmask(ccm_smob), port);
    scm_puts(" needs_binding:", port);
    scm_display(get_charcodemap_needs_binding(ccm_smob), port);
    scm_puts(">", port);

    return 1;
}

static scm_t_bits mouse_location_tag;
typedef struct mouse_location
{
    int x, y, screen;
    SCM window;
} mouse_location_t;

SCM
make_mouse_location(int x, int y, int screen, Window window)
{
    SCM smob;
    mouse_location_t *ml = (mouse_location_t *)scm_gc_malloc(sizeof(mouse_location_t), "mouse-location");
    ml->x = x;
    ml->y = y;
    ml->screen = screen;
    ml->window = wrap_xdo_window(window);
    SCM_NEWSMOB(smob, mouse_location_tag, ml);
    return smob;
}

static SCM
mark_mouse_location(SCM ml_smob)
{
    mouse_location_t *ml = (mouse_location_t *)SCM_SMOB_DATA(ml_smob);
    scm_gc_mark(ml->window);
    return SCM_BOOL_F;
}

static int
print_mouse_location(SCM ml_smob, SCM port, scm_print_state *pstate)
{
    mouse_location_t *data = (mouse_location_t *)SCM_SMOB_DATA(ml_smob);
    scm_puts("#<mouse-location (", port);
    scm_display(scm_from_int(data->x), port);
    scm_puts(", ", port);
    scm_display(scm_from_int(data->y), port);
    scm_puts(") ", port);
    scm_display(scm_from_int(data->screen), port);
    if (data->window != SCM_UNDEFINED)
    {
        scm_puts(" ", port);
        scm_display(data->window, port);
    }
    scm_puts(">", port);

    return 1;
}

SCM
get_mouse_location_x(SCM mouse_location_smob)
{
    mouse_location_t *ml = (mouse_location_t *)SCM_SMOB_DATA(mouse_location_smob);
    return scm_from_int(ml->x);
}

SCM
get_mouse_location_y(SCM mouse_location_smob)
{
    mouse_location_t *ml = (mouse_location_t *)SCM_SMOB_DATA(mouse_location_smob);
    return scm_from_int(ml->y);
}

SCM
get_mouse_location_screen(SCM mouse_location_smob)
{
    mouse_location_t *ml = (mouse_location_t *)SCM_SMOB_DATA(mouse_location_smob);
    return scm_from_int(ml->screen);
}

SCM
get_mouse_location_window(SCM mouse_location_smob)
{
    mouse_location_t *ml = (mouse_location_t *)SCM_SMOB_DATA(mouse_location_smob);
    return ml->window;
}

static scm_t_bits xdo_search_tag;
// typedef struct xdo_search {
//   const char *title;
//   const char *winclass;
//   const char *winclassname;
//   const char *winname;
//   int pid;
//   long max_depth;
//   int only_visible;
//   int screen;
//   enum { SEARCH_ANY, SEARCH_ALL } require;
//   unsigned int searchmask;
//   long desktop;
//   unsigned int limit;
// } xdo_search_t;

SCM
make_xdo_search(const char *title, const char *winclass,
                const char *winclassname, const char *winname,
                int pid, long max_depth, int only_visible,
                int screen, int require, unsigned int searchmask,
                long desktop, unsigned int limit)
{
    SCM smob;
    xdo_search_t *sq = (xdo_search_t *)scm_gc_malloc(sizeof(xdo_search_t), "xdo_search");
#define _(d, s) { if (s != NULL) { d = (const char*)malloc(strlen(s)); memcpy((char*)d, s, strlen(s)); } }
    _(sq->title, title);
    _(sq->winclass, winclass);
    _(sq->winclassname, winclassname);
    _(sq->winname, winname);
#undef _
    sq->pid = pid;
    sq->max_depth = max_depth;
    sq->only_visible = only_visible;
    sq->screen = screen;
    sq->require = require;
    sq->searchmask = searchmask;
    sq->desktop = desktop;
    sq->limit = limit;
    SCM_NEWSMOB(smob, xdo_search_tag, sq);
    return smob;
}

SCM
make_xdo_search_wrapper(SCM title, SCM winclass, SCM winclassname,
                        SCM winname, SCM pid, SCM max_depth,
                        SCM only_visible, SCM screen,
                        SCM desktop, SCM require)
{
    SCM ret;
    unsigned int mask = 0;
    char *title_c = NULL, *winclass_c = NULL, *winclassname_c = NULL, *winname_c = NULL;
    if (!scm_is_false(title)) {
      title_c = scm_to_locale_string(title); mask += (1 << 0);
    }
    if (!scm_is_false(winclass)) {
      winclass_c = scm_to_locale_string(winclass); mask += (1 << 1);
    }
    if (!scm_is_false(winname)) {
      winname_c = scm_to_locale_string(winname); mask += (1 << 2);
    }
    if (!scm_is_false(winclassname)) {
      winclassname_c = scm_to_locale_string(winclassname); mask += (1 << 6);
    }
    if (!scm_is_false(pid)) mask += (1 << 3);
    if (!scm_is_false(only_visible)) mask += (1 << 4);
    if (!scm_is_false(screen)) mask += (1 << 5);
    if (!scm_is_false(desktop)) mask += (1 << 7);
#define _(x, y, z) (scm_is_false(x) ? y : z)
    ret = make_xdo_search(
            title_c,
            winclass_c,
            winclassname_c,
            winname_c,
            _(pid, 0, scm_to_int(pid)), // pid
            _(max_depth, 0, scm_to_long(max_depth)), // max_depth
            _(only_visible, 0, (scm_is_true(only_visible) ? 1 : scm_to_int(only_visible))),
            _(screen, 0, scm_to_int(screen)), // screen
            _(require, 0, scm_to_int(require)), // require
            mask, // searchmask
            _(desktop, 0, scm_to_long(desktop)), // desktop
            1024); // limit
#undef _
    if (title_c != NULL) free(title_c);
    if (winclass_c != NULL) free(winclass_c);
    if (winclassname_c != NULL) free(winclassname_c);
    if (winname_c != NULL) free(winname_c);
    return ret;
}

xdo_search_t *
unwrap_xdo_search(SCM smob)
{
    return (xdo_search_t *)SCM_SMOB_DATA(smob);
}

size_t
free_xdo_search(SCM smob)
{
    xdo_search_t *sq = (xdo_search_t *)SCM_SMOB_DATA(smob);
    if (sq->title != NULL) free((char *)sq->title);
    if (sq->winclass != NULL) free((char *)sq->winclass);
    if (sq->winclassname != NULL) free((char *)sq->winclassname);
    if (sq->winname != NULL) free((char *)sq->winname);

    return 0;
}

#define EXPORT_SMOB_FUNCTIONS "mouse-location-x",\
                              "mouse-location-y",\
                              "mouse-location-screen",\
                              "mouse-location-window",\
                              "charcodemap-key",\
                              "charcodemap-code",\
                              "charcodemap-symbol",\
                              "charcodemap-index",\
                              "charcodemap-modmask",\
                              "charcodemap-needs-binding",\
                              "make-xdo-search",

void setup_smobs()
{
    xdo_window_tag = scm_make_smob_type("xdo-window", sizeof(Window));
    scm_set_smob_print(xdo_window_tag, print_xdo_window);
    scm_c_define_gsubr("xdo-window?", 1, 0, 0, is_xdo_window);

    charcodemap_tag = scm_make_smob_type("charcodemap", sizeof(charcodemap_t));
    scm_set_smob_print(charcodemap_tag, print_charcodemap);
    scm_c_define_gsubr("charcodemap-key", 1, 0, 0, get_charcodemap_key);
    scm_c_define_gsubr("charcodemap-code", 1, 0, 0, get_charcodemap_code);
    scm_c_define_gsubr("charcodemap-symbol", 1, 0, 0, get_charcodemap_symbol);
    scm_c_define_gsubr("charcodemap-index", 1, 0, 0, get_charcodemap_index);
    scm_c_define_gsubr("charcodemap-modmask", 1, 0, 0, get_charcodemap_modmask);
    scm_c_define_gsubr("charcodemap-needs-binding", 1, 0, 0, get_charcodemap_needs_binding);

    mouse_location_tag = scm_make_smob_type("mouse-location", sizeof(mouse_location_t));
    scm_set_smob_print(mouse_location_tag, print_mouse_location);
    scm_set_smob_mark(mouse_location_tag, mark_mouse_location);
    scm_c_define_gsubr("mouse-location-x", 1, 0, 0, get_mouse_location_x);
    scm_c_define_gsubr("mouse-location-y", 1, 0, 0, get_mouse_location_y);
    scm_c_define_gsubr("mouse-location-screen", 1, 0, 0, get_mouse_location_screen);
    scm_c_define_gsubr("mouse-location-window", 1, 0, 0, get_mouse_location_window);

    xdo_search_tag = scm_make_smob_type("xdo_search", sizeof(xdo_search_t));
    scm_set_smob_free(xdo_search_tag, free_xdo_search);
    scm_c_define_gsubr("make-xdo-search", 10, 0, 0, make_xdo_search_wrapper);
}

#endif // __XDO_SMOBS__
