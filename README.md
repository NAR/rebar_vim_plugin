rebar_vim_plugin
================
A rebar plugin to support `vim-rebar`.

Install
-------
- Clone this repo into your `ERL_LIBS` path. If `~/.erllibs` is in `ERL_LIBS`:

```
cd ~/.erllibs && git clone https://github.com/fishcakez/rebar_vim_plugin.git
```

- Compile the plugin:

```
cd rebar_vim_plugin && rebar compile
```

- Add plugin to default rebar config (located at `~/.rebar/config`):

```
{plugins, [rebar_vim_plugin]}.
```
