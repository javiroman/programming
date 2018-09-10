%%-------------------------------------------------------------------
%% @author javiroman
%% @copyright (C) 2015, <COMPANY>
%% @doc
%%
%% @end
%% Created : 09. Dec 2015 1:43 AM
%%%%%%-------------------------------------------------------------------
-author("javiroman").

-define(APP, cowboy_react).

-define(ROUTES,
[{
  '_',
  [
    {"/", cowboy_static, {priv_file, ?APP, "build/index.html"}},
    {"/static/[...]", cowboy_static, {priv_dir, ?APP, "build/static",
      [{mimetypes, cow_mimetypes, all}]}}
  ]
}]
).
