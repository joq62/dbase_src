%% This is the application resource file (.app file) for the 'base'
%% application.
{application,dbase,
[{description, "dbase " },
{vsn, "1.0.0" },
{modules, 
	  [dbase_app,dbase_sup,dbase]},
{registered,[dbase]},
{applications, [kernel,stdlib]},
{mod, {dbase_app,[]}},
{start_phases, []}
]}.
