%% This is the application resource file (.app file) for the 'base'
%% application.
{application, dbase_unit_test,
[{description, "dbase_unit_test  " },
{vsn, "1.0.0" },
{modules, 
	  [dbase_unit_test_app,dbase_unit_test_sup,dbase_unit_test]},
{registered,[dbase_unit_test,control,dbase,common]},
{applications, [kernel,stdlib]},
{mod, {dbase_unit_test_app,[]}},
{start_phases, []}
]}.
