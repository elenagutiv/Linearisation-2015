:- module(_, [], [ciaobld(bundlehooks)]).

:- doc(title,  "Bundle Hooks for Linearisation-2015").

'$builder_hook'(desc_name('Linearisation-2015')).

% ============================================================================

:- use_module(ciaobld(ciaoc_aux), [build_libs/2]).

'$builder_hook'(build_libraries) :-
	build_libs('Linearisation-2015', 'src').

'$builder_hook'(build_bin) :-
	bundleitem_do(linearisepe, 'Linearisation-2015', build_nodocs).

% TODO: just say cmd('cmds/linearisepe', [...])
'$builder_hook'(linearisepe:item_def( 
    cmds_list('Linearisation-2015', bundle_src('Linearisation-2015')/'src', [
        'linearise'-[
          output='linearise', % (executable will be called 'linearise')
	  plexe,
	  final_ciaoc
	]
    ]))).

'$builder_hook'(install) :- bundleitem_do(only_global_ins(~desc), 'Linearisation-2015', install).

'$builder_hook'(uninstall) :- bundleitem_do(only_global_ins(~desc), 'Linearisation-2015', uninstall).

desc := [
  linearisepe,
  lib('Linearisation-2015', 'src')
].
