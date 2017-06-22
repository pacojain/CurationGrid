BeginPackage["CurationGrid`"]

CurationGrid::usage= "This is a usage message ;)"
sortList::usage= "Another usage message!"
sortList2::usage= "Another usage message!"

Begin["Private`"]

$historyLength = 30
ClearAll[CurationGrid]
SetAttributes[CurationGrid, HoldFirst]
CurationGrid[data_]:=Module[
	{
		varInName = ToString[HoldForm[data]]
	},
	Print[varInName]
]

ClearAll[sortList];
sortby = OrderedQ;
sortList[list_]:= (state= None; Dynamic[Grid[
	Join[{{
		Button["alpha", Switch[state,
			"Ascending", state = "Descending"; sortby = OrderedQ,
			_, state = "Ascending"; sortby = (Not@OrderedQ[##]) &
		]],
		Button["num", Switch[state,
			"Ascending", state = "Descending"; sortby = (#1[[2]] < #2[[2]] &),
			_, state = "Ascending"; sortby = (#1[[2]] > #2[[2]] &)
		]],
		state (* Just to see to see what it's set to *)
	}}, Sort[list, sortby]]
]])

ClearAll[sortList2];
Attributes[HoldFirst]
sortList2[listIn_, opts: OptionsPattern[]]:= DynamicModule[
	{
		state= None,
		hideFlag = False,
		sortby = OrderedQ,
		list = MapThread[Join, {Table[{j, True}, {j, Range[Length[listIn]]}], listIn}]
	},
	Dynamic[Grid[
		Join[{{
			Button[If[hideFlag, "u", "h"], Switch[hideFlag,
				True, hideFlag = False,
				False, hideFlag = True
			]],
			Button["alpha", Switch[state,
				"Ascending", state = "Descending"; sortby = OrderedQ,
				_, state = "Ascending"; sortby = (Not@OrderedQ[##]) &
			]],
			Button["num", Switch[state,
				"Ascending", state = "Descending"; sortby = (#1[[2]] < #2[[2]] &),
				_, state = "Ascending"; sortby = (#1[[2]] > #2[[2]] &)
			]]
		}},
		(*Prepend[#[[3;;]], Button["x", list= MapAt[Not, list, {First[#],2}]]]& /@ Sort[list, sortby]],*)
		If[#[[2]] || ! hideFlag, Prepend[#[[3;;]], Button[If[#[[2]]," ", "x"], list= MapAt[Not, list, {First[#],2}]]], Nothing]& /@ Sort[list, sortby]],
		FilterRules[{opts}, Options[Grid]]
	]]
]


End[]

EndPackage[]
