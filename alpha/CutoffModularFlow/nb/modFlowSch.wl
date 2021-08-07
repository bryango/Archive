#!/usr/bin/env wolframscript
(* ::Package:: *)

(* ::Subsubsection:: *)
(*Setup*)


(* ::Input::Initialization:: *)
SetDirectory[NotebookDirectory[]];


(* ::Input::Initialization:: *)
dX[n_]:=Symbol["d"<>ToString[coord[[n]]]]


(* ::Input::Initialization:: *)
$Assumptions=And[u\[Element]Reals,r\[Element]Reals,v\[Element]Reals,r>Tu+Tv>0,Tu>0,Tv>0,L>0,T>0,r>2T,rc>2T,L\[Infinity]>L,x\[Element]Reals];


(* ::Input::Initialization:: *)
coord={u,v,r};
metricSign=-1;


(* ::Subsubsection:: *)
(*Poincar\[EAcute] to BTZ*)


(* ::Input::Initialization:: *)
u2btz=u->Sqrt[(r^2-(Tu+Tv)^2)/(r^2-(Tu-Tv)^2)] E^(2u*Tu);
v2btz=(u2btz/.u->v/.{Tu->Tv,Tv->Tu})//Simplify
z2btz=z->Sqrt[((Tu+Tv)^2-(Tu-Tv)^2)/(r^2-(Tu-Tv)^2)]Exp[(u+v)/2 (Tu+Tv)+(u-v)/2 (Tu-Tv)]//Simplify
poincare2btz={u2btz,v2btz,z2btz};

(Dt[u]Dt[v]+Dt[z]^2)/z^2/.poincare2btz/.(Dt[#]->0&/@{Tu,Tv})//Simplify


(* ::Input:: *)
(**)


(* ::Input:: *)
(*(r^2-(Tu+Tv)^2)(r^2-(Tu-Tv)^2)==r^4+(Tu^2-Tv^2)^2-2 r^2 (Tu^2+Tv^2)//Simplify*)


(* ::Input::Initialization:: *)
ds2=(r^2 dr^2)/((r^2-(Tu+Tv)^2)(r^2-(Tu-Tv)^2))+Tu^2 du^2+Tv^2 dv^2+(r^2-Tu^2-Tv^2)du dv;


(* ::Input::Initialization:: *)
metric=btz`metric=Table[1/2 D[ds2,dX[i],dX[j]],{i,1,Length[coord]},{j,1,Length[coord]}]


(* ::Input::Initialization:: *)
<<diffgeoM`


(* ::Input:: *)
(**)


(* ::Input:: *)
(*Simplify[Einstein- metric]*)


(* ::Input:: *)
(**)


(* ::Input:: *)
(*("DefaultPlotStyle"/.(Method/.Charting`ResolvePlotTheme[#2,#1]))/.Directive[x_,__]:>x&[Plot,"Web"]*)


(* ::Input:: *)
(**)


(* ::Input:: *)
(*#.g.#&[Dt[coord]]/.r->Sqrt[\[Rho]+Tu^2+Tv^2]/.{Dt[Tu]->0,Dt[Tv]->0}//Expand//FullSimplify*)


(* ::Input:: *)
(*Tu^2 Dt[u]^2+(r^2-Tu^2-Tv^2) Dt[u] Dt[v]+Tv^2 Dt[v]^2*)


(* ::Subsection:: *)
(*Killing vectors*)


(* ::Input::Initialization:: *)
ju=Function[{n},\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
RowBox[{
SuperscriptBox["E", 
RowBox[{
RowBox[{"-", "2"}], " ", "Tu", " ", "u"}]], 
RowBox[{"{", 
RowBox[{
FractionBox[
RowBox[{"-", 
RowBox[{"(", 
RowBox[{
SuperscriptBox["r", "2"], "-", 
SuperscriptBox["Tu", "2"], "-", 
SuperscriptBox["Tv", "2"]}], ")"}]}], 
RowBox[{"2", " ", "Tu", " ", 
SqrtBox[
RowBox[{
RowBox[{"(", 
RowBox[{
SuperscriptBox["r", "2"], "-", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{"Tu", "-", "Tv"}], ")"}], "2"]}], ")"}], " ", 
RowBox[{"(", 
RowBox[{
SuperscriptBox["r", "2"], "-", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{"Tu", "+", "Tv"}], ")"}], "2"]}], ")"}]}]]}]], ",", 
FractionBox["Tu", 
SqrtBox[
RowBox[{
RowBox[{"(", 
RowBox[{
SuperscriptBox["r", "2"], "-", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{"Tu", "-", "Tv"}], ")"}], "2"]}], ")"}], " ", 
RowBox[{"(", 
RowBox[{
SuperscriptBox["r", "2"], "-", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{"Tu", "+", "Tv"}], ")"}], "2"]}], ")"}]}]]], ",", 
FractionBox[
RowBox[{"-", 
SqrtBox[
RowBox[{
RowBox[{"(", 
RowBox[{
SuperscriptBox["r", "2"], "-", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{"Tu", "-", "Tv"}], ")"}], "2"]}], ")"}], " ", 
RowBox[{"(", 
RowBox[{
SuperscriptBox["r", "2"], "-", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{"Tu", "+", "Tv"}], ")"}], "2"]}], ")"}]}]]}], 
RowBox[{"2", " ", "r"}]]}], "}"}]}], 
RowBox[{"n", "==", 
RowBox[{"-", "1"}]}]},
{
RowBox[{"{", 
RowBox[{
FractionBox[
RowBox[{"-", "1"}], 
RowBox[{"2", " ", "Tu"}]], ",", "0", ",", "0"}], "}"}], 
RowBox[{"n", "==", "0"}]},
{
RowBox[{
SuperscriptBox["E", 
RowBox[{
RowBox[{"+", "2"}], " ", "Tu", " ", "u"}]], 
RowBox[{"{", 
RowBox[{
FractionBox[
RowBox[{"-", 
RowBox[{"(", 
RowBox[{
SuperscriptBox["r", "2"], "-", 
SuperscriptBox["Tu", "2"], "-", 
SuperscriptBox["Tv", "2"]}], ")"}]}], 
RowBox[{"2", " ", "Tu", " ", 
SqrtBox[
RowBox[{
RowBox[{"(", 
RowBox[{
SuperscriptBox["r", "2"], "-", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{"Tu", "-", "Tv"}], ")"}], "2"]}], ")"}], " ", 
RowBox[{"(", 
RowBox[{
SuperscriptBox["r", "2"], "-", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{"Tu", "+", "Tv"}], ")"}], "2"]}], ")"}]}]]}]], ",", 
FractionBox["Tu", 
SqrtBox[
RowBox[{
RowBox[{"(", 
RowBox[{
SuperscriptBox["r", "2"], "-", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{"Tu", "-", "Tv"}], ")"}], "2"]}], ")"}], " ", 
RowBox[{"(", 
RowBox[{
SuperscriptBox["r", "2"], "-", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{"Tu", "+", "Tv"}], ")"}], "2"]}], ")"}]}]]], ",", 
FractionBox[
RowBox[{"+", 
SqrtBox[
RowBox[{
RowBox[{"(", 
RowBox[{
SuperscriptBox["r", "2"], "-", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{"Tu", "-", "Tv"}], ")"}], "2"]}], ")"}], " ", 
RowBox[{"(", 
RowBox[{
SuperscriptBox["r", "2"], "-", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{"Tu", "+", "Tv"}], ")"}], "2"]}], ")"}]}]]}], 
RowBox[{"2", " ", "r"}]]}], "}"}]}], 
RowBox[{"n", "==", "1"}]},
{"0", 
TagBox["True",
"PiecewiseDefault",
AutoDelete->True]}
},
AllowedDimensions->{2, Automatic},
Editable->True,
GridBoxAlignment->{"Columns" -> {{Left}}, "Rows" -> {{Baseline}}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "Rows" -> {{1.}}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.84]}, Offset[0.27999999999999997`]}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}},
Selectable->True]}
},
GridBoxAlignment->{"Columns" -> {{Left}}, "Rows" -> {{Baseline}}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "Rows" -> {{1.}}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.35]}, Offset[0.27999999999999997`]}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}}],
"Piecewise",
DeleteWithContents->True,
Editable->False,
SelectWithContents->True,
Selectable->False,
StripWrapperBoxes->True]\)];
jv=Function[{n},\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
RowBox[{
SuperscriptBox["E", 
RowBox[{
RowBox[{"-", "2"}], " ", "Tv", " ", "v"}]], " ", 
RowBox[{"{", 
RowBox[{
FractionBox["Tv", 
SqrtBox[
RowBox[{
RowBox[{"(", 
RowBox[{
SuperscriptBox["r", "2"], "-", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{"Tu", "-", "Tv"}], ")"}], "2"]}], ")"}], " ", 
RowBox[{"(", 
RowBox[{
SuperscriptBox["r", "2"], "-", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{"Tu", "+", "Tv"}], ")"}], "2"]}], ")"}]}]]], ",", 
FractionBox[
RowBox[{"-", 
RowBox[{"(", 
RowBox[{
SuperscriptBox["r", "2"], "-", 
SuperscriptBox["Tu", "2"], "-", 
SuperscriptBox["Tv", "2"]}], ")"}]}], 
RowBox[{"2", " ", "Tv", 
SqrtBox[
RowBox[{
RowBox[{"(", 
RowBox[{
SuperscriptBox["r", "2"], "-", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{"Tu", "-", "Tv"}], ")"}], "2"]}], ")"}], " ", 
RowBox[{"(", 
RowBox[{
SuperscriptBox["r", "2"], "-", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{"Tu", "+", "Tv"}], ")"}], "2"]}], ")"}]}]]}]], ",", 
RowBox[{"-", 
FractionBox[
SqrtBox[
RowBox[{
RowBox[{"(", 
RowBox[{
SuperscriptBox["r", "2"], "-", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{"Tu", "-", "Tv"}], ")"}], "2"]}], ")"}], " ", 
RowBox[{"(", 
RowBox[{
SuperscriptBox["r", "2"], "-", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{"Tu", "+", "Tv"}], ")"}], "2"]}], ")"}]}]], 
RowBox[{"2", " ", "r"}]]}]}], "}"}]}], 
RowBox[{"n", "==", 
RowBox[{"-", "1"}]}]},
{
RowBox[{"{", 
RowBox[{"0", ",", 
FractionBox[
RowBox[{"-", "1"}], 
RowBox[{"2", " ", "Tv"}]], ",", "0"}], "}"}], 
RowBox[{"n", "==", "0"}]},
{
RowBox[{
SuperscriptBox["E", 
RowBox[{
RowBox[{"+", "2"}], " ", "Tv", " ", "v"}]], " ", 
RowBox[{"{", 
RowBox[{
FractionBox["Tv", 
SqrtBox[
RowBox[{
RowBox[{"(", 
RowBox[{
SuperscriptBox["r", "2"], "-", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{"Tu", "-", "Tv"}], ")"}], "2"]}], ")"}], " ", 
RowBox[{"(", 
RowBox[{
SuperscriptBox["r", "2"], "-", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{"Tu", "+", "Tv"}], ")"}], "2"]}], ")"}]}]]], ",", 
FractionBox[
RowBox[{"-", 
RowBox[{"(", 
RowBox[{
SuperscriptBox["r", "2"], "-", 
SuperscriptBox["Tu", "2"], "-", 
SuperscriptBox["Tv", "2"]}], ")"}]}], 
RowBox[{"2", " ", "Tv", 
SqrtBox[
RowBox[{
RowBox[{"(", 
RowBox[{
SuperscriptBox["r", "2"], "-", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{"Tu", "-", "Tv"}], ")"}], "2"]}], ")"}], " ", 
RowBox[{"(", 
RowBox[{
SuperscriptBox["r", "2"], "-", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{"Tu", "+", "Tv"}], ")"}], "2"]}], ")"}]}]]}]], ",", 
FractionBox[
RowBox[{"+", 
SqrtBox[
RowBox[{
RowBox[{"(", 
RowBox[{
SuperscriptBox["r", "2"], "-", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{"Tu", "-", "Tv"}], ")"}], "2"]}], ")"}], " ", 
RowBox[{"(", 
RowBox[{
SuperscriptBox["r", "2"], "-", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{"Tu", "+", "Tv"}], ")"}], "2"]}], ")"}]}]]}], 
RowBox[{"2", " ", "r"}]]}], "}"}]}], 
RowBox[{"n", "==", "1"}]},
{"0", 
TagBox["True",
"PiecewiseDefault",
AutoDelete->True]}
},
AllowedDimensions->{2, Automatic},
Editable->True,
GridBoxAlignment->{"Columns" -> {{Left}}, "Rows" -> {{Baseline}}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "Rows" -> {{1.}}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.84]}, Offset[0.27999999999999997`]}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}},
Selectable->True]}
},
GridBoxAlignment->{"Columns" -> {{Left}}, "Rows" -> {{Baseline}}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "Rows" -> {{1.}}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.35]}, Offset[0.27999999999999997`]}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}}],
"Piecewise",
DeleteWithContents->True,
Editable->False,
SelectWithContents->True,
Selectable->False,
StripWrapperBoxes->True]\)];


(* ::Subsubsection::Closed:: *)
(*From Poincar\[EAcute] AdS*)


(* ::Input::Initialization:: *)
juPoincare=Function[{n},\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
RowBox[{"{", 
RowBox[{
RowBox[{"-", "1"}], ",", "0", ",", "0"}], "}"}], 
RowBox[{"n", "==", 
RowBox[{"-", "1"}]}]},
{
RowBox[{"{", 
RowBox[{
RowBox[{"-", "u"}], ",", "0", ",", 
RowBox[{"-", 
FractionBox["z", "2"]}]}], "}"}], 
RowBox[{"n", "==", "0"}]},
{
RowBox[{"{", 
RowBox[{
RowBox[{"-", 
SuperscriptBox["u", "2"]}], ",", 
SuperscriptBox["z", "2"], ",", 
RowBox[{
RowBox[{"-", "u"}], " ", "z"}]}], "}"}], 
RowBox[{"n", "==", "1"}]},
{"0", 
TagBox["True",
"PiecewiseDefault",
AutoDelete->True]}
},
AllowedDimensions->{2, Automatic},
Editable->True,
GridBoxAlignment->{"Columns" -> {{Left}}, "Rows" -> {{Baseline}}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "Rows" -> {{1.}}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.84]}, Offset[0.27999999999999997`]}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}},
Selectable->True]}
},
GridBoxAlignment->{"Columns" -> {{Left}}, "Rows" -> {{Baseline}}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "Rows" -> {{1.}}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.35]}, Offset[0.27999999999999997`]}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}}],
"Piecewise",
DeleteWithContents->True,
Editable->False,
SelectWithContents->True,
Selectable->False,
StripWrapperBoxes->True]\)];
jvPoincare=Function[{n},\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
RowBox[{"{", 
RowBox[{"0", ",", 
RowBox[{"-", "1"}], ",", "0"}], "}"}], 
RowBox[{"n", "==", 
RowBox[{"-", "1"}]}]},
{
RowBox[{"{", 
RowBox[{"0", ",", 
RowBox[{"-", "v"}], ",", 
RowBox[{"-", 
FractionBox["z", "2"]}]}], "}"}], 
RowBox[{"n", "==", "0"}]},
{
RowBox[{"{", 
RowBox[{
SuperscriptBox["z", "2"], ",", 
RowBox[{"-", 
SuperscriptBox["v", "2"]}], ",", 
RowBox[{
RowBox[{"-", "v"}], " ", "z"}]}], "}"}], 
RowBox[{"n", "==", "1"}]},
{"0", 
TagBox["True",
"PiecewiseDefault",
AutoDelete->True]}
},
AllowedDimensions->{2, Automatic},
Editable->True,
GridBoxAlignment->{"Columns" -> {{Left}}, "Rows" -> {{Baseline}}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "Rows" -> {{1.}}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.84]}, Offset[0.27999999999999997`]}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}},
Selectable->True]}
},
GridBoxAlignment->{"Columns" -> {{Left}}, "Rows" -> {{Baseline}}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "Rows" -> {{1.}}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.35]}, Offset[0.27999999999999997`]}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}}],
"Piecewise",
DeleteWithContents->True,
Editable->False,
SelectWithContents->True,
Selectable->False,
StripWrapperBoxes->True]\)];


(* ::Input::Initialization:: *)
<<"Physica/GRUtils.wl"


(* ::Input::Initialization:: *)
{u,v,z}/.poincare2btz;

jacobianPoincarebyBTZ=jacobianFromFunc[
Function[{u,v,r},%//Evaluate],
{u,v,r}
]//Simplify;

jacobianBTZbyPoincare=Inverse[jacobianPoincarebyBTZ]//Simplify;


(* ::Input::Initialization:: *)
juBTZs=(jacobianBTZbyPoincare.(juPoincare[#]/.poincare2btz)//Simplify)&/@Range[-1,1];
jvBTZs=(jacobianBTZbyPoincare.(jvPoincare[#]/.poincare2btz)//Simplify)&/@Range[-1,1];

juBTZs//Column


(* ::Input:: *)
(*ju/@Range[-1,1];*)
(*%//Column*)
(**)
(*ju/@Range[-1,1]==juBTZs//Simplify*)
(*jv/@Range[-1,1]==jvBTZs//Simplify*)


(* ::Input:: *)
(**)


(* ::Subsubsection::Closed:: *)
(*Basic Checks*)


(* ::Text:: *)
(*Killing vectors at the boundary:*)


(* ::Input:: *)
(*Series[ju[#][[1;;2]]&/@Range[-1,1],{r,\[Infinity],1}]*)


(* ::Input:: *)
(*Series[jv[#][[1;;2]]&/@Range[-1,1],{r,\[Infinity],1}]*)


(* ::Input:: *)
(**)


(* ::Text:: *)
(*Check Killing's equation:*)


(* ::Input:: *)
(*Lie[ju[#1],metric]&/@Range[-1,1]*)


(* ::Input:: *)
(*Lie[jv[#1],metric]&/@Range[-1,1]*)


(* ::Input:: *)
(**)


(* ::Text:: *)
(*The algebra:*)


(* ::Input:: *)
(*Subsets[{-1,0,1},{2}]*)


(* ::Input:: *)
(**)


(* ::Input:: *)
(*algju=Lie[ju[#1],ju[#2],{up}]&@@@Subsets[{-1,0,1},{2}];*)
(*Simplify[algju+{ju[-1],2ju[0],ju[1]}]*)


(* ::Input:: *)
(**)


(* ::Input:: *)
(**)


(* ::Input:: *)
(*algjv=Lie[jv[#1],jv[#2],{up}]&@@@Subsets[{-1,0,1},{2}];*)
(*Simplify[algjv+{jv[-1],2jv[0],jv[1]}]*)


(* ::Input:: *)
(**)


(* ::Input:: *)
(*Lie[ju[#1],jv[#2],{up}]&@@@Tuples[{-1,0,1},2]*)


(* ::Input:: *)
(**)


(* ::Subsubsection:: *)
(*Bulk modular flow generator*)


(* ::Input::Initialization:: *)
sameTuTv={Tu->T,Tv->T};


(* ::Input::Initialization:: *)
RTsameT=r->(2T Cosh[T L])/Sqrt[Cosh[T L]^2-Cosh[2T x]^2];


(* ::Input::Initialization:: *)
coeffs={Coth[Tu(up-um)]/(E^(-2Tu up)+E^(-2Tu um)),-Coth[Tu(up-um)],Coth[Tu(up-um)]/(E^(2Tu up)+E^(2Tu um)),Coth[Tv(vp-vm)]/(E^(-2Tv vp)+E^(-2Tv vm)),-Coth[Tv(vp-vm)],Coth[Tv(vp-vm)]/(E^(2Tv vp)+E^(2Tv vm))};


(* ::Input::Initialization:: *)
\[Xi]=2\[Pi](
 coeffs[[1;;3]].(ju/@Range[-1,1])
- coeffs[[4;;6]].(jv/@Range[-1,1])
)/.{up->xp,vp->xp,um->xm,vm->xm}/.{xp->L/2,xm->-L/2}


(* ::Item:: *)
(*Now we enforce Subscript[T, u]=Subscript[T, v]=T to simplify our calculations*)


(* ::Input::Initialization:: *)
metric=static`metric=btz`metric/.sameTuTv//Simplify
<<diffgeoM`


(* ::Input::Initialization:: *)
\[Xi]sameT=\[Xi]/.sameTuTv//ExpToTrig


(* ::Input:: *)
(*Simplify[\[Xi]sameT/.{u->x,v->x}/.RTsameT,{Cosh[2 L T]>=Cosh[4 T x]}]*)


(* ::Text:: *)
(*Check surface gravity (for same T):*)


(* ::Input:: *)
(*covD[lower[\[Xi]sameT]];*)
(*PrintTemporary["# covD: Done!"];*)
(**)
(*contract[%%**%%,{1,3},{2,4}];*)
(*PrintTemporary["# contract: Done!"];*)
(**)
(*FullSimplify[%%/.{u->x,v->x}/.RTsameT,{Cosh[2 L T]-Cosh[4 T x]>0,Cosh[L T]-Cosh[2T x]>0}]*)


(* ::Input:: *)
(**)


(* ::Text:: *)
(*Mathematica refuse to handle the Tu!=Tv case:*)


(* ::Input:: *)
(*metric=btz`metric;*)
(*<<diffgeoM`*)
(**)
(*covD[lower[\[Xi]]];*)
(*PrintTemporary["# covD: Done!"];*)
(**)
(*contract[%%**%%,{1,3},{2,4}];*)
(*PrintTemporary["# contract: Done!"];*)
(**)
(*FullSimplify[%%/.{u->x,v->x}/.RTsameT,{Cosh[2 L T]-Cosh[4 T x]>0,Cosh[L T]-Cosh[2T x]>0}]*)


(* ::Subsubsection:: *)
(*Charges*)


(* ::Item:: *)
(*To restore the original metric:*)


(* ::Input::Initialization:: *)
coord
metric=btz`metric
<<diffgeoM`

\[Delta]=(\[Delta]Tv \!\(
\*SubscriptBox[\(\[PartialD]\), \(Tv\)]#\)+\[Delta]Tu \!\(
\*SubscriptBox[\(\[PartialD]\), \(Tu\)]#\))&;


(* ::Text:: *)
(*Subscript[k, \[Mu]\[Nu]] with lower indices:*)


(* ::Input::Initialization:: *)
kg[\[Xi]_,h_]:=Module[{dh,d\[Xi],\[Xi]dh,hd\[Xi]},
dh=covD[h];
d\[Xi]=covD[\[Xi]];
\[Xi]dh=\[Xi]**dh;
hd\[Xi]=h**d\[Xi];
Identity[
-raise[antisymmetrize[
contract[\[Xi]dh,{3,4}]-contract[\[Xi]dh,{2,3}]+contract[\[Xi]dh,{1,3}]
+(1/2)contract[h,{1,2}]d\[Xi]-(1/2)contract[hd\[Xi],{2,3}]+(1/2)contract[hd\[Xi],{2,4}]
]]
]
]


(* ::Text:: *)
(*Constant t slice,  \[DifferentialD]x^\[Alpha]->\[DifferentialD]x,*)


(* ::Input:: *)
(*coord*)


(* ::Item:: *)
(*WARNING: sign experiment ongoing!*)


(* ::Input::Initialization:: *)
\[Delta]q[\[Xi]_,h_]:=1/(8\[Pi] G) Identity[rg*((kg[lower[\[Xi]],h])[[#,3]]&/@{1,2}).{1,-1}]


(* ::Input::Initialization:: *)
\[Delta]qr[\[Xi]_,h_]:=-1/(8\[Pi] G) Identity[rg*
(Function[idx,((kg[lower[\[Xi]],h])[[#,idx]]&/@{1,2}).{1,-1}]/@{1,2}).{1,+1}
]


(* ::Item:: *)
(*It seems that \[Delta]qr lacks a 1/2 factor (check!)*)


(* ::Input:: *)
(**)


(* ::Text:: *)
(*Quick check (finite Tu, Tv):*)


(* ::Input:: *)
(*Simplify[2\[Pi] \[Delta]q[#,\[Delta]@metric]&/@{\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(u\)]coord\),-\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(v\)]coord\)}]*)


(* ::Item:: *)
(*Again enforce Subscript[T, u]=Subscript[T, v]=T to simplify our calculations*)


(* ::Input::Initialization:: *)
metric=static`metric
<<diffgeoM`

\[Delta]=(\[Delta]T \!\(
\*SubscriptBox[\(\[PartialD]\), \(T\)]#\))&;


(* ::Input:: *)
(*covD[\[Xi]sameT]//ExpToTrig//FullSimplify;*)


(* ::Input::Initialization:: *)
varQ\[Xi]sameT=\[Delta]q[\[Xi]sameT,\[Delta]@metric];


(* ::Input::Initialization:: *)
varQ\[Xi]sameTsimp=varQ\[Xi]sameT//ExpToTrig//Simplify


(* ::Input:: *)
(*varQ\[Xi]sameTsimp/.{u->x,v->x}//ExpToTrig//Simplify*)
(*(\[Delta]T Coth[L T](1-r /Sqrt[r^2-4 T^2] Cosh[2 T x]/Cosh[L T]))/(2 G)==%//Simplify*)
(**)
(*%%//Quiet[Limit[#,r->\[Infinity]]]&//Simplify*)


(* ::Input:: *)
(*r/Sqrt[r^2-4 T^2]/.RTsameT//Simplify[#,{Cosh[2 L T]-Cosh[4 T x]>0}]&//Simplify*)


(* ::Input:: *)
(*varQ\[Xi]sameTsimp/.{u->x,v->x}//Quiet[Limit[#,r->\[Infinity]]]&//ExpToTrig//Simplify*)
(**)
(*\!\( *)
(*\*SubsuperscriptBox[\(\[Integral]\), \(\(-L\)/2\), \(L/2\)]\(% \[DifferentialD]x\)\)*)
(**)
(*\[Integral](%/.\[Delta]T->1)\[DifferentialD]T//FullSimplify[#,{T>0}]&*)


(* ::Input:: *)
(**)


(* ::Item:: *)
(*Integrate along the RT surface*)


(* ::Input:: *)
(*RTcutoff*)


(* ::Input:: *)
(*RTsameT*)


(* ::Input:: *)
(*r^2-4 T^2/.RTsameT//FullSimplify*)


(* ::Input:: *)
(*((r Cosh[2 T x] Csch[L T])/Sqrt[r^2-4 T^2])^2//Simplify*)
(**)
(*%/.RTsameT//Simplify*)


(* ::Input:: *)
(*varQ\[Xi]sameT/.{u->x,v->x}//ExpToTrig//Simplify*)
(**)
(*%/.RTsameT//Simplify*)
(**)
(*%//Simplify[#,{Cosh[2 L T]>Cosh[4 T x]}]&*)


(* ::Input:: *)
(*Hold[\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(x\)]r\)]/.RTsameT//ReleaseHold*)


(* ::Input:: *)
(*r/.RTsameT*)


(* ::Input:: *)
(*\[Delta]qr[\[Xi]sameT,\[Delta]@metric]/.{u->x,v->x}//ExpToTrig//Simplify*)
(**)
(*Hold[\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(x\)]r\)]/.RTsameT//ReleaseHold;*)
(**)
(*% %%/.RTsameT//Simplify*)
(**)
(*%//FullSimplify[#,{Cosh[2 L T]>Cosh[4 T x]}]&*)
(**)
(*\!\( *)
(*\*SubsuperscriptBox[\(\[Integral]\), \(\(-L\)/2\), \(L/2\)]\(% \[DifferentialD]x\)\)*)
(**)
(*\[Integral](%/.\[Delta]T->1)\[DifferentialD]T//FullSimplify[#,{T>0}]&*)


(* ::Input:: *)
(**)


(* ::Input:: *)
(*\!\( *)
(*\*SubsuperscriptBox[\(\[Integral]\), \(rc\), \(\[Infinity]\)]\( *)
(*\*FractionBox[\(2\ T\ \[Delta]T\ Csch[L\ T]\ Sinh[2\ T\ x]\), \(G\ *)
(*\*SuperscriptBox[\(( *)
(*\*SuperscriptBox[\(r\), \(2\)] - 4\ *)
(*\*SuperscriptBox[\(T\), \(2\)])\), \(3/2\)]\)] \[DifferentialD]r\)\)*)
(**)
(*Limit[%,T->0]*)


(* ::Input:: *)
(**)


(* ::Input:: *)
(*Log[x+Sqrt[1+x^2]]//ExpToTrig*)


(* ::Input:: *)
(**)
(**)
(**)
(**)
(**)
(**)
(**)
(**)


(* ::Input:: *)
(*ind`metric=static`metric/.RTsameT/.{x->(u+v)/2}//Simplify*)
(*\[Delta]@(ind`metric)//Simplify*)
(**)
(*ans=\[Delta]q[\[Xi]sameT,%]*)
(**)
(*ans/.{u->x,v->x}/.RTsameT*)


(* ::Input:: *)
(*ans/.{G->1,T->1,L->1}//Simplify*)


(* ::Input:: *)
(*Plot[ans/.\[Delta]T->1/.{G->1,T->.8,L->1},{x,-1,1}]*)


(* ::Subsubsection:: *)
(*Finite cutoff*)


(* ::Input:: *)
(*r/.RTsameT/.L->L\[Infinity]/.x->L/2*)
(**)
(*Solve[rc==%,L\[Infinity]]*)


(* ::Input::Initialization:: *)
L\[Infinity]2Lc=(L\[Infinity]->ArcCosh[(rc Cosh[L T])/Sqrt[rc^2-4 T^2]]/T);


(* ::Input:: *)
(*z2btz*)


(* ::Input::Initialization:: *)
zc2rc=zc->z/.z2btz/.sameTuTv/.{u->x,v->x,r->rc}


(* ::Input::Initialization:: *)
rtUgly=r->(r/.RTsameT/.L->L\[Infinity]/.L\[Infinity]2Lc//Simplify)


(* ::Input::Initialization:: *)
r/.rtUgly//NumeratorDenominator//Divide[#,rc]&;

RTcutoff=r->(2 T Cosh[L T])/Sqrt[Cosh[L T]^2-(1-(4 T^2)/(rc^2) ) Cosh[2 T x]^2]

(r/.RTcutoff//NumeratorDenominator)==%%//FullSimplify[#,{rc>0}]&


(* ::Input:: *)
(**)
(**)


(* ::Input:: *)
(*poincare2btz*)


(* ::Input:: *)
(*{u,v,z}/.poincare2btz/.sameTuTv/.{u->x,v->x}//Simplify//Series[#,{T,0,1}]&*)


(* ::Input:: *)
(*r^2/.RTsameT*)


(* ::Input:: *)
(*r/.RTcutoff/.{rc->(2T)/zc}//Simplify[#,{zc>0}]&*)
(**)
(*z^2+x^2-(L/2)^2-zc^2/.First[Solve[%==(2T)/z,z]]//Series[#,{T,0,2}]&*)


(* ::Input:: *)
(*z^2+x^2-(L/2)^2-zc^2/.x->(u+v)/2/.poincare2btz/.( *)
(*z2btz/.{z->zc,r->rc}*)
(*)/.sameTuTv/.{u->x,v->x}//Simplify//Series[#,{T,0,2}]&*)
(**)


(* ::Input:: *)
(*r/.RTcutoff//Series[#,{T,0,2}]&*)


(* ::Input:: *)
(**)


(* ::Input:: *)
(*L\[Infinity]/2/.L\[Infinity]2Lc;*)
(*Series[%,{T,0,2}]//Simplify*)


(* ::Input:: *)
(*Series[Sqrt[(L/2)^2+(zc/.zc2rc)^2],{T,0,2}]*)


(* ::Input:: *)
(*Plot[{L\[Infinity]/2/.L\[Infinity]2Lc,Sqrt[(L/2)^2+((2T)/rc)^2]}/.{T->1,x->0,rc->4}//Evaluate,{L,0,100}]*)


(* ::Input:: *)
(*Plot[{L\[Infinity]/2/.L\[Infinity]2Lc,Sqrt[(L/2)^2+((2T)/rc)^2]}/.{T->1,x->0}/.L->1//Evaluate,{rc,0,100},PlotRange->Automatic]*)


(* ::Subsubsection:: *)
(*Cutoff charges*)


(* ::Input:: *)
(*$Assumptions*)


(* ::Input:: *)
(*varQ\[Xi]sameTsimp/.{u->x,v->x}/.L->L\[Infinity]//ExpToTrig//FullSimplify*)


(* ::Input:: *)
(*(Sinh[T(x+L/2)]Sinh[T(x-L/2)])/Sinh[2T L/2]//TrigReduce//FullSimplify*)


(* ::Input:: *)
(*\[Xi]sameT/.{u->x,v->x}*)
(**)
(*1/2 (%[[1]]-%[[2]])//Simplify*)


(* ::Input:: *)
(*Limit[(\[Pi] (Coth[L T]-(r Cosh[2 T x] Csch[L T])/Sqrt[r^2-4 T^2]))/T,r->\[Infinity]]//FullSimplify*)


(* ::Item:: *)
(*This is the charge:*)


(* ::Input:: *)
(*varQ\[Xi]sameTsimp/.{u->x,v->x}/.L->L\[Infinity]/.r->rc//ExpToTrig//FullSimplify*)
(**)
(*\!\( *)
(*\*SubsuperscriptBox[\(\[Integral]\), \(\(-L\)/2\), \(L/2\)]\(% \[DifferentialD]x\)\)*)
(**)
(*%/.L\[Infinity]2Lc//FullSimplify*)
(**)
(*(*\[Integral](%/.\[Delta]T\[Rule]1)\[DifferentialD]T*)*)


(* ::Input:: *)
(**)


(* ::Input:: *)
(*(rc \[Delta]T (L T Cosh[L T]-Sinh[L T]))/(2 G T Sqrt[-rc^2+4 T^2+rc^2 Cosh[L T]^2])*)
(**)
(*L \!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(L\)]%\)//Simplify*)
(**)
(*\[Integral](%/.\[Delta]T->1)\[DifferentialD]T*)


(* ::Input:: *)
(**)


(* ::Item:: *)
(*What if we send rc->\[Infinity] while keeping L,Subscript[L, \[Infinity]] fixed?*)


(* ::Input:: *)
(*\!\( *)
(*\*SubsuperscriptBox[\(\[Integral]\), \(\(-L\)/2\), \(L/2\)]\( *)
(*\*FractionBox[\(\[Delta]T\ \((Coth[L\[Infinity]\ T] - Cosh[2\ T\ x]\ Csch[L\[Infinity]\ T])\)\), \(2\ G\)] \[DifferentialD]x\)\)*)
(**)
(*%/.L\[Infinity]2Lc//FullSimplify*)
(**)
(*L \!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(L\)]%\)//Simplify*)
(**)
(*\[Integral](%/.\[Delta]T->1)\[DifferentialD]T*)


(* ::Item:: *)
(*The difference:*)


(* ::Input:: *)
(*(\[Delta]T (L rc T Cosh[L T]-Sqrt[rc^2-4 T^2] Sinh[L T]))/(2 G T Sqrt[-rc^2+4 T^2+rc^2 Cosh[L T]^2])-(rc \[Delta]T (L T Cosh[L T]-Sinh[L T]))/(2 G T Sqrt[-rc^2+4 T^2+rc^2 Cosh[L T]^2])//Simplify*)


(* ::Input:: *)
(*((-1+rc/Sqrt[rc^2-4 T^2]) \[Delta]T Csch[L T] Sinh[2 T x])/(2 G T)/.L->L\[Infinity]/.x->L/2/.L\[Infinity]2Lc//Simplify*)


(* ::Input:: *)
(**)
(**)
(**)


(* ::Item:: *)
(*Compare with PengXiang's flow:*)


(* ::Input:: *)
(*(rc \[Delta]T (L T  Cosh[L T]-Sinh[L T] Sqrt[rc^2-4 T^2]/rc))/(2 G T Sqrt[-rc^2+4 T^2+rc^2 Cosh[L T]^2])*)


(* ::Input:: *)
(**)
(**)
(**)


(* ::Input:: *)
(*L \!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(L\)]*)
(*\*FractionBox[\(rc\ \[Delta]T\ \((L\ T\ \ Cosh[L\ T] - Sinh[L\ T] *)
(*\*FractionBox[*)
(*SqrtBox[\( *)
(*\*SuperscriptBox[\(rc\), \(2\)] - 4\ *)
(*\*SuperscriptBox[\(T\), \(2\)]\)], \(rc\)])\)\), \(2\ G\ T *)
(*\*SqrtBox[\(\(-*)
(*\*SuperscriptBox[\(rc\), \(2\)]\) + 4\ *)
(*\*SuperscriptBox[\(T\), \(2\)] + *)
(*\*SuperscriptBox[\(rc\), \(2\)]\ *)
(*\*SuperscriptBox[\(Cosh[L\ T]\), \(2\)]\)]\)]\)//Simplify*)
(**)
(*\[Integral](%/.\[Delta]T->1)\[DifferentialD]T*)


(* ::Input:: *)
(**)
(**)
(**)
(**)


(* ::Input:: *)
(*-rc^2+rc^2 Cosh[L T]^2//Simplify*)


(* ::Input:: *)
(*1/(2 G) (L Coth[L T]-1/T)/Sqrt[1+(4T^2)/(rc^2 Sinh[L T]^2)]==*)


(* ::Input:: *)
(*1/(2 G) \[Integral](L Coth[L T]-1/T)/Sqrt[1+(4T^2)/(rc^2 Sinh[L T]^2)]\[DifferentialD]T*)


(* ::Input:: *)
(*\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(L\)]\(( *)
(*\*FractionBox[\(1\), \(2\ G\)] \((L\ Coth[L\ T] - *)
(*\*FractionBox[\(1\), \(T\)])\)/*)
(*\*SqrtBox[\(1 + *)
(*\*FractionBox[\(4 *)
(*\*SuperscriptBox[\(T\), \(2\)]\), \( *)
(*\*SuperscriptBox[\(rc\), \(2\)] *)
(*\*SuperscriptBox[\(Sinh[L\ T]\), \(2\)]\)]\)])\)\)//FullSimplify*)


(* ::Input:: *)
(*\[Integral](rc (-2 L rc^2 T+8 L T^3+rc^2 Sinh[2 L T]))/(2 G (-rc^2+8 T^2+rc^2 Cosh[2 L T]) Sqrt[rc^2+4 T^2 Csch[L T]^2]) \[DifferentialD]T*)


(* ::Input:: *)
(*\[Integral](T Coth[L T])/(2 G Sqrt[1+((4 T^2)/(rc^2 Sinh[L T]^2)) ]) \[DifferentialD]L//FullSimplify*)


(* ::Input:: *)
(*(4T^2)/(Sqrt[rc^2 Sinh[L T]^2+4 (T^2) ]-rc Sinh[L T])^2//ExpandDenominator//Simplify*)


(* ::Input:: *)
(*\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(L\)]\(Log[*)
(*\*FractionBox[\( *)
(*\*SqrtBox[\(\(1\)\(+\)*)
(*\*FractionBox[\(4\ *)
(*\*SuperscriptBox[\(T\), \(2\)]\), \( *)
(*\*SuperscriptBox[\(rc\), \(2\)] *)
(*\*SuperscriptBox[\(Sinh[L\ T]\), \(2\)]\)]\(\ \)\)] + 1\), \( *)
(*\*SqrtBox[\(\(1\)\(+\)*)
(*\*FractionBox[\(4\ *)
(*\*SuperscriptBox[\(T\), \(2\)]\), \( *)
(*\*SuperscriptBox[\(rc\), \(2\)] *)
(*\*SuperscriptBox[\(Sinh[L\ T]\), \(2\)]\)]\(\ \)\)] - 1\)]]\)\)//Simplify*)


(* ::Input:: *)
(*ArcTanh[x]//TrigToExp*)


(* ::Input:: *)
(*Log[(x+1)/(x-1)]//FullSimplify*)


(* ::Input:: *)
(**)


(* ::Input:: *)
(*Plot[r/.(RTsameT/.{L->1,T->1})//Evaluate,{x,-1,1},PlotRange->{0,10}]*)


(* ::Input:: *)
(**)


(* ::Input:: *)
(*r/.RTsameT*)
(**)
(*Sqrt[(g[[1,1]]+g[[2,2]]+2g[[1,2]])+g[[3,3]](\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(x\)]%\))^2/.RTsameT]//Simplify[#,Cosh[2 L T]-Cosh[4 T x]>0]&*)
(**)
(*\[Integral]% \[DifferentialD]x//Simplify*)
(**)


(* ::Input:: *)
(*(1/2 Log[Sinh[T (L+2 x)]/Sinh[T (L-2 x)]]/.x->#)&/@{(L-\[Epsilon])/2,-((L-\[Epsilon])/2)}*)
(**)
(*(#1-#2&)@@%//Series[#,{\[Epsilon],0,0}]&//FullSimplify[#,{\[Epsilon]>0}]&*)


(* ::Input:: *)
(*\[Xi]sameT/.{u->x,v->x}/.RTsameT//Simplify[#,{Cosh[2 L T]>Cosh[4 T x]}]&*)


(* ::Input:: *)
(**)
(**)


(* ::Input:: *)
(*Limit[\[Xi]sameT/.{L->l},r->\[Infinity]]*)


(* ::Input:: *)
(*\[Xi]sameT*)


(* ::Input:: *)
(*(*\[Delta]q[\[Xi]sameT/.{L\[Rule]l},\[Delta]@metric]/.{u\[Rule]x,v\[Rule]x}//ExpToTrig//Simplify*)*)
(**)
(*\[Delta]q[\[Xi]sameT/.{L->L\[Infinity],r->rcc},\[Delta]@metric]/.{u->x,v->x}//ExpToTrig//Simplify//Limit[#,r->\[Infinity]]&//Limit[#,rcc->\[Infinity]]&*)
(**)
(*(*Limit[%,rc\[Rule]\[Infinity]]*)*)
(**)
(*\!\( *)
(*\*SubsuperscriptBox[\(\[Integral]\), \(\(-L\)/2\), \(L/2\)]\(\((% /. L\[Infinity]2Lc)\) \[DifferentialD]x\)\)*)
(**)
(*L \!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(L\)]%\)//FullSimplify*)
(**)
(*\[Integral](%/.\[Delta]T->1)\[DifferentialD]T//FullSimplify*)


(* ::Input:: *)
(*L \!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(L\)]\(Log[Sinh[L\ T]]\)\)*)


(* ::Input:: *)
(*(TrigReduce[ArcCoth[1/x]]==ArcTanh[x])*)


(* ::Input:: *)
(*ArcCoth[rc/Sqrt[rc^2-4 T^2]]/(2 G)//TrigToExp//Simplify*)


(* ::Input:: *)
(*ArcCoth[rc/Sqrt[rc^2-4 T^2]]/(2 G)//Series[#,{T,0,1}]&//Simplify*)


(* ::Input:: *)
(**)
(**)
(**)
(**)


(* ::Input:: *)
(*L \!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(L\)]\(Log[Sinh[L\ T]]\)\)*)


(* ::Input:: *)
(*L \!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(L\)]\(ArcCosh[*)
(*\*FractionBox[\(L\), \(L0\)]]\)\)*)


(* ::Input:: *)
(*L \!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(L\)]\(ArcTanh[*)
(*\*FractionBox[\(1\), *)
(*SqrtBox[\(1 + *)
(*\*FractionBox[\(4 *)
(*\*SuperscriptBox[\(T\), \(2\)]\), \( *)
(*\*SuperscriptBox[\(rc\), \(2\)] *)
(*\*SuperscriptBox[\(Sinh[L\ T]\), \(2\)]\)]\)]]]\)\)//Simplify*)


(* ::Input:: *)
(*L \!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(L\)]\(ArcSinh[\(( *)
(*\*FractionBox[\(L\), \(2/rc\)])\)]\)\)//Simplify*)


(* ::Input:: *)
(*L \!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(L\)]\(ArcTanh[*)
(*\*FractionBox[\(1\), *)
(*SqrtBox[\(1 + *)
(*\*FractionBox[\(4\), \( *)
(*\*SuperscriptBox[\(rc\), \(2\)] *)
(*\*SuperscriptBox[\(L\), \(2\)]\)]\)]]]\)\)//Simplify*)


(* ::Input:: *)
(*ArcSinh[(L/(2/rc))]//TrigToExp//Simplify*)
(*ArcTanh[1/Sqrt[1+4/(rc^2 L^2)]]//TrigToExp//Simplify*)


(* ::Input:: *)
(*-L \!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(L\)]\(Log[\(-L\)\ rc + *)
(*\*SqrtBox[\(4 + *)
(*\*SuperscriptBox[\(L\), \(2\)]\ *)
(*\*SuperscriptBox[\(rc\), \(2\)]\)]]\)\)//Simplify*)


(* ::Input:: *)
(*Tanh[ArcSinh[L]]//TrigToExp*)


(* ::Input:: *)
(*Plot[{ArcTanh[1/Sqrt[1+1/L^2]],ArcSinh[L]+.1},{L,0,20},PlotRange->All]*)


(* ::Input:: *)
(*(L rc+Sqrt[4+L^2 rc^2])/(-L rc+Sqrt[4+L^2 rc^2])//NumeratorDenominator*)
(**)
(*%(-L rc-Sqrt[4+L^2 rc^2])//Simplify//Expand//Simplify*)
(**)
(*#1/#2&@@%//Simplify*)


(* ::Input:: *)
(**)


(* ::Input::Initialization:: *)
<<"Physica/MathUtils.wl";
saveScript[];
