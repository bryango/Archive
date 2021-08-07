#!/usr/bin/env wolframscript
(* ::Package:: *)

(* ::Input::Initialization:: *)
SetDirectory[NotebookDirectory[]]


(* ::Subsubsection::Closed:: *)
(*Some Mathematica tricks*)


(* ::Input::Initialization:: *)
$Post=.;
$Post=(Beep[];(*Speak["Wow"];*)#)&;


(* ::Input::Initialization:: *)
ClearAll[timedSimplify];
timedSimplify[expr_,assumpt_:{},timeout_:{1,30}]:=Quiet[
Simplify[expr,assumpt,TimeConstraint->timeout]
,{Simplify::time,Simplify::gtime}
]


(* ::Section:: *)
(*Ba\[NTilde]ados Entropy*)


(* ::Subsection::Closed:: *)
(*Setup*)


(* ::Item:: *)
(*Both the Schwarzschild and the Ba\[NTilde]ados radial coordinates are denoted with by r.*)


(* ::Item:: *)
(*We can convert the Schwarzschild r into the Ba\[NTilde]ados r with the following commands:*)


(* ::Input:: *)
(*{karlToBanados,banadosToKarl}*)


(* ::Item:: *)
(*Endpoints of the interval at \[Infinity] are denoted as \!\(\*SubscriptBox[\((u, v)\), \(\[PlusMinus]\)]\)*)


(* ::Item:: *)
(*Lengths of the interval are denoted with:*)


(* ::Subitem:: *)
(*lu,lv,L\[Infinity],L at \[Infinity]*)


(* ::Subitem:: *)
(*luc,lvc,lc,l at finite cutoff*)


(* ::Input::Initialization:: *)
f=Function[r,((r^2-(Tu+Tv)^2)(r^2-(Tu-Tv)^2))/r^2];
f[r]==r^2-2(Tu^2+Tv^2)+(Tu^2-Tv^2)^2/r^2//Simplify


(* ::Input::Initialization:: *)
$Assumptions=And[
G>0,
u\[Element]Reals,v\[Element]Reals,x\[Element]Reals,z>0,
Tu>0,Tv>0,T>0,
r>Tu+Tv>0,rc>Tu+Tv>0,r>2T,rc>2T,
lu>luc>0,lv>lvc>0,L>lc>0,
-(luc/2)<u<luc/2,-(lvc/2)<v<lvc/2,
f[r]>0,f[rc]>0,
rc^4+(Tu^2-Tv^2)^2-2 rc^2 (Tu^2+Tv^2)>0
];


(* ::Input::Initialization:: *)
params={Tu,Tv,T,lu,lv,L,rc};
constParams=(Dt[#]->0&)/@params


(* ::Input::Initialization:: *)
\[Delta]=(\[Delta]Tv \!\(
\*SubscriptBox[\(\[PartialD]\), \(Tv\)]#\)+\[Delta]Tu \!\(
\*SubscriptBox[\(\[PartialD]\), \(Tu\)]#\))&;


(* ::Item:: *)
(*Special cases:*)


(* ::Input::Initialization:: *)
sameT={Tu->T,Tv->T,\[Delta]Tu->\[Delta]T,\[Delta]Tv->\[Delta]T};
sameL={lu->L,lv->L};
sameLc={lvc->lc,luc->lc};
constTime={u->x,v->x};
atCutoff={u->luc/2,v->lvc/2};


(* ::Input::Initialization:: *)
coord={u,v,r};
metricSign=-1;
lengthToMetric[quadraticForm_,coord_:coord]:=Table[
1/2 D[quadraticForm,Dt[i],Dt[j]],
{i,coord},{j,coord}
]


(* ::Subsection::Closed:: *)
(*Metric*)


(* ::Input::Initialization:: *)
poincareLength=(Dt[u]Dt[v]+Dt[z]^2)/z^2;


(* ::Input::Initialization:: *)
banadosLength=Dt[r]^2/r^2+(r Dt[u]+Tv^2/r Dt[v])(r Dt[v]+Tu^2/r Dt[u]);


(* ::Input:: *)
(*karlLength*)


(* ::Input:: *)
(*poincareByBanados*)


(* ::Subsubsection::Closed:: *)
(*Schwarzschild (Karl) from Poincar\[EAcute]*)


(* ::Input::Initialization:: *)
uPoincareByKarl=u->Sqrt[(r^2-(Tu+Tv)^2)/(r^2-(Tu-Tv)^2)] E^(2u*Tu);
vPoincareByKarl=(uPoincareByKarl/.u->v/.{Tu->Tv,Tv->Tu})//Simplify
zPoincareByKarl=z->Sqrt[((Tu+Tv)^2-(Tu-Tv)^2)/(r^2-(Tu-Tv)^2)]Exp[(u+v)/2 (Tu+Tv)+(u-v)/2 (Tu-Tv)]//Simplify

poincareByKarl={uPoincareByKarl,vPoincareByKarl,zPoincareByKarl};

karlLength=poincareLength/.poincareByKarl/.(Dt[#]->0&/@{Tu,Tv})//Simplify


(* ::Input:: *)
(*r^2 f[r]==r^4+(Tu^2-Tv^2)^2-2 r^2 (Tu^2+Tv^2)//Simplify*)


(* ::Subsubsection::Closed:: *)
(*Ba\[NTilde]ados from Schwarzschild*)


(* ::Item:: *)
(*Schwarzschild to Ba\[NTilde]ados: see <https://arxiv.org/abs/hep-th/9901148> eq. (90)*)


(* ::Input::Initialization:: *)
karlToBanados=(r->\[Sqrt]((Tu+Tv)^2 (Cosh[Log[r/Sqrt[Tu Tv]]])^2-(Tu-Tv)^2 (Sinh[Log[r/Sqrt[Tu Tv]]])^2))//Simplify
karlLength/.karlToBanados/.constParams//Simplify
%-banadosLength//Simplify


(* ::Subsubsection::Closed:: *)
(*Schwarzschild from Ba\[NTilde]ados*)


(* ::Item:: *)
(*The radial cutoff:*)


(* ::Input:: *)
(*r2/.Solve[(r/.karlToBanados/.r->Sqrt[r2])==rKarl,r2]//Flatten*)
(**)
(*(%[[2]]>%[[1]])//Simplify*)


(* ::Input:: *)
(*1/2 (r^2-Tu^2-Tv^2+Sqrt[-4 Tu^2 Tv^2+(-r^2+Tu^2+Tv^2)^2])==1/4 (Sqrt[r^2-(Tu+Tv)^2]+Sqrt[r^2-(Tu-Tv)^2])^2//Simplify*)


(* ::Input::Initialization:: *)
banadosToKarl=r->1/2 (Sqrt[r^2-(Tu+Tv)^2]+Sqrt[r^2-(Tu-Tv)^2]);


(* ::Input:: *)
(*{r/.karlToBanados/.banadosToKarl,r/.banadosToKarl/.karlToBanados}//Simplify*)


(* ::Subsubsection::Closed:: *)
(*Ba\[NTilde]ados from Poincar\[EAcute]*)


(* ::Input::Initialization:: *)
poincareByBanados=poincareByKarl/.karlToBanados//Simplify


(* ::Input:: *)
(*(Dt[u]Dt[v]+Dt[z]^2)/z^2/.poincareByBanados/.constParams//Simplify*)
(*banadosLength==%//Simplify*)
(**)


(* ::Input::Initialization:: *)
banadosMetric=lengthToMetric[banadosLength,coord]


(* ::Subsubsection:: *)
(*Cutoff relations*)


(* ::Input::Initialization:: *)
\[Rho]cNice=r/.banadosToKarl/.r->rc


(* ::Input::Initialization:: *)
\[Rho]cExpanded=Sqrt[\[Rho]cNice^2//ExpandAll//Simplify]


(* ::Input::Initialization:: *)
\[Rho]c=\[Rho]cExpanded;


(* ::Subsection::Closed:: *)
(*Initialize diffgeo*)


(* ::Input::Initialization:: *)
metric=banadosMetric
<<diffgeoM`
Simplify[Einstein- metric]


(* ::Subsection::Closed:: *)
(*Killing vectors*)


(* ::Subsubsection::Closed:: *)
(*In Ba\[NTilde]ados metric*)


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
RowBox[{
RowBox[{"-", 
FractionBox["1", 
RowBox[{"2", " ", "Tu"}]]}], 
FractionBox[
RowBox[{
SuperscriptBox["r", "4"], "+", 
RowBox[{
SuperscriptBox["Tu", "2"], " ", 
SuperscriptBox["Tv", "2"]}]}], 
RowBox[{
SuperscriptBox["r", "4"], "-", 
RowBox[{
SuperscriptBox["Tu", "2"], " ", 
SuperscriptBox["Tv", "2"]}]}]]}], ",", 
RowBox[{"Tu", 
FractionBox[
SuperscriptBox["r", "2"], 
RowBox[{
SuperscriptBox["r", "4"], "-", 
RowBox[{
SuperscriptBox["Tu", "2"], " ", 
SuperscriptBox["Tv", "2"]}]}]]}], ",", 
RowBox[{
RowBox[{"-", 
FractionBox["1", "2"]}], "r"}]}], "}"}]}], 
RowBox[{"n", "==", 
RowBox[{"-", "1"}]}]},
{
RowBox[{"{", 
RowBox[{
RowBox[{"-", 
FractionBox["1", 
RowBox[{"2", " ", "Tu"}]]}], ",", "0", ",", "0"}], "}"}], 
RowBox[{"n", "==", "0"}]},
{
RowBox[{
SuperscriptBox["E", 
RowBox[{"2", " ", "Tu", " ", "u"}]], 
RowBox[{"{", 
RowBox[{
RowBox[{
RowBox[{"-", 
FractionBox["1", 
RowBox[{"2", " ", "Tu"}]]}], 
FractionBox[
RowBox[{
SuperscriptBox["r", "4"], "+", 
RowBox[{
SuperscriptBox["Tu", "2"], " ", 
SuperscriptBox["Tv", "2"]}]}], 
RowBox[{
SuperscriptBox["r", "4"], "-", 
RowBox[{
SuperscriptBox["Tu", "2"], " ", 
SuperscriptBox["Tv", "2"]}]}]]}], ",", 
RowBox[{"Tu", 
FractionBox[
SuperscriptBox["r", "2"], 
RowBox[{
SuperscriptBox["r", "4"], "-", 
RowBox[{
SuperscriptBox["Tu", "2"], " ", 
SuperscriptBox["Tv", "2"]}]}]]}], ",", 
RowBox[{
FractionBox["1", "2"], " ", "r"}]}], "}"}]}], 
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
RowBox[{"-", "2"}], " ", "Tv", " ", "v"}]], 
RowBox[{"{", 
RowBox[{
RowBox[{"Tv", 
FractionBox[
SuperscriptBox["r", "2"], 
RowBox[{
SuperscriptBox["r", "4"], "-", 
RowBox[{
SuperscriptBox["Tu", "2"], " ", 
SuperscriptBox["Tv", "2"]}]}]]}], ",", 
RowBox[{
RowBox[{"-", 
FractionBox["1", 
RowBox[{"2", " ", "Tv"}]]}], 
FractionBox[
RowBox[{
SuperscriptBox["r", "4"], "+", 
RowBox[{
SuperscriptBox["Tu", "2"], " ", 
SuperscriptBox["Tv", "2"]}]}], 
RowBox[{
SuperscriptBox["r", "4"], "-", 
RowBox[{
SuperscriptBox["Tu", "2"], " ", 
SuperscriptBox["Tv", "2"]}]}]]}], ",", 
RowBox[{
RowBox[{"-", 
FractionBox["1", "2"]}], " ", "r"}]}], "}"}]}], 
RowBox[{"n", "==", 
RowBox[{"-", "1"}]}]},
{
RowBox[{"{", 
RowBox[{"0", ",", 
RowBox[{"-", 
FractionBox["1", 
RowBox[{"2", " ", "Tv"}]]}], ",", "0"}], "}"}], 
RowBox[{"n", "==", "0"}]},
{
RowBox[{
SuperscriptBox["E", 
RowBox[{"2", " ", "Tv", " ", "v"}]], 
RowBox[{"{", 
RowBox[{
RowBox[{"Tv", 
FractionBox[
SuperscriptBox["r", "2"], 
RowBox[{
SuperscriptBox["r", "4"], "-", 
RowBox[{
SuperscriptBox["Tu", "2"], " ", 
SuperscriptBox["Tv", "2"]}]}]]}], ",", 
RowBox[{
RowBox[{"-", 
FractionBox["1", 
RowBox[{"2", " ", "Tv"}]]}], 
FractionBox[
RowBox[{
SuperscriptBox["r", "4"], "+", 
RowBox[{
SuperscriptBox["Tu", "2"], " ", 
SuperscriptBox["Tv", "2"]}]}], 
RowBox[{
SuperscriptBox["r", "4"], "-", 
RowBox[{
SuperscriptBox["Tu", "2"], " ", 
SuperscriptBox["Tv", "2"]}]}]]}], ",", 
RowBox[{
FractionBox["1", "2"], " ", "r"}]}], "}"}]}], 
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


(* ::Input:: *)
(*<<"Physica/GRUtils.wl";*)
(**)
(*{u,v,z}/.poincareByBanados;*)
(*jacobianPoincareByBanados=jacobianFromFunc[*)
(*Function[{u,v,r},%//Evaluate],*)
(*{u,v,r}*)
(*]//Simplify;*)
(**)
(*jacobianBanadosByPoincare=Inverse[jacobianPoincareByBanados]//Simplify;*)
(**)
(*juBTZs=(jacobianBanadosByPoincare.(juPoincare[#]/.poincareByBanados)//Simplify)&/@Range[-1,1];*)
(*jvBTZs=(jacobianBanadosByPoincare.(jvPoincare[#]/.poincareByBanados)//Simplify)&/@Range[-1,1];*)
(**)
(*ju/@Range[-1,1]==juBTZs//Simplify*)
(*jv/@Range[-1,1]==jvBTZs//Simplify*)


(* ::Input:: *)
(**)


(* ::Subsubsection::Closed:: *)
(*Basic checks*)


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


(* ::Subsection:: *)
(*Modular flow & fix points (geodesic / RT surface)*)


(* ::Input::Initialization:: *)
poincareEndpts={SubPlus[u]->E^(lu Tu),SubPlus[v]->E^(lv Tv),SubMinus[u]->E^(-lu Tu),SubMinus[v]->E^(-lv Tv)};

poincareFlowCoeffs={
-((SubMinus[u] SubPlus[u])/(SubMinus[u]-SubPlus[u])),(SubMinus[u]+SubPlus[u])/(SubMinus[u]-SubPlus[u]),1/(-SubMinus[u]+SubPlus[u]),
-((SubMinus[v] SubPlus[v])/(SubMinus[v]-SubPlus[v])),(SubMinus[v]+SubPlus[v])/(SubMinus[v]-SubPlus[v]),1/(-SubMinus[v]+SubPlus[v])
}/.poincareEndpts//FullSimplify


(* ::Subsubsection::Closed:: *)
(*Modular flow \[Xi]*)


(* ::Input::Initialization:: *)
\[Xi]=2\[Pi](
 poincareFlowCoeffs[[1;;3]].(ju/@Range[-1,1])
-poincareFlowCoeffs[[4;;6]].(jv/@Range[-1,1])
)//ExpToTrig//Simplify


(* ::Subsubsection:: *)
(*Geodesic: first equation*)


(* ::Input::Initialization:: *)
RTslice=Hold[Sinh[2Tu u]/Sinh[Tu lu]==Sinh[2Tv v]/Sinh[Tv lv]];

vRTu=v->1/(2Tv) ArcSinh[Sinh[Tv lv]/Sinh[Tu lu] Sinh[2Tu u]];
uRTv=u->1/(2Tu) ArcSinh[Sinh[Tu lu]/Sinh[Tv lv] Sinh[2Tv v]];


(* ::Item::Closed:: *)
(*Some checks*)


(* ::Input:: *)
(*Simplify[\[Xi][[3]]==0]*)
(*Simplify[ReleaseHold[RTslice]];*)
(*%===%%*)


(* ::Input:: *)
(*\[Xi][[3]]/.vRTu*)
(*\[Xi][[3]]/.uRTv*)


(* ::Subsubsection:: *)
(*\[DifferentialD]u<->\[DifferentialD]v*)


(* ::Item:: *)
(*Derivation*)


(* ::Input::Initialization:: *)
Dt@ReleaseHold@RTslice/.constParams
Solve[%,Dt[v]]//Flatten
dvRTdu=%[[1]];

Solve[%%%,Dt[u]]//Flatten
duRTdv=%[[1]];


(* ::Input:: *)
(*Column@{duRTdv,dvRTdu}*)


(* ::Subsubsection:: *)
(*Geodesic: same T*)


(* ::Item:: *)
(*RT surface in Schwarzschild can be found in the textbook:*)


(* ::Input::Initialization:: *)
RTsameTkarl=r->(2T Cosh[T L])/Sqrt[Cosh[T L]^2-Cosh[2T x]^2];


(* ::Item::Closed:: *)
(*Map to the Banados coordinates*)


(* ::Input:: *)
(*(r/.RTsameTkarl)^2==(r/.karlToBanados/.sameT)^2*)
(**)
(*Sqrt[r2/.Solve[%/.r->Sqrt[r2],r2]]//Simplify*)
(**)
(*(\[Xi]/.sameT/.sameL/.constTime/.r->#)&/@%//Simplify*)


(* ::Item:: *)
(*The second solution is the one.*)


(* ::Input::Initialization:: *)
RTsameT=r->T Sqrt[(Cosh[L T]+Cosh[2 T x])/(Cosh[L T]-Cosh[2 T x])];


(* ::Subsubsection::Closed:: *)
(*Geodesic: general case*)


(* ::Item:: *)
(*We use the special case above to filter out the correct solution!*)


(* ::Input:: *)
(*\[Xi]/.vRTu//FullSimplify;*)
(**)
(*(And@@Thread[%=={0,0,0}]//Simplify)/.And->List;*)
(**)
(*Solve[#/.r->Sqrt[r2]//Evaluate//Simplify,r2]&/@%//Flatten;*)
(**)
(*%/.sameT/.sameL/.constTime//Simplify*)
(**)
(*%%[[{2,4}]];*)
(**)
(*%//Column*)
(**)
(*\[Xi]/.v->1/(2Tv) ArcSinh[Sinh[Tv lv]/Sinh[Tu lu] Sinh[2Tu u]]/.r->Sqrt[r2]/.#&/@%%//Simplify*)
(**)
(*%%%[[1]]*)


(* ::Item:: *)
(*We've found the solution that is consistent with the special case!*)


(* ::Subsubsection:: *)
(*Geodesic: second equation*)


(* ::Input::Initialization:: *)
rRTuv=(r->Sqrt[Tu Tv] Sqrt[Sinh[Tu lu]/Sinh[Tv lv] (Cosh[Tv lv]+Cosh[2Tv v])/(Cosh[Tu lu]-Cosh[2Tu u])]);


(* ::Item::Closed:: *)
(*Checks*)


(* ::Input:: *)
(*\[Xi]/.rRTuv/.vRTu//Simplify*)


(* ::Input:: *)
(*r/.{{rRTuv/.sameT/.sameL/.constTime},{RTsameT}}//Simplify*)
(*%/.List->Equal*)


(* ::Subsubsection:: *)
(*Geodesic: radial parameter*)


(* ::Item::Closed:: *)
(*Solve for u,v*)


(* ::Input:: *)
(*Solve[((r/.rRTuv/.vRTu/.u->1/(2Tu) ArcCosh[x])//Simplify)==r,x]//Simplify*)
(*Solve[((r/.rRTuv/.uRTv/.v->1/(2Tv) ArcCosh[y])//Simplify)==r,y]//Simplify*)


(* ::Input::Initialization:: *)
uRTr=u->1/(2Tu) ArcCosh[1/(r^4-Tu^2 Tv^2) ((r^4+Tu^2 Tv^2) Cosh[lu Tu]-2 r^2 Tu Tv Coth[lv Tv] Sinh[lu Tu])];
vRTr=v->1/(2Tv) ArcCosh[1/(r^4-Tu^2 Tv^2) ((r^4+Tu^2 Tv^2) Cosh[lv Tv]-2 r^2 Tu Tv Coth[lu Tu] Sinh[lv Tv])];


(* ::Item::Closed:: *)
(*Checks*)


(* ::Input:: *)
(*r/.rRTuv/.uRTr/.vRTr//Simplify*)


(* ::Input:: *)
(*RTslice*)


(* ::Input:: *)
(*((((r^4+Tu^2 Tv^2) Cosh[lu Tu]-2 r^2 Tu Tv Coth[lv Tv] Sinh[lu Tu])/(r^4-Tu^2 Tv^2))^2-1)/Sinh[Tu lu]^2-((((r^4+Tu^2 Tv^2) Cosh[lv Tv]-2 r^2 Tu Tv Coth[lu Tu] Sinh[lv Tv])/(r^4-Tu^2 Tv^2))^2-1)/Sinh[Tv lv]^2//Simplify*)


(* ::Subsection::Closed:: *)
(*Some checks for the geodesic*)


(* ::Subsubsection::Closed:: *)
(*Surface gravity*)


(* ::Item:: *)
(*Now we temporarily take Subscript[T, u]=Subscript[T, v]=T and Subscript[l, u]=Subscript[l, v]=L to simplify our calculations; check surface gravity (for same T):*)


(* ::Input:: *)
(*metric=banadosMetric/.sameT//Simplify*)
(*<<diffgeoM`*)
(**)
(*covD[lower[\[Xi]/.sameT/.sameL]];*)
(*contract[%**%,{1,3},{2,4}];*)
(**)
(*Simplify[%/.RTsameT/.constTime]*)
(**)
(*metric=banadosMetric;*)
(*<<diffgeoM`*)


(* ::Input:: *)
(**)


(* ::Item:: *)
(*Mathematica refuses to handle the general case if we eliminate v or u and compute with brute force.*)


(* ::Item:: *)
(*However, it can work with the more symmetric expression rRTuv:*)


(* ::Input:: *)
(*rRTuv*)


(* ::Input:: *)
(*metric=banadosMetric;*)
(*<<diffgeoM`*)
(**)
(*covD[lower[\[Xi]]]/.rRTuv;*)
(*contract[%**%,{1,3},{2,4}]/.rRTuv;*)
(**)
(*PrintTemporary["# Done! Simplifying ..."];*)
(**)
(*Simplify[%%]*)
(**)
(*%/.vRTu//Simplify*)


(* ::Subsubsection::Closed:: *)
(*Numeric*)


(* ::Input:: *)
(*Module[{x,dx,rhs,lhs,diff,rands},*)
(*x={u,v,r}/.rRTuv/.vRTu//Simplify;*)
(*dx=lower[\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(u\)]x\)]/.rRTuv/.vRTu;*)
(**)
(*rhs=\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(u\)]x\);*)
(*lhs=\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(u\)]\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(u\)]x\)\)+contract[Christoffel**dx**dx,{2,4},{3,5}]/.rRTuv/.vRTu;*)
(*diff=lhs-lhs[[1]]rhs;*)
(**)
(*rands=RandomReal[1,4,WorkingPrecision->50];*)
(*rands=Thread[{Tu,Tv,lu,lv}->rands];*)
(**)
(*Plot[*)
(*Norm[diff/.rands,\[Infinity]],*)
(*{u,-1,1}*)
(*,WorkingPrecision->30*)
(*,PlotRange->{-10^-#,10^-#}&@200//Evaluate*)
(*,PlotStyle->Thickness[.01]*)
(*]*)
(*]*)


(* ::Subsubsection::Closed:: *)
(*Analytic*)


(* ::Item:: *)
(*We can check the geodesic equations analytically with \[Rho] parametrization:*)


(* ::Input:: *)
(*Module[{x,dx,rhs,lhs},*)
(*x={u,v,r}/.uRTr/.vRTr;*)
(*dx=lower[\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(r\)]x\)]/.uRTr/.vRTr;*)
(**)
(*rhs=\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(r\)]x\);*)
(*lhs=\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(r\)]\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(r\)]x\)\)+contract[Christoffel**dx**dx,{2,4},{3,5}]/.uRTr/.vRTr;*)
(**)
(*Thread[lhs-(lhs[[-1]]rhs)=={0,0,0}]//Simplify*)
(*]*)


(* ::Input:: *)
(*{-\[Sqrt](r^4-Tu^2 Tv^2+(r^4+Tu^2 Tv^2) Cosh[lu Tu]-2 r^2 Tu Tv Coth[lv Tv] Sinh[lu Tu]) \[Sqrt](-r^4+Tu^2 Tv^2+(r^4+Tu^2 Tv^2) Cosh[lu Tu]-2 r^2 Tu Tv Coth[lv Tv] Sinh[lu Tu])+Csch[lv Tv] Sinh[lu Tu] \[Sqrt](r^4-Tu^2 Tv^2+(r^4+Tu^2 Tv^2) Cosh[lv Tv]-2 r^2 Tu Tv Coth[lu Tu] Sinh[lv Tv]) \[Sqrt](-r^4+Tu^2 Tv^2+(r^4+Tu^2 Tv^2) Cosh[lv Tv]-2 r^2 Tu Tv Coth[lu Tu] Sinh[lv Tv])==0,*)
(*Csch[lu Tu] \[Sqrt](r^4-Tu^2 Tv^2+(r^4+Tu^2 Tv^2) Cosh[lu Tu]-2 r^2 Tu Tv Coth[lv Tv] Sinh[lu Tu]) \[Sqrt](-r^4+Tu^2 Tv^2+(r^4+Tu^2 Tv^2) Cosh[lu Tu]-2 r^2 Tu Tv Coth[lv Tv] Sinh[lu Tu]) Sinh[lv Tv]-\[Sqrt](r^4-Tu^2 Tv^2+(r^4+Tu^2 Tv^2) Cosh[lv Tv]-2 r^2 Tu Tv Coth[lu Tu] Sinh[lv Tv]) \[Sqrt](-r^4+Tu^2 Tv^2+(r^4+Tu^2 Tv^2) Cosh[lv Tv]-2 r^2 Tu Tv Coth[lu Tu] Sinh[lv Tv])==0}//Simplify*)
(**)


(* ::Input:: *)
(*{(\[Sqrt](r^4-Tu^2 Tv^2+(r^4+Tu^2 Tv^2) Cosh[lu Tu]-2 r^2 Tu Tv Coth[lv Tv] Sinh[lu Tu]) \[Sqrt](-r^4+Tu^2 Tv^2+(r^4+Tu^2 Tv^2) Cosh[lu Tu]-2 r^2 Tu Tv Coth[lv Tv] Sinh[lu Tu]))^2==(Csch[lv Tv] Sinh[lu Tu] \[Sqrt](r^4-Tu^2 Tv^2+(r^4+Tu^2 Tv^2) Cosh[lv Tv]-2 r^2 Tu Tv Coth[lu Tu] Sinh[lv Tv]) \[Sqrt](-r^4+Tu^2 Tv^2+(r^4+Tu^2 Tv^2) Cosh[lv Tv]-2 r^2 Tu Tv Coth[lu Tu] Sinh[lv Tv]))^2,(\[Sqrt](r^4-Tu^2 Tv^2+(r^4+Tu^2 Tv^2) Cosh[lv Tv]-2 r^2 Tu Tv Coth[lu Tu] Sinh[lv Tv]) \[Sqrt](-r^4+Tu^2 Tv^2+(r^4+Tu^2 Tv^2) Cosh[lv Tv]-2 r^2 Tu Tv Coth[lu Tu] Sinh[lv Tv]))^2==(Csch[lu Tu] \[Sqrt](r^4-Tu^2 Tv^2+(r^4+Tu^2 Tv^2) Cosh[lu Tu]-2 r^2 Tu Tv Coth[lv Tv] Sinh[lu Tu]) \[Sqrt](-r^4+Tu^2 Tv^2+(r^4+Tu^2 Tv^2) Cosh[lu Tu]-2 r^2 Tu Tv Coth[lv Tv] Sinh[lu Tu]) Sinh[lv Tv])^2}//Simplify*)


(* ::Subsection::Closed:: *)
(*Charges in usual AdS*)


(* ::Subsubsection::Closed:: *)
(*Setup*)


(* ::Item:: *)
(*Check that we've used the correct metric, and define the variation:*)


(* ::Input::Initialization:: *)
coord
metric===banadosMetric
<<diffgeoM`

g===metric
\[Delta]g=\[Delta]@g


(* ::Item:: *)
(*Subscript[k, \[Mu]\[Nu]] with lower indices, but the input \[Xi] is assumed to be a vector \[Xi]^\[Mu] with an upper index:*)


(* ::Input::Initialization:: *)
kg[\[Xi]vect_,h_]:=Module[{\[Xi],dh,d\[Xi],\[Xi]dh,hd\[Xi]},
\[Xi]=lower[\[Xi]vect];
dh=covD[h];
d\[Xi]=covD[\[Xi]];
\[Xi]dh=\[Xi]**dh;
hd\[Xi]=h**d\[Xi];
(-1)antisymmetrize[
+contract[\[Xi]dh,{3,4}]
-contract[\[Xi]dh,{2,3}]
+contract[\[Xi]dh,{1,3}]
+(1/2)contract[h,{1,2}]d\[Xi]
-(1/2)contract[hd\[Xi],{2,3}]
+(1/2)contract[hd\[Xi],{2,4}]
]
]


(* ::Input:: *)
(*vol===rg*)


(* ::Item:: *)
(*The output is a corank-2 tensor, to be integrated in a codim-2 surface: \[Delta]Q =\[Integral]Subscript[\[Delta]\[Chi], \[Alpha]]\[DifferentialD]x^\[Alpha].*)


(* ::Input::Initialization:: *)
\[Delta]\[Chi][\[Xi]_,h_]:=-1/(16\[Pi] G) vol contract[
Normal[LeviCivitaTensor[dim]]**kg[\[Xi],h],
{1,4},{2,5}
]


(* ::Item:: *)
(*We include an extra minus sign so that the result matches with the conventional (t,r,\[Phi]) coordinates, which has a different orientation compared to the (u,v,r)~(t,\[Phi],r) coordinates.*)


(* ::Input:: *)
(**)


(* ::Subsubsection::Closed:: *)
(*Mass & angular momentum*)


(* ::Text:: *)
(*Quick check (finite Tu, Tv):*)


(* ::Input:: *)
(*Simplify[2\[Pi] \[Delta]\[Chi][#,\[Delta]g]&/@{\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(u\)]coord\),-\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(v\)]coord\)}]*)


(* ::Text:: *)
(*Mass: \!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(t\)]\(\(=\)\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(u\)]\(-*)
(*\*SubscriptBox[\(\[PartialD]\), \(v\)]\)\)\)\),*)


(* ::Input:: *)
(*Simplify[2\[Pi] \[Delta]\[Chi][{1,-1,0},\[Delta]g].{1,1,0}]*)


(* ::Text:: *)
(*Angular momentum: \!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(\[Phi]\)]\(\(=\)\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(u\)]\(+*)
(*\*SubscriptBox[\(\[PartialD]\), \(v\)]\)\)\)\),*)


(* ::Input:: *)
(*Simplify[2\[Pi] \[Delta]\[Chi][{1,1,0},\[Delta]g].{1,1,0}]*)


(* ::Subsubsection::Closed:: *)
(*Entropy in usual AdS*)


(* ::Input::Initialization:: *)
\[Delta]\[Chi]0=\[Delta]\[Chi][\[Xi],\[Delta]g]//Simplify


(* ::Input::Initialization:: *)
\[Delta]dq0=\[Delta]\[Chi]0.{Dt[u],Dt[v],Dt[r]}/.constParams//Simplify


(* ::Item:: *)
(*If we integrate along the RT surface and convert \[DifferentialD]x^\[Alpha]->\[DifferentialD]u, then the integrand is given by:*)


(* ::Input::Initialization:: *)
\[Delta]dq0RT=\[Delta]dq0/.rRTuv/.constParams;


(* ::Input::Initialization:: *)
\[Delta]dq0RTu=\[Delta]dq0RT/.dvRTdu/.vRTu//Simplify


(* ::Item:: *)
(*Similarly, we can do  \!\(TraditionalForm\`\[DifferentialD]*)
(*\*SuperscriptBox[\(x\), \(\[Alpha]\)] -> \[DifferentialD]v\):*)


(* ::Input::Initialization:: *)
\[Delta]dq0RTv=\[Delta]dq0RT/.duRTdv/.uRTv//Simplify


(* ::Item:: *)
(*If we integrate along some constant \[Rho] line, we get:*)


(* ::Input:: *)
(*\[Delta]dq0stokesU=\[Delta]\[Chi]0.{Dt[u],Dt[v],0}/.dvRTdu/.vRTu//Simplify*)
(*(\[Delta]dq0stokesU==\[Delta]dq0RTu)//Simplify*)


(* ::Item:: *)
(*They actually agree with each other; this is the first law!*)


(* ::Item:: *)
(*We then integrate along the u direction:*)


(* ::Input:: *)
(*\[Integral]Evaluate[\[Delta]dq0RTu/.Dt[u]->1]\[DifferentialD]u//Simplify*)
(**)
(*%/.{{u->lu/2},{u->-lu/2}}*)
(**)
(*%//Apply[Subtract]//Simplify*)


(* ::Input::Initialization:: *)
\[Delta]q\[Infinity]=1/(4 G Tu Tv) (-Tv \[Delta]Tu-Tu \[Delta]Tv+lu Tu Tv \[Delta]Tu Coth[lu Tu]+lv Tu Tv \[Delta]Tv Coth[lv Tv]);


(* ::Input::Initialization:: *)
q\[Infinity]=1/(4G) (Log[Sinh[Tu lu]/(\[Epsilon] Tu)]+Log[Sinh[Tv lv]/(\[Epsilon] Tv)]);


(* ::Input:: *)
(*\[Delta]@q\[Infinity]==\[Delta]q\[Infinity]//Simplify*)


(* ::Input:: *)
(*q\[Infinity]/.sameT/.sameL*)
(**)
(*Limit[%,T->0]*)


(* ::Subsection::Closed:: *)
(*Cutoff geometry*)


(* ::Subsubsection::Closed:: *)
(*Radial coordinate*)


(* ::Input:: *)
(*Plot[r/.karlToBanados/.sameT/.T->1//Evaluate,{r,0,10},*)
(*PlotRange->{0,Automatic},*)
(*AxesLabel->{"\[Rho]","r"}*)
(*]*)


(* ::Input:: *)
(**)


(* ::Item:: *)
(*The map only works for \[Rho]>=Sqrt[Subscript[T, u] Subscript[T, v]]:*)


(* ::Input:: *)
(*Solve[(r/.karlToBanados//D[#,r]&)==0,r]*)


(* ::Input:: *)
(*r/.karlToBanados/.r->Sqrt[Tu Tv]//Simplify*)


(* ::Item:: *)
(*... which corresponds to the outer horizon of the black hole!*)


(* ::Input:: *)
(**)


(* ::Input:: *)
(*#.g.#&[Dt/@coord]/.constTime//Simplify*)


(* ::Input:: *)
(*\[Integral]1/Sqrt[(r^2+Tu^2) (r^2+Tv^2)] \[DifferentialD]r//Exp//FullSimplify*)
(*conformal[r_]=Re[%];*)
(**)
(*Plot[conformal[r]/.sameT/.T->.3,{r,-1,1}]*)


(* ::Item:: *)
(*This is a conformally flat radial coordinate!*)


(* ::Subsubsection::Closed:: *)
(*Visualizations*)


(* ::Input:: *)
(*Plot[r/.RTsameT/.{L->2,T->1}//Evaluate,{x,-\[Pi],\[Pi]},PlotRange->{0,Automatic}]*)


(* ::Input:: *)
(*cutoffGeometry=PolarPlot[*)
(*{1,(1/(1+1/Identity[#]))&/@{*)
(*r/.karlToBanados/.sameT/.r->20,*)
(*r/.RTsameT,*)
(*T*)
(*}/.{L->.25\[Pi],T->3}*)
(*}//Flatten//Evaluate*)
(*,{x,-# \[Pi],# \[Pi]}&[.2]//Evaluate*)
(*,PlotStyle->{Dotted,Dashed,Automatic,Dashed}*)
(*(*,PlotRange\[Rule]{{-#,#},{-#,#}}&[1.1]//Evaluate*)*)
(*,PlotRange->{{.55,1.05},{-.65,.65}}//Evaluate*)
(*,MaxRecursion->10*)
(*,Axes->False*)
(*]*)
(**)
(*<<"Physica/MathUtils.wl";*)
(*exportPlot[cutoffGeometry,"../img/"]*)


(* ::Item:: *)
(*This is not actually exact: one should expect the diagram to be conformally flat, which is not the case here.*)


(* ::Input:: *)
(*blackholeGeometry=PolarPlot[*)
(*{1,(1/(1+1/Abs[#]))&/@{*)
(*r/.karlToBanados/.sameT/.r->10,*)
(*r/.RTsameT,*)
(*T*)
(*}/.{L->.25\[Pi],T->1}*)
(*}//Flatten//Evaluate*)
(*,{x,-# \[Pi],# \[Pi]}&[1]//Evaluate*)
(*,PlotStyle->{Dotted,Dashed,Automatic}*)
(*,PlotRange->{{-#,#},{-#,#}}&[1.1]//Evaluate*)
(*,MaxRecursion->1000*)
(*,Axes->False*)
(*]*)
(**)
(*exportPlot[blackholeGeometry,"../img/"]*)


(* ::Subsection::Closed:: *)
(*Cutoff geodesic & lengths*)


(* ::Subsubsection:: *)
(*Cutoff coordinates from RT*)


(* ::Item:: *)
(*Cutoff with same Tand L:*)


(* ::Input:: *)
(*rc->(r/.karlToBanados/.sameT/.RTsameT//Simplify)*)
(**)
(*rc/Sqrt[rc^2-4T^2]/.%//Simplify*)


(* ::Input:: *)
(*rc^4+(Tu^2-Tv^2)^2-2 rc^2 (Tu^2+Tv^2)==rc^2 f[rc]//Simplify*)


(* ::Item::Closed:: *)
(*For the general case, again we use the special case to pick out the right solution*)


(* ::Input:: *)
(*{*)
(*(r/.karlToBanados/.RTuv)^2==rc^2,*)
(*ReleaseHold[RTslice]*)
(*}/.{*)
(*Cosh[2 Tu u]->x,*)
(*Cosh[2 Tv v]->y,*)
(*Sinh[2 Tu u]->Sqrt[x^2-1],*)
(*Sinh[2 Tv v]->Sqrt[y^2-1]*)
(*}//Simplify*)
(**)
(*Solve[%,{x,y}]//Simplify*)
(**)


(* ::Input:: *)
(*Cosh[Tu luc]->-(((4 Tu Tv Cosh[lv Tv] Sinh[lu Tu]+2 (-rc^2+Tu^2+Tv^2) Cosh[lu Tu] Sinh[lv Tv])Csch[lv Tv])/(2 Sqrt[rc^4+(Tu^2-Tv^2)^2-2 rc^2 (Tu^2+Tv^2)]))*)
(*Cosh[Tu luc]/Cosh[Tu lu]/.%//Simplify*)
(**)
(*%/.sameT/.sameL//Simplify*)


(* ::Input::Initialization:: *)
lc2l={
luc->1/Tu ArcCosh[Cosh[Tu lu] (rc^2-Tu^2-Tv^2-2 Tu Tv Tanh[lu Tu]/Tanh[lv Tv])/Sqrt[rc^2 f[rc]]],
lvc->1/Tv ArcCosh[Cosh[Tv lv] (rc^2-Tu^2-Tv^2-2 Tu Tv Tanh[lv Tv]/Tanh[lu Tu])/Sqrt[rc^2 f[rc]]],
lc->1/T ArcCosh[Cosh[L T] Sqrt[rc^2-4 T^2]/rc]
}//Simplify;


(* ::Input:: *)
(*{Cosh[Tu luc]/Cosh[Tu lu],Cosh[Tv lvc]/Cosh[Tv lv]}/.lc2l//Simplify*)


(* ::Input:: *)
(*lc2l/.sameT/.sameL/.sameLc//Simplify//Union*)


(* ::Item::Closed:: *)
(*Check that this is indeed the solution*)


(* ::Input:: *)
(*#^2&/@ReleaseHold[RTslice]/.atCutoff/.lc2l//Simplify*)


(* ::Input:: *)
(*r^2/.karlToBanados/.rRTuv/.atCutoff/.lc2l//FullSimplify*)


(* ::Item:: *)
(*This agrees with the r parametrization of the RT surface:*)


(* ::Input:: *)
(*\[Rho]c*)


(* ::Input:: *)
(*uRTr/.r->\[Rho]c//FullSimplify*)


(* ::Input::Initialization:: *)
l2lc={
lu->1/Tu ArcCosh[Cosh[luc Tu] ((rc^2-Tu^2-Tv^2)+2 Tu Tv Coth[lvc Tv] Tanh[luc Tu])/Sqrt[rc^2 f[rc]]],
lv->1/Tv ArcCosh[Cosh[lvc Tv] ((rc^2-Tu^2-Tv^2)+2 Tu Tv Coth[luc Tu] Tanh[lvc Tv])/Sqrt[rc^2 f[rc]]],
L->1/T ArcCosh[Cosh[lc T] rc/Sqrt[rc^2-4 T^2]]
}//Simplify;


(* ::Input:: *)
(*{Cosh[Tu lu]/Cosh[Tu luc],Cosh[Tv lv]/Cosh[Tv lvc]}/.l2lc//Simplify*)


(* ::Input:: *)
(*l2lc/.sameT/.sameLc/.sameL//Simplify//Union*)


(* ::Item:: *)
(*The derivation for l2lc is given in the following section.*)


(* ::Subsubsection::Closed:: *)
(*Map back to Poincar\[EAcute]*)


(* ::Input:: *)
(*poincareEndpts*)
(*poincareByBanados*)
(**)
(*{(SubPlus[u]+SubMinus[u])/2,(SubPlus[v]+SubMinus[v])/2}/.poincareEndpts//FullSimplify*)
(*{(SubPlus[u]-SubMinus[u])/2,(SubPlus[v]-SubMinus[v])/2}/.poincareEndpts//FullSimplify*)


(* ::Input:: *)
(*poincareCutoffs={u,v,z}/.poincareByBanados/.{*)
(*{u->luc/2,v->lvc/2,r->\[Rho]c},*)
(*{u->-luc/2,v->-lvc/2,r->\[Rho]c}*)
(*}//Simplify;*)
(**)
(*%//Column*)


(* ::Input:: *)
(*poincareCutoffs;*)
(*(%[[1]]-%[[2]])/2//ExpToTrig//Simplify*)
(*%[[1]]/%[[2]]*)


(* ::Item:: *)
(*Indeed they satisfy the first RT equation.*)


(* ::Item:: *)
(*What about the other one?*)


(* ::Input:: *)
(*Thread[{u,v,z}->#]&/@poincareCutoffs*)
(**)
(*(z^2+(u-um)(v-vm)==0)/.%//ExpToTrig//Simplify*)
(**)
(*Map[Numerator,%,2]*)
(**)
(*umvmSols=Solve[%,{um,vm}]//Simplify;*)


(* ::Input:: *)
(*umvmSols//Grid*)


(* ::Item:: *)
(*The two solutions correspond to the two endpoints: \!\(\*SubscriptBox[\((u, v)\), \(\[PlusMinus]\)]\)*)


(* ::Input:: *)
(*{um,vm}/.umvmSols;*)
(**)
(*(%[[1]]+%[[2]])/2//Simplify*)
(**)
(*Thread[%==Cosh[{Tu lu,Tv lv}]/.l2lc]//Simplify*)


(* ::Item:: *)
(*This is l2lc.*)


(* ::Input:: *)
(**)


(* ::Subsubsection::Closed:: *)
(*Numeric checks for Subscript[l, \[Infinity]] and Subscript[l, c]*)


(* ::Item:: *)
(*Check that they indeed compose to identity*)


(* ::Input:: *)
(*Module[{rands,checklist},*)
(*While[True,*)
(*rands=RandomReal[1,5,WorkingPrecision->50];*)
(*rands=Thread[{Tu,Tv,lu,lv,rc}->rands*{1,1,\[Pi],\[Pi],1`50}];*)
(*checklist={rc^2-2 (Tu^2+Tv^2)+(Tu^2-Tv^2)^2/rc^2,*)
(*rc^2-Tu^2-Tv^2-2 Tu Tv Coth[lv Tv] Tanh[lu Tu],*)
(*rc^2-Tu^2-Tv^2-2 Tu Tv Coth[lu Tu] Tanh[lv Tv]}/.rands;*)
(*(*PrintTemporary[Column[{rands,Column[checklist],AllTrue[checklist,Positive]}]];*)*)
(*If[AllTrue[checklist,Positive],*)
(*Print[Column[checklist]];Break[]]*)
(*];*)
(*rands*)
(*]*)
(**)
(*(({Cosh[Tu lu],Cosh[Tv lv]}/.l2lc/.lc2l)-{Cosh[Tu lu],Cosh[Tv lv]})/.%*)
(**)


(* ::Item::Closed:: *)
(*Hidden here are some datasets that break the equality, probably due to branch cuts*)


(* ::Input:: *)
(*{Tu->0.54838748089909177746831250351413570252225449134795901387331246401173453395309`50.,Tv->0.25924577269431123661543553484704380833761341502808364701905630881276361944461`50.,lu->0.57202008356324324500134392677716144320401459623399030127165549151482495844281`50.,lv->0.16382203809082586334307418334499594481465167359649915438798427720130427762396`50.,rc->0.92515433086268006134345274759458117297781215898289635790927057816723132536497`50.};*)


(* ::Input:: *)
(*{Tu->0.36573429724413338783244577267524520191330432104685997977378467807621684913146`50.,Tv->0.35488771369789040940330890543896588305318114083800294638112325010550109478202`50.,lu->0.86131607803338592678313352267355396422439829541682054258498102204050958862163`50.,lv->0.27357539661195460220750840966863354788168665573754499970619225068588834640673`50.,rc->0.66514303482207382315629836899140149741873864205958812579146648731525940006227`50.};*)


(* ::Input:: *)
(*{Tu->0.7126725911089558535744274492373678557332917666642424563967732888958426756052`50.,Tv->0.14875019629942315932948552529263719861614823757139898074276160863014340976172`50.,lu->0.57176785454127728322308231935222049857621967947153250368860453194401457105784`50.,lv->0.95873210839627354827579346258604566951267863373764074594511296864157094588103`50.,rc->0.87953826506937463923135748612470542367091233254261734184485313666362703885037`50.};*)


(* ::Subsection::Closed:: *)
(*Geodesic length*)


(* ::Input:: *)
(*lengthRT*)


(* ::Subsubsection::Closed:: *)
(*From general formula*)


(* ::Item:: *)
(*See (4.19) of https://arxiv.org/pdf/2006.10740.pdf*)


(* ::Input::Initialization:: *)
dPlus=Function[{u1,v1,r1,u2,v2,r2},
E^(Tv(v1-v2))/(8Tu Tv r1 r2) (E^(Tu (u1-u2)) (r1^2+Tu Tv)(r2^2+Tu Tv)-E^(-Tu (u1-u2)) (r1^2-Tu Tv)(r2^2-Tu Tv))
];

dMinus=Function[{u1,v1,r1,u2,v2,r2},
E^(-Tv(v1-v2))/(8Tu Tv r1 r2) (E^(-Tu (u1-u2)) (r1^2+Tu Tv)(r2^2+Tu Tv)-E^(Tu (u1-u2)) (r1^2-Tu Tv)(r2^2-Tu Tv))
];

(dPlus[u1,v1,r1,u2,v2,r2]/.{Tu->-Tu,Tv->-Tv})==dMinus[u1,v1,r1,u2,v2,r2]//Simplify


(* ::Input::Initialization:: *)
lengthFunction=Function[{u1,v1,r1,u2,v2,r2},
Evaluate[
ArcCosh[dPlus[u1,v1,r1,u2,v2,r2]+dMinus[u1,v1,r1,u2,v2,r2]]//ExpToTrig//Simplify
]
]


(* ::Input::Initialization:: *)
lengthRT=lengthFunction[luc/2,lvc/2,\[Rho]c,-luc/2,-lvc/2,\[Rho]c]//Simplify


(* ::Item:: *)
(*Check that it is indeed what we expected for same T and same L:*)


(* ::Input:: *)
(*Sinh[lengthRT/2]/.lvc->luc/.sameT//FullSimplify*)
(**)
(*2ArcSinh[%]*)
(**)
(*4G qc/.lvc->luc/.sameT//Simplify*)
(*%==%%*)


(* ::Item:: *)
(*Is it possible to simplify it further? Probably not...*)


(* ::Input:: *)
(*Sinh[lengthRT/2]//FullSimplify*)
(**)
(*simpDiff=%-(rc Sqrt[Sinh[luc Tu]Sinh[lvc Tv]])/(2 Sqrt[Tu Tv])//FullSimplify*)


(* ::Input:: *)
(*simpDiff/.{*)
(*rc->100,Tu->2,Tv->3,luc->\[Pi]/5,lvc->\[Pi]/7*)
(*}//N[#,20]&*)


(* ::Input:: *)
(**)


(* ::Subsubsection::Closed:: *)
(*Geodesic length from Poincar\[EAcute] [incorrect, incomplete]*)


(* ::Input::Initialization:: *)
z/.poincare2btz/.r->\[Rho]cHold//Simplify;
zCutoff=%/.{{u->luc/2,v->lvc/2},{u->-luc/2,v->-lvc/2}}//Simplify


(* ::Input::Initialization:: *)
uvPoincareRTz={
u->Sqrt[((lu lv)/4-z^2) lu/lv],
v->Sqrt[((lu lv)/4-z^2) lv/lu]
}/.{lu->Sinh[Tu lu],lv->Sinh[Tv lv]};


(* ::Input:: *)
(*z^2+u v/.uvPoincareRTz*)
(**)
(*Simplify[%,Csch[lv Tv] Sinh[lu Tu] (-z^2+1/4 Sinh[lu Tu] Sinh[lv Tv])>0]*)
(*Simplify[%,(-z^2+1/4 Sinh[lu Tu] Sinh[lv Tv])>0]*)


(* ::Input:: *)
(*u/v/.uvPoincareRTz//Simplify*)


(* ::Item:: *)
(*Half circle:*)


(* ::Input::Initialization:: *)
simplifyPoincare=Simplify[#,{
-z^2+1/4 Sinh[lu Tu] Sinh[lv Tv]>0,z>0,
-z1^2+1/4 Sinh[lu Tu] Sinh[lv Tv]>0,z1>0,
-z2^2+1/4 Sinh[lu Tu] Sinh[lv Tv]>0,z2>0
}]&;
(Dt[z]^2+Dt[u]Dt[v])/z^2/.uvPoincareRTz/.constParams//Simplify


Sqrt[%/.Dt[z]->1]//simplifyPoincare
\[Integral]% \[DifferentialD]z//simplifyPoincare
lengthIndef=Function[z,Evaluate[%]];


(* ::Input::Initialization:: *)
Series[lengthIndef[z],{z,Sqrt[1/4 Sinh[lu Tu] Sinh[lv Tv]],2}]//simplifyPoincare
lengthPeak=Series[%,{z,Sqrt[1/4 Sinh[lu Tu] Sinh[lv Tv]],0}]//simplifyPoincare//Normal

Series[lengthIndef[z],{z,0,2}]//simplifyPoincare


(* ::Item:: *)
(*The peak is finite, while as z->0 the indefinite integral goes to log z->-\[Infinity], therefore the length should be:*)


(* ::Input:: *)
(*lengthPeak-lengthIndef[z];*)
(**)
(*Exp[%]//Simplify*)
(**)
(*%/.{{z->z1},{z->z2}}//Apply[Times]*)
(**)
(*(*%//Simplify[#,{*)
(*1-4 z1^2 Csch[lu Tu] Csch[lv Tv]>0,*)
(*1-4 z2^2 Csch[lu Tu] Csch[lv Tv]>0*)
(*}]&*)*)
(**)
(*Cosh[Log[%]]//Simplify[#,{*)
(*1-4 z1^2 Csch[lu Tu] Csch[lv Tv]>0,*)
(*1-4 z2^2 Csch[lu Tu] Csch[lv Tv]>0*)
(*}]&*)
(**)
(*Thread[{z1,z2}->zCutoff]*)
(**)
(*%%/.%/.lc2l//Simplify[#,{*)
(*Tu Tv+\[Rho]cHold^2>0*)
(*}]&*)


(* ::Input:: *)
(*Thread[{z1,z2}->zCutoff]*)
(*Sqrt[(Tu Tv+\[Rho]cHold^2)^2 (1-4 z1^2 Csch[lu Tu] Csch[lv Tv])]/(Tu Tv+\[Rho]cHold^2)/.%//ExpandAll//ExpToTrig//Simplify[#,Tu Tv+\[Rho]cHold^2>0]&//TrigExpand*)


(* ::Input:: *)
(*Cosh[luc Tu+lvc Tv]//TrigExpand*)


(* ::Input:: *)
(**)
(**)
(*Cosh[Log[%%/.%]](*//ExpToTrig*)//Simplify[#,{*)
(*1-(16 Tu Tv \[Rho]cHold^2 Cosh[luc Tu+lvc Tv] Csch[lu Tu] Csch[lv Tv])/(Tu Tv+\[Rho]cHold^2)^2+(16 Tu Tv \[Rho]cHold^2 Csch[lu Tu] Csch[lv Tv] Sinh[luc Tu+lvc Tv])/(Tu Tv+\[Rho]cHold^2)^2>0,*)
(*1-(16 Tu Tv \[Rho]cHold^2 Csch[lu Tu] Csch[lv Tv] (Cosh[luc Tu+lvc Tv]+Sinh[luc Tu+lvc Tv]))/(Tu Tv+\[Rho]cHold^2)^2>0,*)
(*1-(16 E^(-luc Tu-lvc Tv) Tu Tv \[Rho]cHold^2 Csch[lu Tu] Csch[lv Tv])/(Tu Tv+\[Rho]cHold^2)^2>0,*)
(*1-(16 E^(luc Tu+lvc Tv) Tu Tv \[Rho]cHold^2 Csch[lu Tu] Csch[lv Tv])/(Tu Tv+\[Rho]cHold^2)^2>0*)
(*}]&*)


(* ::Input:: *)
(**)
(*%/.z->Sqrt[1/4 Sinh[lu Tu] Sinh[lv Tv]]//Simplify*)
(**)
(*lengthIndef=Function[z,Evaluate[%]];*)


(* ::Item:: *)
(*Sum of 2 halves:*)


(* ::Input:: *)
(*lengthIndef/@zCutoff*)
(**)
(*lengthRTcheck=Total[%]-2lengthIndef[Sqrt[1/4 Sinh[lu Tu] Sinh[lv Tv]]];*)
(*Cosh[lengthRTcheck]/.\[Rho]cHold->\[Rho]c//FullSimplify*)


(* ::Input:: *)
(*ArcCosh[x]//TrigToExp//Simplify*)


(* ::Input:: *)
(*ArcCosh[(-((E^(1/2 (-luc Tu-lvc Tv)) Sqrt[lu lv] (-rc^2+Tu^2-2 Tu Tv+Tv^2-Sqrt[-4 Tu^2 Tv^2+(-rc^2+Tu^2+Tv^2)^2]))/(4 Sqrt[2] Sqrt[Tu Tv (rc^2-Tu^2-Tv^2+Sqrt[-4 Tu^2 Tv^2+(-rc^2+Tu^2+Tv^2)^2])])))+\[Sqrt]((-((E^(1/2 (-luc Tu-lvc Tv)) Sqrt[lu lv] (-rc^2+Tu^2-2 Tu Tv+Tv^2-Sqrt[-4 Tu^2 Tv^2+(-rc^2+Tu^2+Tv^2)^2]))/(4 Sqrt[2] Sqrt[Tu Tv (rc^2-Tu^2-Tv^2+Sqrt[-4 Tu^2 Tv^2+(-rc^2+Tu^2+Tv^2)^2])])))^2-1)(-((E^((luc Tu)/2+(lvc Tv)/2) Sqrt[lu lv] (-rc^2+Tu^2-2 Tu Tv+Tv^2-Sqrt[-4 Tu^2 Tv^2+(-rc^2+Tu^2+Tv^2)^2]))/(4 Sqrt[2] Sqrt[Tu Tv (rc^2-Tu^2-Tv^2+Sqrt[-4 Tu^2 Tv^2+(-rc^2+Tu^2+Tv^2)^2])])))]//Simplify*)


(* ::Input:: *)
(*(lengthRT-4G qc)/.lc2l/.sameL/.sameT//ExpToTrig//Simplify[#,-rc^2+(rc^2-4 T^2) Cosh[L T]^2>0]&*)
(**)
(*%/.{*)
(*T->1,L->\[Pi]/5,rc->100000000000*)
(*}//N[#,20]&*)


(* ::Input:: *)
(*Evaluate[{lengthRT,4G qc}/.{T->1,rc->100}]*)


(* ::Input:: *)
(*Cosh[lengthRTsameLT]//Simplify*)


(* ::Input:: *)
(*lengthRTsameLT=lengthRT/.lc2l/.sameL/.sameT//ExpToTrig//Simplify[#,-rc^2+(rc^2-4 T^2) Cosh[L T]^2>0]&*)
(*lengthRefSameLT=2 ArcCosh[Sqrt[-rc^2+(rc^2-4 T^2) Cosh[L T]^2]/(2 T)]/.lc2l/.sameL/.sameT//ExpToTrig//Simplify[#,-rc^2+(rc^2-4 T^2) Cosh[L T]^2>0]&*)


(* ::Input:: *)
(*({u,v,z}/.poincare2btz//Series[#,r->\[Infinity]]&)/.sameT//Simplify*)


(* ::Input:: *)
(*Limit[rc/(2T) Sinh[T L],T->0]*)


(* ::Input:: *)
(*ArcSinh[x]//TrigToExp*)


(* ::Input:: *)
(*ArcCosh[x]//TrigToExp*)


(* ::Input:: *)
(*Plot[*)
(*Evaluate[( *)
(*{lengthRTsameLT,2ArcSinh[rc/(2T) Sinh[T L]],2Log[rc/(2T) 2 Cosh[T L]]}*)
(*)/.{T->1,rc->100}],*)
(*{L,0,1000\[Pi]}*)
(*,PlotLabels->Range[3]*)
(*,MaxRecursion->15*)
(*]*)


(* ::Input:: *)
(*ArcCosh[1/(4 Sqrt[2] T Sqrt[rc^2-2 T^2+rc Sqrt[rc^2-4 T^2]]) lu rc (rc+Sqrt[rc^2-4 T^2]) ((Cosh[lu T] (rc^2-2 T^2-2 T^2 Coth[lv T] Tanh[lu T]))/Sqrt[rc^4-4 rc^2 T^2]-Sqrt[(-rc Sqrt[rc^2-4 T^2]+(rc^2-2 T^2) Cosh[lu T]-2 T^2 Coth[lv T] Sinh[lu T])/(rc Sqrt[rc^2-4 T^2]+(rc^2-2 T^2) Cosh[lu T]-2 T^2 Coth[lv T] Sinh[lu T])] (1+(Cosh[lu T] (rc^2-2 T^2-2 T^2 Coth[lv T] Tanh[lu T]))/Sqrt[rc^4-4 rc^2 T^2]))]+ArcCosh[1/(4 Sqrt[2] T Sqrt[rc^2-2 T^2+rc Sqrt[rc^2-4 T^2]]) lu rc (rc+Sqrt[rc^2-4 T^2]) ((Cosh[lu T] (rc^2-2 T^2-2 T^2 Coth[lv T] Tanh[lu T]))/Sqrt[rc^4-4 rc^2 T^2]+Sqrt[(-rc Sqrt[rc^2-4 T^2]+(rc^2-2 T^2) Cosh[lu T]-2 T^2 Coth[lv T] Sinh[lu T])/(rc Sqrt[rc^2-4 T^2]+(rc^2-2 T^2) Cosh[lu T]-2 T^2 Coth[lv T] Sinh[lu T])] (1+(Cosh[lu T] (rc^2-2 T^2-2 T^2 Coth[lv T] Tanh[lu T]))/Sqrt[rc^4-4 rc^2 T^2]))]*)


(* ::Input:: *)
(*ArcSinh[Sqrt[(rc+Sqrt[rc^2-4 T^2])^2 (E^(2 luc T) lu^2 rc^2-16 T^2)/(2 rc^2-4 T^2+2 rc Sqrt[rc^2-4 T^2])]/(4 T)]+ArcSinh[Sqrt[(E^(-luc T) (rc+Sqrt[rc^2-4 T^2]))^2 (lu^2 rc^2-16 E^(2 luc T) T^2)/(2 rc^2-4 T^2+2 rc Sqrt[rc^2-4 T^2])]/(4 T)]//Simplify*)


(* ::Input:: *)
(*Cosh[Tu luc]/.lc2l/.lv->lu/.sameT//Simplify*)
(*Solve[Cosh[T luc]==%,lu]*)


(* ::Input:: *)
(*Sinh[ArcSinh[Sqrt[E^(-2 luc T) lu^2 rc^2-16 T^2]/(4 T)]+ArcSinh[Sqrt[E^(2 luc T) lu^2 rc^2-16 T^2]/(4 T)]]/.lu->1/T ArcCosh[(rc Cosh[luc T])/Sqrt[rc^2-4 T^2]]//TrigToExp//Simplify*)


(* ::Input:: *)
(*Sinh[ArcSinh[Sqrt[E^(-2 luc T) L^2 rc^2-16 T^2]/(4 T)]+ArcSinh[Sqrt[E^(2 luc T) L^2 rc^2-16 T^2]/(4 T)]]//TrigToExp//Simplify*)


(* ::Input:: *)
(*(rc Sinh[luc T])/(2 T)//TrigToExp//Simplify*)


(* ::Input:: *)
(*(E^(-luc T) (-1+E^(2 luc T)) rc)/(4 T)==(E^(-luc T) (-8 E^(4 luc T) L^2 rc^2 T^2+E^(luc T) L^3 rc^3 Sqrt[E^(2 luc T) L^2 rc^2-16 T^2]-8 E^(3 luc T) L rc T^2 Sqrt[E^(2 luc T) L^2 rc^2-16 T^2]-8 L rc T^2 (L rc+Sqrt[L^2 rc^2-16 E^(2 luc T) T^2])+E^(2 luc T) (L^4 rc^4+L^3 rc^3 Sqrt[L^2 rc^2-16 E^(2 luc T) T^2]+Sqrt[E^(2 luc T) L^2 rc^2-16 T^2] Sqrt[E^(-2 luc T) L^6 rc^6-16 L^4 rc^4 T^2])))/(8 T^2 (L rc+E^(luc T) Sqrt[E^(-2 luc T) L^2 rc^2-16 T^2]) (E^(luc T) L rc+Sqrt[E^(2 luc T) L^2 rc^2-16 T^2]))//Simplify*)


(* ::Input:: *)
(*2 (-1+E^(2 luc T)) rc T+(8 E^(4 luc T) L^2 rc^2 T^2-E^(luc T) L^3 rc^3 Sqrt[E^(2 luc T) L^2 rc^2-16 T^2]+8 E^(3 luc T) L rc T^2 Sqrt[E^(2 luc T) L^2 rc^2-16 T^2]+8 L rc T^2 (L rc+Sqrt[L^2 rc^2-16 E^(2 luc T) T^2])-E^(2 luc T) (L^4 rc^4+L^3 rc^3 Sqrt[L^2 rc^2-16 E^(2 luc T) T^2]+Sqrt[E^(2 luc T) L^2 rc^2-16 T^2] Sqrt[E^(-2 luc T) L^6 rc^6-16 L^4 rc^4 T^2]))==0//Simplify*)


(* ::Input:: *)
(*2 rc T (-1+4 (1+E^(4 luc T)) L^2 rc T+4 L T (E^(3 luc T) Sqrt[E^(2 luc T) L^2 rc^2-16 T^2]+Sqrt[L^2 rc^2-16 E^(2 luc T) T^2]))==E^(luc T) L^3 rc^3 Sqrt[E^(2 luc T) L^2 rc^2-16 T^2]+E^(2 luc T) (L^4 rc^4-2 rc T+L^3 rc^3 Sqrt[L^2 rc^2-16 E^(2 luc T) T^2]+Sqrt[E^(2 luc T) L^2 rc^2-16 T^2] Sqrt[E^(-2 luc T) L^6 rc^6-16 L^4 rc^4 T^2])//FullSimplify*)


(* ::Input:: *)
(*2 rc T (-1+4 L T ((1+E^(4 luc T)) L rc+E^(3 luc T) Sqrt[E^(2 luc T) L^2 rc^2-16 T^2]+Sqrt[L^2 rc^2-16 E^(2 luc T) T^2]))-E^(luc T) L^3 rc^3 Sqrt[E^(2 luc T) L^2 rc^2-16 T^2]+E^(2 luc T) (L^4 rc^4-2 rc T+L^3 rc^3 Sqrt[L^2 rc^2-16 E^(2 luc T) T^2]+Sqrt[E^(2 luc T) L^2 rc^2-16 T^2] Sqrt[E^(-2 luc T) L^6 rc^6-16 L^4 rc^4 T^2])/.{*)
(*rc->100,L->2,T->1,luc*)
(*}*)


(* ::Input:: *)
(**)


(* ::Subsection:: *)
(*Charges in cutoff AdS*)


(* ::Item:: *)
(*RT proposal for the entropy*)


(* ::Input::Initialization:: *)
qc=1/(4G) lengthRT


(* ::Input::Initialization:: *)
qc/.sameT/.sameLc//Simplify
Sinh[2G %]//FullSimplify
qcSameTL=1/(2G) ArcSinh[%]


(* ::Subsubsection::Closed:: *)
(*Summary of results*)


(* ::Item:: *)
(*I. na\[IDoubleDot]ve variation in Ba\[NTilde]ados gauge:*)


(* ::Input:: *)
(*\[Delta]g*)


(* ::Input:: *)
(*\[Delta]quNaive*)


(* ::Text:: *)
(*The result is not apparently symmetric under u<->v, but is in fact symmetric due to:*)


(* ::Input:: *)
(*RTslice*)


(* ::Item:: *)
(*However, \[Delta]quNaive is wrong even for Subscript[T, u]=Subscript[T, v]=T and Subscript[l, u]=Subscript[l, v]=Subscript[l, c].*)


(* ::Item:: *)
(*II. The next attempt is to impose Dirichlet at cutoff*)


(* ::Input:: *)
(*\[Delta]gDirichlet*)


(* ::Text:: *)
(*It differs from \[Delta]g by:*)


(* ::Input:: *)
(*\[Delta]gCorrection*)


(* ::Text:: *)
(*The correction can be constructed from from \[Delta]coord, by either coordinate transformation, or by solving a generalized Killing equation:*)


(* ::Input:: *)
(*\[Delta]gDirichlet==\[Delta]g+\[Delta]gCorrection//ReleaseHold//Simplify*)


(* ::Input::Initialization:: *)
\[Delta]quDirichlet=(Tu Tv (luc (rc^2-Tu^2-Tv^2) (rc^2 \[Delta]Tu-Tu^2 \[Delta]Tu-Tv^2 \[Delta]Tu+2 Tu Tv \[Delta]Tv)+2 lvc Tu Tv (-2 Tu Tv \[Delta]Tu+Tu^2 \[Delta]Tv+(-rc^2+Tv^2) \[Delta]Tv)) Coth[lu Tu]+Tu Tv (2 luc Tu Tv (-rc^2 \[Delta]Tu+Tu^2 \[Delta]Tu+Tv^2 \[Delta]Tu-2 Tu Tv \[Delta]Tv)+lvc (rc^2-Tu^2-Tv^2) (2 Tu Tv \[Delta]Tu-Tu^2 \[Delta]Tv+(rc^2-Tv^2) \[Delta]Tv)) Coth[lv Tv]-Sqrt[rc^2 f[rc]] (-(Tu^2-Tv^2) (-Tv \[Delta]Tu+Tu \[Delta]Tv)+rc^2 (Tv \[Delta]Tu+Tu \[Delta]Tv)) Csch[lu Tu] Sinh[luc Tu])/(4 G Tu Tv (rc^2 f[rc]));


(* ::Input:: *)
(*\[Delta]quDirichlet==\[Delta]quNaive+\[Delta]quC//Simplify*)


(* ::Item:: *)
(*We had expected the Dirichlet boundary condition to always work, as it is the setup for Guica, Monten, Kraus et al.*)


(* ::Item:: *)
(*However, it seems that this only works for Subscript[l, u]=Subscript[l, v]=Subscript[l, c]!*)


(* ::Item:: *)
(*III. Variation from Schwarzschild *)


(* ::Input:: *)
(*\[Delta]gKarl*)


(* ::Input:: *)
(*\[Delta]quKarl*)


(* ::Item:: *)
(*This works! But we don't understand how it works. Clearly it does not respect the boundary condition at Subscript[r, c].*)


(* ::Item:: *)
(*IV. Accounting for the fluctuating cutoff: very complicated expression, doesn't seem right!*)


(* ::Input:: *)
(*\[Delta]gRadial*)


(* ::Subsubsection::Closed:: *)
(*I. Variation in Ba\[NTilde]ados gauge (na\[IDoubleDot]ve)*)


(* ::Input:: *)
(*\[Delta]dq0*)


(* ::Item::Closed:: *)
(*Try to integrate along r: failed*)


(* ::Item:: *)
(*Here we use \[FilledCircle] to visually pick out \[DifferentialD]r:*)


(* ::Input:: *)
(*\[Delta]dq/.uRTr/.vRTr/.constParams/.Dt->\[FilledCircle]//Collect[#,\[FilledCircle][r]]&;*)
(**)
(*(%/\[FilledCircle][r])*)


(* ::Item:: *)
(*Integrate along u or v:*)


(* ::Input:: *)
(*atCutoff*)


(* ::Input:: *)
(*\[Integral]Evaluate[\[Delta]dq0RTu/.Dt[u]->1]\[DifferentialD]u//Simplify;*)
(*%/.{{u->luc/2},{u->-luc/2}};*)
(*%//Apply[Subtract]//Simplify*)
(**)
(*(uRTv/.atCutoff)//Map[2#&]*)
(*ArcSinh[Csch[lu Tu] Sinh[luc Tu] Sinh[lv Tv]]/.%//Simplify*)


(* ::Input:: *)
(*\[Integral]Evaluate[\[Delta]dq0RTv/.Dt[v]->1]\[DifferentialD]v//Simplify;*)
(*%/.{{v->lvc/2},{v->-lvc/2}};*)
(*%//Apply[Subtract]//Simplify*)
(**)
(*(vRTu/.atCutoff)//Map[2#&]*)
(*ArcSinh[Csch[lv Tv] Sinh[lu Tu] Sinh[lvc Tv]]/.%//Simplify*)


(* ::Input::Initialization:: *)
\[Delta]quNaive=1/(4 G) (luc \[Delta]Tu Coth[lu Tu]+lvc \[Delta]Tv Coth[lv Tv]-((Tv \[Delta]Tu+Tu \[Delta]Tv)/(Tu Tv)) Sinh[Tu luc]/Sinh[Tu lu]);


(* ::Item:: *)
(*Some u,v symmetrization attempts:*)


(* ::Input:: *)
(*\[Delta]qNaive=1/(4 G) (luc \[Delta]Tu Coth[lu Tu]+lvc \[Delta]Tv Coth[lv Tv]-((Tv \[Delta]Tu+Tu \[Delta]Tv)/(Tu Tv)) (Sinh[Tu luc]/Sinh[Tu lu]+Sinh[Tv lvc]/Sinh[Tv lv])/2);*)


(* ::Input:: *)
(*\[Delta]qNaive=1/(4 G) (luc \[Delta]Tu Coth[lu Tu]+lvc \[Delta]Tv Coth[lv Tv]-((Tv \[Delta]Tu+Tu \[Delta]Tv)/(Tu Tv))Sqrt[Sinh[Tu luc]/Sinh[Tu lu] Sinh[Tv lvc]/Sinh[Tv lv]]);*)


(* ::Input:: *)
(*(\[Delta]qNaive/.{luc->lu,lvc->lv})==\[Delta]q\[Infinity]//Simplify*)


(* ::Item:: *)
(*Note that here Subscript[l, u] depends on Subscript[T, u,v] implicitly!*)


(* ::Subsubsection::Closed:: *)
(*Same T check: holding \!\(\*OverscriptBox[\(l\), \(~\)]\)=Subscript[l, \[Infinity]]=L fixed - failed*)


(* ::Input:: *)
(*qcSameTL/.lc2l/.sameL/.sameT//Simplify*)


(* ::Input::Initialization:: *)
qcSameTLsimp=ArcSinh[Sqrt[-rc^2+(rc^2-4 T^2) Cosh[L T]^2]/(2 T)]/(2 G);


(* ::Input:: *)
(*\[Delta]T \!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(T\)]qcSameTLsimp\)//Simplify*)
(*\[Delta]qNaive/.lc2l/.sameL/.sameT/.{\[Delta]Tu->\[Delta]T,\[Delta]Tv->\[Delta]T}//Simplify*)
(**)
(*(%-%%)/(\[Delta]T/(2G T))//Simplify*)
(**)
(*%/.{*)
(*T->1,L->\[Pi]/5,rc->10*)
(*}//N[#,50]&*)


(* ::Item:: *)
(*Not working!*)


(* ::Subsubsection::Closed:: *)
(*Same T check: cutoff Subscript[l, u]=Subscript[l, v]=luc fixed - failed*)


(* ::Item:: *)
(*Now let's try to hold the cutoff length Subscript[l, u]=Subscript[l, v]=luc fixed:*)


(* ::Input:: *)
(*(\[Delta]qNaive/.sameLc/.sameL/.sameT)/(1/(2G) \[Delta]T/T)//Simplify*)
(**)
(*%/.lc2l/.sameL/.sameT//FullSimplify*)


(* ::Input:: *)
(*(\[Delta]@qc/.sameLc/.sameT)/(1/(2G) \[Delta]T/T)//Simplify*)
(**)
(*%/.lc2l/.sameL/.sameT//FullSimplify*)


(* ::Input:: *)
(**)


(* ::Input:: *)
(*(-rc^2+(rc^2-4 T^2) Cosh[L T]^2)/rc^2==-(rc^2/(rc^2-4 T^2))+Cosh[L T]^2//Simplify*)
(**)
(*%/.l2lc*)
(*%//Simplify*)


(* ::Item:: *)
(*That's not right!*)


(* ::Item:: *)
(*Not the same! It seems that one needs to*)


(* ::Subitem:: *)
(*keep track of the change of cutoff Subscript[\[Rho], c], or*)


(* ::Subitem:: *)
(*switch to a better coord*)


(* ::Subsubsection::Closed:: *)
(*II. Dirichlet at cutoff*)


(* ::Item:: *)
(*Induced metric form at Subscript[\[Rho], c]*)


(* ::Input:: *)
(*#.g.#&[Dt/@coord]/.Dt[r]->0/.r->\[Rho]c//Simplify*)


(* ::Item:: *)
(*Normalize the cutoff metric:*)


(* ::Item:: *)
(*Experiment: use a \[Rho] dependent transformation:*)


(* ::Input:: *)
(*uvNewByOld={*)
(*u->(u+v)/2+(Tu^2-Tv^2+Sqrt[r^2 f[r]])/r^2 (u-v)/2,*)
(*v->(u+v)/2+(Tu^2-Tv^2-Sqrt[r^2 f[r]])/r^2 (u-v)/2*)
(*}/.karlToBanados//Simplify*)


(* ::Item:: *)
(*Conventional: \[Rho] independent:*)


(* ::Input::Initialization:: *)
uvNewByOld={
u->(u+v)/2+(Tu^2-Tv^2+Sqrt[rc^2 Hold[f][rc]])/rc^2 (u-v)/2,
v->(u+v)/2+(Tu^2-Tv^2-Sqrt[rc^2 Hold[f][rc]])/rc^2 (u-v)/2
};

uvOldByNew=Solve[{
U==u,V==v
}/.uvNewByOld//Evaluate,
{u,v}]/.{U->u,V->v}//Flatten//Simplify

uvNewByOld=uvNewByOld//ReleaseHold//ExpandAll//Simplify
uvOldByNew=uvOldByNew//ReleaseHold//Simplify//ExpandAll//Simplify


(* ::Item::Closed:: *)
(*Ba\[NTilde]ados with rescaled u,v and unchanged \[Rho]: not good!*)


(* ::Input:: *)
(*metricFormRescaled=#.g.#&[Dt/@coord]/.uvOldByNew/.constParams/.Dt[rc]->0//Simplify*)
(**)
(*%/.r->\[Rho]c/.constParams/.Dt[rc]->0//Simplify*)
(**)
(*Coefficient[metricFormRescaled,Dt[r]^2]*)
(*Coefficient[metricFormRescaled,Dt[u]^2]//Simplify*)
(*Coefficient[metricFormRescaled,Dt[u]Dt[v]]//Simplify*)


(* ::Input:: *)
(*\[Delta][metricFormRescaled]/.uvNewByOld/.constParams/.Dt[rc]->0//Simplify*)
(*lengthToMetric[%]//Simplify[#,{Hold[f][rc]>0}]&*)
(**)
(*#.(%/.r->\[Rho]c).#&[Dt/@coord]//Simplify*)
(*Coefficient[%,Dt[u]Dt[v]]//Simplify*)


(* ::Item:: *)
(*Note that [\[Delta]] and [restriction to the boundary] do not commute!*)


(* ::Subitem:: *)
(*This is not unexpected, since Subscript[\[Rho], c] moves as we vary Subscript[T, u,v]*)


(* ::Subitem:: *)
(*\[Rho] should be rescaled as well to cancel this!*)


(* ::Item:: *)
(*Rescale \[Rho] linearly such that the new Subscript[\[Rho], c] is just Subscript[r, c]*)


(* ::Item:: *)
(*This differs from Schwarzschild; for in that case the relation is non-linear, but here it is linear*)


(* ::Input::Initialization:: *)
dirichletLength=#.g.#&[Dt/@coord]/.uvOldByNew/.r->\[Rho]c/rc r/.constParams/.Dt[rc]->0//Simplify
%/.r->rc/.constParams/.Dt[rc]->0//Simplify
Coefficient[dirichletLength,Dt[r]^2]//Simplify


(* ::Item:: *)
(*Vary new coord >> convert back to old*)


(* ::Input::Initialization:: *)
\[Delta][dirichletLength]/.r->rc/\[Rho]c r/.uvNewByOld/.constParams/.Dt[rc]->0//Simplify
\[Delta]gDirichlet=lengthToMetric[%]//ExpandAll//Simplify

#.(%/.r->\[Rho]c).#&[Dt/@coord]//Simplify


(* ::Item:: *)
(*The variation does not decay as r->\[Infinity]:*)


(* ::Input:: *)
(*(2 Tu Tv (Tu^2 Sqrt[rc^2-(Tu-Tv)^2]+2 Tu Sqrt[rc^2-(Tu-Tv)^2] Tv+Sqrt[rc^2-(Tu-Tv)^2] Tv^2+Tu^2 Sqrt[rc^2-(Tu+Tv)^2]-2 Tu Tv Sqrt[rc^2-(Tu+Tv)^2]+Tv^2 Sqrt[rc^2-(Tu+Tv)^2]-rc^2 (Sqrt[rc^2-(Tu-Tv)^2]+Sqrt[rc^2-(Tu+Tv)^2])) (-(Tu^2-Tv^2) (-Tv \[Delta]Tu+Tu \[Delta]Tv)+rc^2 (Tv \[Delta]Tu+Tu \[Delta]Tv)))/(((rc^2-(Tu-Tv)^2) (rc^2-(Tu+Tv)^2))^(3/2) (Sqrt[rc^2-(Tu-Tv)^2]+Sqrt[rc^2-(Tu+Tv)^2]))/.{*)
(*rc->1000,Tu->1,Tv->3,\[Delta]Tu->3,\[Delta]Tv->4*)
(*}//N[#,20]&*)


(* ::Item::Closed:: *)
(*Alternatively, Schwarzschild with rescaled u,v: also need to rescale r. [incomplete]*)


(* ::Input:: *)
(*#.g.#&[Dt/@coord]/.banadosToKarl/.constParams//Simplify*)
(**)
(*metricFormRescaled=#.g.#&[Dt/@coord]/.uvOldByNew/.banadosToKarl/.constParams/.Dt[rc]->0//Simplify*)
(**)
(*%/.r->rc/.Dt[rc]->0//FullSimplify*)


(* ::Item:: *)
(*Vary new coord >> convert back to old*)


(* ::Input:: *)
(*\[Delta][metricFormRescaled]/.karlToBanados/.uvNewByOld/.constParams/.Dt[rc]->0//Simplify*)
(*\[Delta]gc=Table[1/2 D[%,Dt[i],Dt[j]],{i,coord},{j,coord}]//Simplify*)
(**)
(*%/.r->\[Rho]c//Simplify*)


(* ::Item:: *)
(*Note that \[Delta] and restriction to the boundary do not commute!*)


(* ::Subitem:: *)
(*We need to cancel the \[Rho] dependence as well!*)


(* ::Item:: *)
(*If \[Delta]g vanishes on the boundary, does that mean \[Delta]Q=0?*)


(* ::Subitem:: *)
(*No, because \[Del]\[Delta]g need not vanish.*)


(* ::Subsubsection::Closed:: *)
(*Reconstructed from variation of coordinates*)


(* ::Input:: *)
(*\[Delta]g*)
(*\[Delta]gDirichlet-\[Delta]g//Shallow*)


(* ::Input:: *)
(*banadosLength/.Dt[r]->0/.r->\[Rho]c//FullSimplify*)
(*banadosLength/.Dt[r]->0/.r->\[Rho]c/.{u->x+t,v->x-t}//Simplify*)


(* ::Item:: *)
(*Consider the variation of coordinates*)


(* ::Input::Initialization:: *)
coord/.uvOldByNew/.r->\[Rho]c/rc r//\[Delta]//Simplify
\[Delta]coord=%/.r->rc/\[Rho]c r/.uvNewByOld//Simplify


(* ::Input:: *)
(*d\[Delta]coord=Dt[%]/.constParams/.{Dt[rc]->0,Dt[\[Delta]Tu]->0,Dt[\[Delta]Tv]->0}//Simplify*)


(* ::Item:: *)
(*d and \[Delta] in fact commute:*)


(* ::Input:: *)
(*coord/.uvOldByNew/.r->\[Rho]c/rc r//Simplify;*)
(*Dt[%]/.constParams/.{Dt[rc]->0,Dt[\[Delta]Tu]->0,Dt[\[Delta]Tv]->0}//Simplify*)
(*\[Delta][%]/.uvNewByOld/.r->rc/\[Rho]c r/.constParams/.{Dt[rc]->0,Dt[\[Delta]Tu]->0,Dt[\[Delta]Tv]->0}//Simplify*)
(*%==d\[Delta]coord//Simplify*)


(* ::Item:: *)
(*A more compact form:*)


(* ::Input::Initialization:: *)
\[Delta]coordHold={
Hold[\[Delta][(rc^2-Tu^2+Tv^2)/Sqrt[f[rc]]]Sqrt[f[rc]]] (u-v)/(2rc^2),
Hold[\[Delta][-((rc^2+Tu^2-Tv^2)/Sqrt[f[rc]])]Sqrt[f[rc]]] (u-v)/(2rc^2),
r Hold[\[Delta][Log[\[Rho]c]]]
};


(* ::Input:: *)
(*\[Delta]coordHold//ReleaseHold/.constParams/.{Dt[rc]->0,Dt[\[Delta]Tu]->0,Dt[\[Delta]Tv]->0}//Simplify;*)
(*%==\[Delta]coord//Simplify*)


(* ::Input:: *)
(*{Hold[\[Delta][Tv^2-Tu^2]-(rc^2-Tu^2+Tv^2)/2 \[Delta][Log[f[rc]]]] (u-v)/(2rc^2),Hold[\[Delta][Tv^2-Tu^2]+(rc^2+Tu^2-Tv^2)/2 \[Delta][Log[f[rc]]]] (u-v)/(2rc^2),Hold[\[Delta][Log[\[Rho]c]]]r};*)
(*%==\[Delta]coord//ReleaseHold//Simplify*)


(* ::Input::Initialization:: *)
\[Delta]gCorrection=lieD[\[Delta]coordHold,g]//Simplify


(* ::Input:: *)
(*\[Delta]g+\[Delta]gCorrection==\[Delta]gDirichlet//ReleaseHold//Simplify*)


(* ::Item:: *)
(*We've identified the corrections \[Delta]gDirichlet-\[Delta]g: it is precisely the variation of coordinates!*)


(* ::Subsubsection::Closed:: *)
(*Solving the generalized Killing equation*)


(* ::Item:: *)
(*We would like to obtain \[Delta]coord independent of the coordinate map. This amounts to solving:*)


(* ::Input::Initialization:: *)
Table["\[Delta]"<>ToString[i]<>"@@"<>ToString[coord],{i,coord}]//ToExpression

killingEqs=Thread[

((lieD[%,g]+\[Delta]g/.r->Hold[\[Rho]c])//Simplify)=={0,0,0}\[TensorProduct]{0,0,0}

]//Map[Thread]//FullSimplify;

Grid[%,Dividers->Center]
(*Collect[\[Placeholder],Derivative[__][\[Delta]u][__]]*)


(* ::Input:: *)
(*killingEqs//.{*)
(*\[Delta]r->Function[Evaluate[coord],r Hold[\[Delta][Log[\[Rho]c]]]]*)
(*,\[Delta]u->Function[Evaluate[coord],auu u-auv v]*)
(*,\[Delta]v->Function[Evaluate[coord],avu u-avv v]*)
(*}//FullSimplify;*)
(**)
(*Grid[%,Dividers->Center]*)
(**)
(*%%//Flatten//Union//Simplify//Apply[And]*)
(*%//ReleaseHold//Simplify*)


(* ::Item:: *)
(*We got 3 equations and 4 variables, under constrained.*)


(* ::Input:: *)
(*Solve[{auv rc^2+2 avv Tv^2-(auv (Tu^2+Tv^2)+2 Tv \[Delta]Tv)==0,*)
(*avu rc^2+2 Tu (auu Tu+\[Delta]Tu)-avu (Tu^2+Tv^2)==0,*)
(*auu (rc^2-Tu^2-Tv^2)+avv (-rc^2+Tu^2+Tv^2)-2 (auv Tu^2-avu Tv^2+Tu \[Delta]Tu+Tv \[Delta]Tv)==0},*)
(*{auu,auv,avu,avv}]*)


(* ::Input:: *)
(*{auv rc^2+2 avv Tv^2-(auv (Tu^2+Tv^2)+2 Tv \[Delta]Tv),*)
(*avu rc^2+2 Tu (auu Tu+\[Delta]Tu)-avu (Tu^2+Tv^2),*)
(*auu (rc^2-Tu^2-Tv^2)+avv (-rc^2+Tu^2+Tv^2)-2 (auv Tu^2-avu Tv^2+Tu \[Delta]Tu+Tv \[Delta]Tv)};*)
(**)
(*Table[D[%,a],{a,{auu,auv,avu,avv}}]*)
(*%%/.{auu->0,auv->0,avu->0,avv->0}*)
(**)
(*Append[%%,%]//Transpose//MatrixForm*)


(* ::Item:: *)
(*Ansatz: with u-v dependence, the system is now over constrained, but there is precisely one solution:*)


(* ::Input:: *)
(*killingEqs//.{*)
(*\[Delta]r->Function[Evaluate[coord],r Hold[\[Delta][Log[\[Rho]c]]]]*)
(*,\[Delta]u->Function[Evaluate[coord],au (u-v)/2]*)
(*,\[Delta]v->Function[Evaluate[coord],av (u-v)/2]*)
(*}//FullSimplify;*)
(**)
(*Grid[%,Dividers->Center];*)
(**)
(*%%//Flatten//FullSimplify//Union*)
(*Solve[%[[2;;3]],{au,av}]//ReleaseHold//FullSimplify*)
(**)
(*%//Flatten//Values*)
(*%==2Coefficient[\[Delta]coord,(u-v)][[;;2]]//Simplify*)
(**)
(*%%%%[[-1]]/.(%%%//Flatten)//ReleaseHold//Simplify*)


(* ::Input:: *)
(*\[Delta]coord[[;;2]]=={*)
(*(4Tv(2Tu Tv \[Delta]Tu+(rc^2-(Tu^2+Tv^2))\[Delta]Tv))/(rc^2 f[rc]) (u-v)/2,-((4Tu(2Tu Tv \[Delta]Tv+(rc^2-(Tu^2+Tv^2))\[Delta]Tu))/(rc^2 f[rc])) (u-v)/2*)
(*}//Simplify*)


(* ::Item:: *)
(*It agrees with \[Delta]coord.*)


(* ::Item:: *)
(*See explicitly that only 2 of the 3 equations are independent:*)


(* ::Input:: *)
(*{auv rc^2+2 avv Tv^2-(auv (Tu^2+Tv^2)+2 Tv \[Delta]Tv),*)
(*avu rc^2+2 Tu (auu Tu+\[Delta]Tu)-avu (Tu^2+Tv^2),*)
(*auu (rc^2-Tu^2-Tv^2)+avv (-rc^2+Tu^2+Tv^2)-2 (auv Tu^2-avu Tv^2+Tu \[Delta]Tu+Tv \[Delta]Tv)}/.{auu->au,auv->-au,avu->av,avv->-av}//Simplify*)
(**)
(*Table[D[%,a],{a,{au,av}}]*)
(*%%/.{au->0,av->0}*)
(**)
(*Append[%%,%]//Transpose//MatrixForm*)


(* ::Subsubsection::Closed:: *)
(*Entropy as charge*)


(* ::Input:: *)
(*\[Delta]gDirichlet-\[Delta]g==Lie[\[Delta]coord,g]//Simplify*)


(* ::Input:: *)
(*\[Delta]gC=\[Delta]gDirichlet-\[Delta]g//ExpandAll//Simplify;*)


(* ::Input:: *)
(*\[Delta]gC=Lie[\[Delta]coord,g]//Apart//ExpandAll//Simplify*)


(* ::Input:: *)
(*\[Delta]dqC=\[Delta]\[Chi][\[Xi],\[Delta]gC].{Dt[u],Dt[v],Dt[r]}/.constParams//timedSimplify[#,{},{1,120}]&(*//Apart*)(*//ExpandAll*)*)


(* ::Input:: *)
(*Csch[lu Tu] Sinh[2 Tu u]-Csch[lv Tv] Sinh[2 Tv v]/.vRTu*)


(* ::Item:: *)
(*First let's try to integrate along the cutoff:*)


(* ::Input:: *)
(*\[Delta]dqC/.Dt[r]->0/.r->\[Rho]c/.{*)
(*u->1/(2Tu) ArcSinh[t Sinh[Tu lu]],*)
(*v->1/(2Tv) ArcSinh[t Sinh[Tv lv]]*)
(*}/.constParams/.Dt[t]->1;*)
(*\[Delta]dqtC=%(*//ExpandAll*)(*//Apart*)//timedSimplify[#,{t>0},{1,30}]&*)


(* ::Input:: *)
(*\[Integral]Evaluate[\[Delta]dqtC/.Dt[t]->1]\[DifferentialD]t;*)
(*%/.{{t->Sinh[Tu luc]/Sinh[Tu lu]},{t->-(Sinh[Tu luc]/Sinh[Tu lu])}};*)
(*%//Apply[Subtract]*)
(*\[Delta]quC=(%/.ArcSinh[Csch[lu Tu] Sinh[luc Tu] Sinh[lv Tv]]->lvc Tv)//timedSimplify//Apart//ExpandAll//timedSimplify*)


(* ::Input::Initialization:: *)
\[Delta]quC=-((Csch[lu Tu] Csch[lv Tv] (-4 (luc-lvc) Tu^2 Tv^2 (-rc^2 \[Delta]Tu+Tu^2 \[Delta]Tu+Tv^2 \[Delta]Tu-2 Tu Tv \[Delta]Tv) Cosh[lv Tv] Sinh[lu Tu]-2 (-2 (luc-lvc) Tu^2 Tv^2 (-2 Tu Tv \[Delta]Tu+Tu^2 \[Delta]Tv+(-rc^2+Tv^2) \[Delta]Tv) Cosh[lu Tu]+(rc^4 (Tv \[Delta]Tu+Tu \[Delta]Tv)-rc^2 (2 Tu^2+2 Tv^2+Sqrt[rc^4+(Tu^2-Tv^2)^2-2 rc^2 (Tu^2+Tv^2)]) (Tv \[Delta]Tu+Tu \[Delta]Tv)+(Tu^2-Tv^2) (Tu^2 Tv \[Delta]Tu-Tv (Tv^2+Sqrt[rc^4+(Tu^2-Tv^2)^2-2 rc^2 (Tu^2+Tv^2)]) \[Delta]Tu+Tu^3 \[Delta]Tv+Tu (-Tv^2+Sqrt[rc^4+(Tu^2-Tv^2)^2-2 rc^2 (Tu^2+Tv^2)]) \[Delta]Tv)) Sinh[luc Tu]) Sinh[lv Tv]))/(8 G Tu Tv (rc^4+(Tu^2-Tv^2)^2-2 rc^2 (Tu^2+Tv^2))));


(* ::Item::Closed:: *)
(*Attempt to simplify (incomplete)*)


(* ::Input:: *)
(*(Sinh[Tu luc]/Sinh[Tu lu])^2/.l2lc//Together//Simplify*)


(* ::Input:: *)
(*\[Delta]quCSimp=\[Delta]quC/.Csch[lu Tu] Sinh[luc Tu]->( *)
(*Sqrt[(rc^2-(Tu-Tv)^2) (rc^2-(Tu+Tv)^2)]Sinh[luc Tu]/Sqrt[(-Sqrt[rc^4+(Tu^2-Tv^2)^2-2 rc^2 (Tu^2+Tv^2)]+(rc^2-Tu^2-Tv^2) Cosh[luc Tu]+2 Tu Tv Coth[lvc Tv] Sinh[luc Tu]) (Sqrt[rc^4+(Tu^2-Tv^2)^2-2 rc^2 (Tu^2+Tv^2)]+(rc^2-Tu^2-Tv^2) Cosh[luc Tu]+2 Tu Tv Coth[lvc Tv] Sinh[luc Tu])]*)
(*)*)


(* ::Input:: *)
(*Coth[lu Tu]^2/.l2lc//Together//Simplify*)


(* ::Input:: *)
(*\[Delta]quCSimp=\[Delta]quCSimp/.{*)
(*Coth[lu Tu]->Cosh[luc Tu](rc^2-Tu^2-Tv^2+2 Tu Tv Coth[lvc Tv] Tanh[luc Tu])Sqrt[*)
(*(-Sqrt[rc^4+(Tu^2-Tv^2)^2-2 rc^2 (Tu^2+Tv^2)]+(rc^2-Tu^2-Tv^2) Cosh[luc Tu]+2 Tu Tv Coth[lvc Tv] Sinh[luc Tu]) (Sqrt[rc^4+(Tu^2-Tv^2)^2-2 rc^2 (Tu^2+Tv^2)]+(rc^2-Tu^2-Tv^2) Cosh[luc Tu]+2 Tu Tv Coth[lvc Tv] Sinh[luc Tu])*)
(*]*)
(*}*)


(* ::Input:: *)
(*Coth[lv Tv]^2/.l2lc//Together//Simplify*)


(* ::Input:: *)
(*\[Delta]quCSimp=\[Delta]quCSimp/.{*)
(*Coth[lv Tv]->Cosh[lvc Tv](rc^2-Tu^2-Tv^2+2 Tu Tv Coth[luc Tu] Tanh[lvc Tv])Sqrt[*)
(*(-Sqrt[rc^4+(Tu^2-Tv^2)^2-2 rc^2 (Tu^2+Tv^2)]+(rc^2-Tu^2-Tv^2) Cosh[lvc Tv]+2 Tu Tv Coth[luc Tu] Sinh[lvc Tv]) (Sqrt[rc^4+(Tu^2-Tv^2)^2-2 rc^2 (Tu^2+Tv^2)]+(rc^2-Tu^2-Tv^2) Cosh[lvc Tv]+2 Tu Tv Coth[luc Tu] Sinh[lvc Tv])*)
(*]*)
(*}/.Sqrt[(rc^2-(Tu-Tv)^2) (rc^2-(Tu+Tv)^2)]->Simplify[rc^2 f[rc]]*)


(* ::Item:: *)
(*Numeric check:*)


(* ::Input:: *)
(*Module[{rands,checklist},*)
(*While[True,*)
(*rands=RandomReal[1,7,WorkingPrecision->50];*)
(*rands=Thread[{Tu,Tv,\[Delta]Tu,\[Delta]Tv,luc,lvc,rc}->rands*{1,1,1,1,\[Pi],\[Pi],1`50}];*)
(*checklist={rc^2-2 (Tu^2+Tv^2)+(Tu^2-Tv^2)^2/rc^2,*)
(*rc^2-Tu^2-Tv^2+2 Tu Tv Coth[lvc Tv] Tanh[luc Tu],*)
(*rc^2-Tu^2-Tv^2+2 Tu Tv Coth[luc Tu] Tanh[lvc Tv]}/.rands;*)
(*(*PrintTemporary[Column[{rands,Column[checklist],AllTrue[checklist,Positive]}]];*)*)
(*If[AllTrue[checklist,Positive],*)
(*Print[Column[checklist]];Break[]]*)
(*];*)
(*rands*)
(*]*)
(**)
(*(G(\[Delta][qc]-\[Delta]quNaive-\[Delta]quC)/.l2lc)/.%//N[#,10]&*)
(*(G(\[Delta][qc]-\[Delta]quDirichlet)/.l2lc)/.%%//N[#,10]&*)
(**)


(* ::Input:: *)
(*Module[{rands,checklist},*)
(*While[True,*)
(*rands=RandomReal[1,6,WorkingPrecision->50];*)
(*rands=Thread[{Tu,Tv,\[Delta]Tu,\[Delta]Tv,lc,rc}->rands*{1,1,1,1,\[Pi],1`50}];*)
(*checklist={rc^2-2 (Tu^2+Tv^2)+(Tu^2-Tv^2)^2/rc^2,*)
(*rc^2-Tu^2-Tv^2+2 Tu Tv Coth[lvc Tv] Tanh[luc Tu],*)
(*rc^2-Tu^2-Tv^2+2 Tu Tv Coth[luc Tu] Tanh[lvc Tv]}/.sameLc/.rands;*)
(*(*PrintTemporary[Column[{rands,Column[checklist],AllTrue[checklist,Positive]}]];*)*)
(*If[AllTrue[checklist,Positive],*)
(*Print[Column[checklist]];Break[]]*)
(*];*)
(*rands*)
(*]*)
(**)
(*(G(\[Delta][qc]-\[Delta]quNaive-\[Delta]quC)/.l2lc)/.sameLc/.%//N[#,10]&*)
(*(G(\[Delta][qc]-\[Delta]quDirichlet)/.l2lc)/.sameLc/.%%//N[#,20]&*)
(**)


(* ::Subsubsection::Closed:: *)
(*Constructed with "counter-term"*)


(* ::Input:: *)
(*\[Delta]gCountered=(\[Delta]g-(\[Delta]g/.r->\[Rho]c))//ExpandAll//Simplify*)


(* ::Input:: *)
(*\[Delta]gCountered/.r->\[Rho]c//Simplify*)


(* ::Input:: *)
(*\[Delta]dqCountered=\[Delta]\[Chi][\[Xi],\[Delta]gCountered].{Dt[u],Dt[v],Dt[r]}/.constParams//timedSimplify[#,{},{1,120}]&(*//Apart*)(*//ExpandAll*)*)


(* ::Input:: *)
(*Csch[lu Tu] Sinh[2 Tu u]-Csch[lv Tv] Sinh[2 Tv v]/.vRTu*)


(* ::Item:: *)
(*First let's try to integrate along the cutoff:*)


(* ::Input:: *)
(*\[Delta]dqCountered/.Dt[r]->0/.r->\[Rho]c/.{*)
(*u->1/(2Tu) ArcSinh[t Sinh[Tu lu]],*)
(*v->1/(2Tv) ArcSinh[t Sinh[Tv lv]]*)
(*}/.constParams/.Dt[t]->1;*)
(*\[Delta]dqtCountered=%(*//ExpandAll*)(*//Apart*)//timedSimplify[#,{t>0},{1,30}]&*)


(* ::Input:: *)
(*\[Integral]Evaluate[\[Delta]dqtCountered/.Dt[t]->1]\[DifferentialD]t;*)
(*%/.{{t->Sinh[Tu luc]/Sinh[Tu lu]},{t->-(Sinh[Tu luc]/Sinh[Tu lu])}};*)
(*%//Apply[Subtract]*)
(*\[Delta]quCountered=(%/.ArcSinh[Csch[lu Tu] Sinh[luc Tu] Sinh[lv Tv]]->lvc Tv)//timedSimplify//Apart//ExpandAll//timedSimplify*)


(* ::Input:: *)
(*Module[{rands,checklist},*)
(*While[True,*)
(*rands=RandomReal[1,7,WorkingPrecision->50];*)
(*rands=Thread[{Tu,Tv,\[Delta]Tu,\[Delta]Tv,luc,lvc,rc}->rands*{1,1,1,1,\[Pi],\[Pi],1`50}];*)
(*checklist={rc^2-2 (Tu^2+Tv^2)+(Tu^2-Tv^2)^2/rc^2,*)
(*rc^2-Tu^2-Tv^2+2 Tu Tv Coth[lvc Tv] Tanh[luc Tu],*)
(*rc^2-Tu^2-Tv^2+2 Tu Tv Coth[luc Tu] Tanh[lvc Tv]}/.rands;*)
(*(*PrintTemporary[Column[{rands,Column[checklist],AllTrue[checklist,Positive]}]];*)*)
(*If[AllTrue[checklist,Positive],*)
(*Print[Column[checklist]];Break[]]*)
(*];*)
(*rands*)
(*]*)
(**)
(*(G(\[Delta][qc]-\[Delta]quCountered)/.l2lc)/.%//N[#,10]&*)
(**)


(* ::Input:: *)
(*Module[{rands,checklist},*)
(*While[True,*)
(*rands=RandomReal[1,6,WorkingPrecision->50];*)
(*rands=Thread[{Tu,Tv,\[Delta]Tu,\[Delta]Tv,lc,rc}->rands*{1,1,1,1,\[Pi],1`50}];*)
(*checklist={rc^2-2 (Tu^2+Tv^2)+(Tu^2-Tv^2)^2/rc^2,*)
(*rc^2-Tu^2-Tv^2+2 Tu Tv Coth[lvc Tv] Tanh[luc Tu],*)
(*rc^2-Tu^2-Tv^2+2 Tu Tv Coth[luc Tu] Tanh[lvc Tv]}/.sameLc/.rands;*)
(*(*PrintTemporary[Column[{rands,Column[checklist],AllTrue[checklist,Positive]}]];*)*)
(*If[AllTrue[checklist,Positive],*)
(*Print[Column[checklist]];Break[]]*)
(*];*)
(*rands*)
(*]*)
(**)
(*(G(\[Delta][qc]-\[Delta]quCountered)/.l2lc)/.sameLc/.%//N[#,20]&*)
(**)


(* ::Item:: *)
(*Wrong!*)


(* ::Item:: *)
(*What about this?*)


(* ::Input:: *)
(*\[Delta][g/.r->\[Rho]c]//Simplify*)
(*\[Delta]gCountered=\[Delta]gCounteredAlt=\[Delta]g-%//ReleaseHold//Simplify*)


(* ::Input:: *)
(*\[Delta]gCountered/.r->\[Rho]c//Simplify*)


(* ::Input:: *)
(*\[Delta]dqCountered=\[Delta]\[Chi][\[Xi],\[Delta]gCountered].{Dt[u],Dt[v],Dt[r]}/.constParams//timedSimplify[#,{},{1,120}]&(*//Apart*)(*//ExpandAll*)*)


(* ::Item:: *)
(*Extremely complicated, probably wrong!*)


(* ::Subsubsection::Closed:: *)
(*III. Variation from Schwarzschild*)


(* ::Item:: *)
(*We vary the metric in Schwarzschild and then pull it back to Ba\[NTilde]ados.*)


(* ::Input::Initialization:: *)
\[Delta][#.g.#&[Dt/@coord]/.banadosToKarl/.constParams//Simplify];
Table[1/2 D[%,Dt[i],Dt[j]],{i,coord},{j,coord}]//Simplify

%%/.karlToBanados/.constParams/.Dt[rc]->0//Simplify;
\[Delta]gKarl=Table[1/2 D[%,Dt[i],Dt[j]],{i,coord},{j,coord}]//Simplify


(* ::Input:: *)
(*\[Delta]gKarl/.r->\[Rho]c//Simplify*)


(* ::Input:: *)
(*\[Delta]g//MatrixForm*)
(*\[Delta]gKarl//MatrixForm*)


(* ::Input:: *)
(*\[Delta]dqKarl=\[Delta]\[Chi][\[Xi],\[Delta]gKarl].{Dt[u],Dt[v],Dt[r]}/.constParams//Simplify*)


(* ::Input:: *)
(*\[Delta]dqKarl/.Dt[r]->0/.r->\[Rho]c/.{*)
(*u->1/(2Tu) ArcSinh[t Sinh[Tu lu]],*)
(*v->1/(2Tv) ArcSinh[t Sinh[Tv lv]]*)
(*}/.constParams/.Dt[t]->1;*)
(*\[Delta]dqtKarl=%(*//ExpandAll*)(*//Apart*)//timedSimplify[#,{t>0},{1,30}]&*)


(* ::Input:: *)
(*\[Integral]Evaluate[\[Delta]dqtKarl/.Dt[t]->1]\[DifferentialD]t;*)
(*%/.{{t->Sinh[Tu luc]/Sinh[Tu lu]},{t->-(Sinh[Tu luc]/Sinh[Tu lu])}};*)
(*%//Apply[Subtract]*)
(*\[Delta]quKarl=(%/.ArcSinh[Csch[lu Tu] Sinh[luc Tu] Sinh[lv Tv]]->lvc Tv)//timedSimplify//Apart//ExpandAll//timedSimplify*)


(* ::Input::Initialization:: *)
\[Delta]quKarl=1/(4 G) (luc \[Delta]Tu Coth[lu Tu]+lvc \[Delta]Tv Coth[lv Tv]-((-(Tu^2-Tv^2) (-Tv \[Delta]Tu+Tu \[Delta]Tv)+rc^2 (Tv \[Delta]Tu+Tu \[Delta]Tv)) Csch[lu Tu] Sinh[luc Tu])/(Tu Tv Sqrt[rc^4+(Tu^2-Tv^2)^2-2 rc^2 (Tu^2+Tv^2)]));


(* ::Input:: *)
(*\[Delta]quKarlAlt=1/(4G) (luc \[Delta]Tu Coth[lu Tu]+lvc \[Delta]Tv Coth[lv Tv]*)
(*-(((-Tu^2+Tv^2) (-Tv \[Delta]Tu+Tu \[Delta]Tv)+rc^2 (Tv \[Delta]Tu+Tu \[Delta]Tv)) Csch[lu Tu] Sinh[luc Tu] (rc^2-Tu^2-Tv^2+Sqrt[rc^2 Hold[f][rc]]))/(Tu Tv (rc^2 Hold[f][rc]+(rc^2-Tu^2-Tv^2) Sqrt[rc^2 Hold[f][rc]])));*)


(* ::Input:: *)
(*Module[{rands,checklist},*)
(*While[True,*)
(*rands=RandomReal[1,7,WorkingPrecision->50];*)
(*rands=Thread[{Tu,Tv,\[Delta]Tu,\[Delta]Tv,luc,lvc,rc}->rands*{1,1,1,1,\[Pi],\[Pi],1`50}];*)
(*checklist={rc^2-2 (Tu^2+Tv^2)+(Tu^2-Tv^2)^2/rc^2,*)
(*rc^2-Tu^2-Tv^2+2 Tu Tv Coth[lvc Tv] Tanh[luc Tu],*)
(*rc^2-Tu^2-Tv^2+2 Tu Tv Coth[luc Tu] Tanh[lvc Tv]}/.rands;*)
(*(*PrintTemporary[Column[{rands,Column[checklist],AllTrue[checklist,Positive]}]];*)*)
(*If[AllTrue[checklist,Positive],*)
(*Print[Column[checklist]];Break[]]*)
(*];*)
(*rands*)
(*]*)
(**)
(*(G(\[Delta][qc]-\[Delta]qKarl//ReleaseHold)/.l2lc)/.%//N[#,10]&*)
(**)


(* ::Item:: *)
(*It works! Amazing!*)


(* ::Item:: *)
(*We need to understand why it works: what' s the boundary condition & phase space!*)


(* ::Input:: *)
(**)


(* ::Subsubsection::Closed:: *)
(*Attempt to simplify & prove*)


(* ::Input:: *)
(*Coth[lu Tu]/.l2lc*)


(* ::Input:: *)
(*(Cosh[luc Tu] (rc^2-Tu^2-Tv^2+2 Tu Tv Coth[lvc Tv] Tanh[luc Tu]))/Sqrt[(rc^2-(Tu-Tv)^2) (rc^2-(Tu+Tv)^2)(-1+(Cosh[luc Tu] (rc^2-Tu^2-Tv^2+2 Tu Tv Coth[lvc Tv] Tanh[luc Tu]))/Sqrt[(rc^2-(Tu-Tv)^2) (rc^2-(Tu+Tv)^2)])(1+(Cosh[luc Tu] (rc^2-Tu^2-Tv^2+2 Tu Tv Coth[lvc Tv] Tanh[luc Tu]))/Sqrt[(rc^2-(Tu-Tv)^2) (rc^2-(Tu+Tv)^2)])] //Simplify*)


(* ::Input:: *)
(*Coth[lv Tv]/.l2lc*)


(* ::Input:: *)
(*(Cosh[lvc Tv] (rc^2-Tu^2-Tv^2+2 Tu Tv Coth[luc Tu] Tanh[lvc Tv]))/Sqrt[(rc^2-(Tu-Tv)^2) (rc^2-(Tu+Tv)^2)(-1+(Cosh[lvc Tv] (rc^2-Tu^2-Tv^2+2 Tu Tv Coth[luc Tu] Tanh[lvc Tv]))/Sqrt[(rc^2-(Tu-Tv)^2) (rc^2-(Tu+Tv)^2)])(1+(Cosh[lvc Tv] (rc^2-Tu^2-Tv^2+2 Tu Tv Coth[luc Tu] Tanh[lvc Tv]))/Sqrt[(rc^2-(Tu-Tv)^2) (rc^2-(Tu+Tv)^2)])] //Simplify*)


(* ::Input:: *)
(*Csch[lu Tu]/.l2lc*)


(* ::Input:: *)
(*1/Sqrt[(-1+(Cosh[luc Tu] (rc^2-Tu^2-Tv^2+2 Tu Tv Coth[lvc Tv] Tanh[luc Tu]))/Sqrt[(rc^2-(Tu-Tv)^2) (rc^2-(Tu+Tv)^2)])(1+(Cosh[luc Tu] (rc^2-Tu^2-Tv^2+2 Tu Tv Coth[lvc Tv] Tanh[luc Tu]))/Sqrt[(rc^2-(Tu-Tv)^2) (rc^2-(Tu+Tv)^2)])]//Simplify*)


(* ::Input:: *)
(*Sqrt[rc^4+(Tu^2-Tv^2)^2-2 rc^2 (Tu^2+Tv^2)]/Sqrt[-(rc^4+(Tu^2-Tv^2)^2-2 rc^2 (Tu^2+Tv^2))+Cosh[luc Tu]^2 (-rc^2+Tu^2+Tv^2-2 Tu Tv Coth[lvc Tv] Tanh[luc Tu])^2]//Simplify*)


(* ::Input:: *)
(*\[Delta]quKarlSimp=\[Delta]quKarl/.{*)
(*Coth[lu Tu]->(Cosh[luc Tu] (rc^2-Tu^2-Tv^2+2 Tu Tv Coth[lvc Tv] Tanh[luc Tu]))/(\[Sqrt](-rc^4+2 rc^2 Tu^2-Tu^4+2 rc^2 Tv^2+2 Tu^2 Tv^2-Tv^4+(-rc^2+Tu^2+Tv^2)^2 Cosh[luc Tu]^2+4 Tu^2 Tv^2 Coth[lvc Tv]^2 Sinh[luc Tu]^2-2 Tu Tv (-rc^2+Tu^2+Tv^2) Coth[lvc Tv] Sinh[2 luc Tu])),*)
(*Coth[lv Tv]->(Cosh[lvc Tv] (rc^2-Tu^2-Tv^2+2 Tu Tv Coth[luc Tu] Tanh[lvc Tv]))/(\[Sqrt](-rc^4+2 rc^2 Tu^2-Tu^4+2 rc^2 Tv^2+2 Tu^2 Tv^2-Tv^4+(-rc^2+Tu^2+Tv^2)^2 Cosh[lvc Tv]^2+4 Tu^2 Tv^2 Coth[luc Tu]^2 Sinh[lvc Tv]^2-2 Tu Tv (-rc^2+Tu^2+Tv^2) Coth[luc Tu] Sinh[2 lvc Tv])),*)
(*Csch[lu Tu]->Sqrt[rc^4+(Tu^2-Tv^2)^2-2 rc^2 (Tu^2+Tv^2)]/Sqrt[-(rc^4+(Tu^2-Tv^2)^2-2 rc^2 (Tu^2+Tv^2))+Cosh[luc Tu]^2 (-rc^2+Tu^2+Tv^2-2 Tu Tv Coth[lvc Tv] Tanh[luc Tu])^2]*)
(*}*)


(* ::Subsubsection::Closed:: *)
(*IV. Accounting for the fluctuating cutoff*)


(* ::Input::Initialization:: *)
\[Delta]gRadial=\[Delta]g+(\!\(
\*SubscriptBox[\(\[PartialD]\), \(r\)]g\)/.r->\[Rho]c)\[Delta][\[Rho]c]//Simplify


(* ::Input:: *)
(*Series[\[Delta]gRadial,{r,\[Infinity],2}]//Simplify*)


(* ::Input:: *)
(*\[Delta]dqRadial=\[Delta]\[Chi][\[Xi],\[Delta]gRadial].{Dt[u],Dt[v],Dt[r]}/.constParams//timedSimplify[#,{},{1,60}]&(*//Apart*)(*//ExpandAll*)*)


(* ::Input:: *)
(*Export["\[Delta]dqRadial.wdx",\[Delta]dqRadial]*)


(* ::Input:: *)
(*\[Delta]dqRadial>>"\[Delta]dqRadial.log"*)


(* ::Item:: *)
(*First let's try to integrate along the cutoff:*)


(* ::Input:: *)
(*\[Delta]dqRadial(*/.Dt[r]\[Rule]0*)/.r->\[Rho]c/.{*)
(*u->1/(2Tu) ArcSinh[t Sinh[Tu lu]],*)
(*v->1/(2Tv) ArcSinh[t Sinh[Tv lv]]*)
(*}/.constParams/.Dt[t]->1;*)
(*\[Delta]dqtRadial=%(*//ExpandAll*)//Apart//timedSimplify[#,{t>0},{1,60}]&*)


(* ::Input:: *)
(*Export["\[Delta]dqtRadial.wdx",\[Delta]dqtRadial]*)


(* ::Input:: *)
(*\[Delta]dqtRadial>>"\[Delta]dqtRadial.log"*)


(* ::Input:: *)
(*\[Integral]Evaluate[\[Delta]dqtRadial/.Dt[t]->1]\[DifferentialD]t;*)
(*%/.{{t->Sinh[Tu luc]/Sinh[Tu lu]},{t->-(Sinh[Tu luc]/Sinh[Tu lu])}};*)
(*%//Apply[Subtract]*)
(*\[Delta]quRadial=(%/.ArcSinh[Csch[lu Tu] Sinh[luc Tu] Sinh[lv Tv]]->lvc Tv)(*//Simplify//Apart//ExpandAll//Simplify*)//timedSimplify[#,{t>0},{1,60}]&*)


(* ::Input:: *)
(*Export["\[Delta]quRadial.wdx",\[Delta]quRadial]*)


(* ::Input:: *)
(*\[Delta]quRadial>>"\[Delta]quRadial.log"*)


(* ::Input:: *)
(*Module[{rands,checklist},*)
(*While[True,*)
(*rands=RandomReal[1,7,WorkingPrecision->50];*)
(*rands=Thread[{Tu,Tv,\[Delta]Tu,\[Delta]Tv,luc,lvc,rc}->rands*{1,1,1,1,\[Pi],\[Pi],1`50}];*)
(*checklist={rc^2-2 (Tu^2+Tv^2)+(Tu^2-Tv^2)^2/rc^2,*)
(*rc^2-Tu^2-Tv^2+2 Tu Tv Coth[lvc Tv] Tanh[luc Tu],*)
(*rc^2-Tu^2-Tv^2+2 Tu Tv Coth[luc Tu] Tanh[lvc Tv]}/.rands;*)
(*(*PrintTemporary[Column[{rands,Column[checklist],AllTrue[checklist,Positive]}]];*)*)
(*If[AllTrue[checklist,Positive],*)
(*Print[Column[checklist]];Break[]]*)
(*];*)
(*rands*)
(*]*)
(**)
(*(G(\[Delta][qc]-\[Delta]quRadial//ReleaseHold)/.l2lc)/.%//N[#,10]&*)
(**)


(* ::Item:: *)
(*Not right!*)


(* ::Subsubsection:: *)
(*V. Fix \[Delta]\[Rho] from variation of Subscript[g, \[Phi]\[Phi]]=\!\(\*SubsuperscriptBox[\(r\), \(c\), \(2\)]\)*)


(* ::Input:: *)
(*{u,v}/.uvOldByNew//\[Delta];*)
(*%/.uvNewByOld//Simplify*)
(**)
(*%==\[Delta]coord[[;;2]]*)


(* ::Input::Initialization:: *)
banadosLength/.{u->x,v->x}/.Dt[r]->0//Simplify

(%/.Dt[x]->1);
\[Delta][%]+(\!\(
\*SubscriptBox[\(\[PartialD]\), \(r\)]%\))\[Delta]r//Simplify

Solve[%==0,\[Delta]r]//Simplify
\[Delta]\[Rho]=\[Delta]r/.Flatten[%];


(* ::Input::Initialization:: *)
\[Delta]gCorrect=\[Delta]g+lieD[{0,0,\[Delta]\[Rho]},g]//Simplify
\[Delta]gCorrect==\[Delta]gKarl


(* ::Item:: *)
(*This is effectively the same as variation in Schwarzschild!*)


(* ::Item:: *)
(*On the other hand, if we impose Dirichlet at cutoff:*)


(* ::Input::Initialization:: *)
\[Delta]gNormalized=\[Delta]g+lieD[{Sequence@@\[Delta]coord[[1;;2]],\[Delta]\[Rho]},g]//Simplify


(* ::Input:: *)
(*\[Delta]gNormalized/.r->\[Rho]c//Simplify*)


(* ::Input:: *)
(*\[Delta]dqNormalized=\[Delta]\[Chi][\[Xi],\[Delta]gNormalized].{Dt[u],Dt[v],Dt[r]}/.constParams//timedSimplify[#,{},{1,60}]&(*//Apart*)(*//ExpandAll*)*)


(* ::Item:: *)
(*First let's try to integrate along the cutoff:*)


(* ::Input:: *)
(*\[Delta]dqNormalized(*/.Dt[r]\[Rule]0*)/.r->\[Rho]c/.{*)
(*u->1/(2Tu) ArcSinh[t Sinh[Tu lu]],*)
(*v->1/(2Tv) ArcSinh[t Sinh[Tv lv]]*)
(*}/.constParams/.Dt[t]->1;*)
(*\[Delta]dqtNormalized=%(*//ExpandAll*)(*//Apart*)//timedSimplify[#,{t>0},{1,60}]&*)


(* ::Input:: *)
(*\[Integral]Evaluate[\[Delta]dqtNormalized/.Dt[t]->1]\[DifferentialD]t;*)
(*%/.{{t->Sinh[Tu luc]/Sinh[Tu lu]},{t->-(Sinh[Tu luc]/Sinh[Tu lu])}};*)
(*%//Apply[Subtract]*)
(*\[Delta]quNormalized=(%/.ArcSinh[Csch[lu Tu] Sinh[luc Tu] Sinh[lv Tv]]->lvc Tv)(*//Simplify//Apart//ExpandAll//Simplify*)//timedSimplify[#,{t>0},{1,60}]&*)


(* ::Input:: *)
(*Module[{rands,checklist},*)
(*While[True,*)
(*rands=RandomReal[1,7,WorkingPrecision->50];*)
(*rands=Thread[{Tu,Tv,\[Delta]Tu,\[Delta]Tv,luc,lvc,rc}->rands*{1,1,1,1,\[Pi],\[Pi],1`50}];*)
(*checklist={rc^2-2 (Tu^2+Tv^2)+(Tu^2-Tv^2)^2/rc^2,*)
(*rc^2-Tu^2-Tv^2+2 Tu Tv Coth[lvc Tv] Tanh[luc Tu],*)
(*rc^2-Tu^2-Tv^2+2 Tu Tv Coth[luc Tu] Tanh[lvc Tv]}/.rands;*)
(*(*PrintTemporary[Column[{rands,Column[checklist],AllTrue[checklist,Positive]}]];*)*)
(*If[AllTrue[checklist,Positive],*)
(*Print[Column[checklist]];Break[]]*)
(*];*)
(*rands*)
(*]*)
(**)
(*(G(\[Delta][qc]-\[Delta]quNormalized)/.l2lc)/.%*)
(**)


(* ::Item:: *)
(*Not right!*)


(* ::Subsubsection::Closed:: *)
(*Generalized Killing equation with new \[Delta]\[Rho]*)


(* ::Input:: *)
(*\[Delta]\[Rho]*)


(* ::Input:: *)
(*killingEqs=Thread[*)
(**)
(*Simplify[*)
(*lieD[{\[Delta]u[u,v,r],\[Delta]v[u,v,r],\[Delta]\[Rho]},g]+\[Delta]g/.r->Hold[\[Rho]c]*)
(*][[;;2,;;2]]=={0,0}\[TensorProduct]{0,0}*)
(**)
(*]//Map[Thread]//FullSimplify;*)
(**)
(*Grid[%,Dividers->Center]*)
(*(*Collect[\[Placeholder],Derivative[__][\[Delta]u][__]]*)*)


(* ::Input:: *)
(*killingEqs//.{*)
(*\[Delta]u->Function[Evaluate[coord],auu u-auv v],*)
(*\[Delta]v->Function[Evaluate[coord],avu u-avv v]*)
(*}//FullSimplify;*)
(**)
(*Grid[%,Dividers->Center]*)
(**)
(*%%//Flatten//Union//Simplify//Apply[And]*)
(*%//ReleaseHold//Simplify*)


(* ::Item:: *)
(*We got 3 equations and 4 variables, under constrained.*)


(* ::Input:: *)
(*Solve[{auv rc^2+2 avv Tv^2-(auv (Tu^2+Tv^2)+2 Tv \[Delta]Tv)==0,*)
(*avu rc^2+2 Tu (auu Tu+\[Delta]Tu)-avu (Tu^2+Tv^2)==0,*)
(*(auu-avv) (rc^2-Tu^2-Tv^2)-2 (auv Tu^2-avu Tv^2+Tu \[Delta]Tu+Tv \[Delta]Tv)==0},*)
(*{auu,auv,avu,avv}]*)


(* ::Input:: *)
(*{auv rc^2+2 avv Tv^2-(auv (Tu^2+Tv^2)+2 Tv \[Delta]Tv),*)
(*avu rc^2+2 Tu (auu Tu+\[Delta]Tu)-avu (Tu^2+Tv^2),*)
(*(auu-avv) (rc^2-Tu^2-Tv^2)-2 (auv Tu^2-avu Tv^2+Tu \[Delta]Tu+Tv \[Delta]Tv)};*)
(**)
(*Table[D[%,a],{a,{auu,auv,avu,avv}}]*)
(*%%/.{auu->0,auv->0,avu->0,avv->0}*)
(**)
(*Append[%%,%]//Transpose//MatrixForm*)


(* ::Item:: *)
(*Ansatz: with u-v dependence, the system is now over constrained, but there is precisely one solution:*)


(* ::Input:: *)
(*killingEqs//.{*)
(*\[Delta]u->Function[Evaluate[coord],au (u-v)/2],*)
(*\[Delta]v->Function[Evaluate[coord],av (u-v)/2]*)
(*}//FullSimplify;*)
(**)
(*Grid[%,Dividers->Center];*)
(**)
(*%%//Flatten//FullSimplify//Union*)
(*Solve[%[[2;;3]],{au,av}]//ReleaseHold//FullSimplify*)
(**)
(*%//Flatten//Values*)
(*%==2Coefficient[\[Delta]coord,(u-v)][[;;2]]//Simplify*)
(**)
(*%%%%[[-1]]/.(%%%//Flatten)//ReleaseHold//Simplify*)


(* ::Input:: *)
(*\[Delta]coord[[;;2]]=={*)
(*(4Tv(2Tu Tv \[Delta]Tu+(rc^2-(Tu^2+Tv^2))\[Delta]Tv))/(rc^2 f[rc]) (u-v)/2,-((4Tu(2Tu Tv \[Delta]Tv+(rc^2-(Tu^2+Tv^2))\[Delta]Tu))/(rc^2 f[rc])) (u-v)/2*)
(*}//Simplify*)


(* ::Item:: *)
(*It agrees with \[Delta]coord.*)


(* ::Item:: *)
(*See explicitly that only 2 of the 3 equations are independent:*)


(* ::Input:: *)
(*{auv rc^2+2 avv Tv^2-(auv (Tu^2+Tv^2)+2 Tv \[Delta]Tv),*)
(*avu rc^2+2 Tu (auu Tu+\[Delta]Tu)-avu (Tu^2+Tv^2),*)
(*auu (rc^2-Tu^2-Tv^2)+avv (-rc^2+Tu^2+Tv^2)-2 (auv Tu^2-avu Tv^2+Tu \[Delta]Tu+Tv \[Delta]Tv)}/.{auu->au,auv->-au,avu->av,avv->-av}//Simplify*)
(**)
(*Table[D[%,a],{a,{au,av}}]*)
(*%%/.{au->0,av->0}*)
(**)
(*Append[%%,%]//Transpose//MatrixForm*)


(* ::Section:: *)
(*Ending*)


(* ::Input::Initialization:: *)
<<"Physica/MathUtils.wl"
saveScript[];



