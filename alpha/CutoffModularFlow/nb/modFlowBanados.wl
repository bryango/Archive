#!/usr/bin/env wolframscript
(* ::Package:: *)

(* ::Input::Initialization:: *)
SetDirectory[NotebookDirectory[]]


(* ::Section:: *)
(*Notes*)


(* ::Item:: *)
(*Both the Schwarzschild and the Ba\[NTilde]ados radial coordinates are denoted with by r.*)


(* ::Item:: *)
(*We can convert the Schwarzschild r into the Ba\[NTilde]ados r with the following command:*)


(* ::Input:: *)
(*karlToBanados*)


(* ::Subsubsection::Closed:: *)
(*Setup*)


(* ::Input::Initialization:: *)
$Assumptions=And[u\[Element]Reals,r\[Element]Reals,v\[Element]Reals,r>Tu+Tv>0,Tu>0,Tv>0,lu>0,lv>0,luc>0,lvc>0,-(lu/2)<u<lu/2,-(lv/2)<v<lv/2,L>0,T>0,r>2T,rc>2T,L\[Infinity]>L,x\[Element]Reals,lc>0];


(* ::Input::Initialization:: *)
params={Tu,Tv,lu,lv};
constParams=(Dt[#]->0&)/@params


(* ::Input::Initialization:: *)
coord={u,v,r};
metricSign=-1;


(* ::Input::Initialization:: *)
<<"Physica/GRUtils.wl"


(* ::Subsubsection::Closed:: *)
(*Poincar\[EAcute] to BTZ in Ba\[NTilde]ados metric*)


(* ::Input::Initialization:: *)
u2btz=u->Sqrt[(r^2-(Tu+Tv)^2)/(r^2-(Tu-Tv)^2)] E^(2u*Tu);
v2btz=(u2btz/.u->v/.{Tu->Tv,Tv->Tu})//Simplify
z2btz=z->Sqrt[((Tu+Tv)^2-(Tu-Tv)^2)/(r^2-(Tu-Tv)^2)]Exp[(u+v)/2 (Tu+Tv)+(u-v)/2 (Tu-Tv)]//Simplify
poincare2btzKarl={u2btz,v2btz,z2btz};

btzKarl=(Dt[u]Dt[v]+Dt[z]^2)/z^2/.poincare2btzKarl/.(Dt[#]->0&/@{Tu,Tv})//Simplify


(* ::Input:: *)
(*(r^2-(Tu+Tv)^2)(r^2-(Tu-Tv)^2)==r^4+(Tu^2-Tv^2)^2-2 r^2 (Tu^2+Tv^2)//Simplify*)


(* ::Item:: *)
(*Schwarzschild to Ba\[NTilde]ados: see <https://arxiv.org/abs/hep-th/9901148> eq. (90)*)


(* ::Input::Initialization:: *)
karlToBanados=(r->Sqrt[(Tu+Tv)^2 (Cosh[Log[r/Sqrt[Tu Tv]]])^2-(Tu-Tv)^2 (Sinh[Log[r/Sqrt[Tu Tv]]])^2])//Simplify
btzKarl/.karlToBanados/.{Dt[Tu]->0,Dt[Tv]->0}//Simplify

btzMetricForm=Dt[r]^2/r^2+(r Dt[u]+Tv^2/r Dt[v])(r Dt[v]+Tu^2/r Dt[u]);
%-%%//Simplify


(* ::Input::Initialization:: *)
poincare2btz=poincare2btzKarl/.karlToBanados//Simplify

(Dt[u]Dt[v]+Dt[z]^2)/z^2/.poincare2btz/.(Dt[#]->0&/@{Tu,Tv})//Simplify
btzMetricForm==%//Simplify


(* ::Input::Initialization:: *)
metric=btz`metric=quadToMatrix[btzMetricForm,coord]


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


(* ::Subsubsection::Closed:: *)
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


(* ::Input::Initialization:: *)
{u,v,z}/.poincare2btz;

jacobianPoincarebyBTZ=jacobianFromFunc[
Function[{u,v,r},%//Evaluate],
{u,v,r}
]//Simplify;

jacobianBTZbyPoincare=Inverse[jacobianPoincarebyBTZ]//Simplify;


(* ::Input:: *)
(*juBTZs=(jacobianBTZbyPoincare.(juPoincare[#]/.poincare2btz)//Simplify)&/@Range[-1,1];*)
(*jvBTZs=(jacobianBTZbyPoincare.(jvPoincare[#]/.poincare2btz)//Simplify)&/@Range[-1,1];*)
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


(* ::Subsubsection::Closed:: *)
(*Bulk modular flow generator & fixed points*)


(* ::Input::Initialization:: *)
endptsRule={SubPlus[u]->E^(lu Tu),SubPlus[v]->E^(lv Tv),SubMinus[u]->E^(-lu Tu),SubMinus[v]->E^(-lv Tv)};

coeffs={-((SubMinus[u] SubPlus[u])/(SubMinus[u]-SubPlus[u])),(SubMinus[u]+SubPlus[u])/(SubMinus[u]-SubPlus[u]),1/(-SubMinus[u]+SubPlus[u]),-((SubMinus[v] SubPlus[v])/(SubMinus[v]-SubPlus[v])),(SubMinus[v]+SubPlus[v])/(SubMinus[v]-SubPlus[v]),1/(-SubMinus[v]+SubPlus[v])}/.endptsRule//FullSimplify


(* ::Input::Initialization:: *)
\[Xi]=2\[Pi](
 coeffs[[1;;3]].(ju/@Range[-1,1])
-coeffs[[4;;6]].(jv/@Range[-1,1])
)//ExpToTrig//Simplify


(* ::Item:: *)
(*First equation for the geodesic:*)


(* ::Input::Initialization:: *)
RTslice=Hold[Sinh[2Tu u]/Sinh[Tu lu]==Sinh[2Tv v]/Sinh[Tv lv]];


(* ::Input:: *)
(*Simplify[\[Xi][[3]]==0]*)
(*Simplify[ReleaseHold[RTslice]];*)
(*%===%%*)


(* ::Input::Initialization:: *)
RTv2u=v->1/(2Tv) ArcSinh[Sinh[Tv lv]/Sinh[Tu lu] Sinh[2Tu u]];
RTu2v=u->1/(2Tu) ArcSinh[Sinh[Tu lu]/Sinh[Tv lv] Sinh[2Tv v]];

\[Xi][[3]]/.RTv2u
\[Xi][[3]]/.RTu2v


(* ::Subsubsection::Closed:: *)
(*Geodesic: special case*)


(* ::Item:: *)
(*Let's first look at the special case:*)


(* ::Input::Initialization:: *)
sameT={Tu->T,Tv->T};
sameL={lu->L,lv->L};
constTime={u->x,v->x};


(* ::Item:: *)
(*RT surface in Schwarzschild can be found in the textbook:*)


(* ::Input::Initialization:: *)
RTsameTkarl=r->(2T Cosh[T L])/Sqrt[Cosh[T L]^2-Cosh[2T x]^2];


(* ::Item:: *)
(*Map to the Banados coordinates:*)


(* ::Input:: *)
(*(r/.RTsameTkarl)^2==(r/.karlToBanados/.sameT)^2*)
(**)
(*Sqrt[r2/.Solve[%/.r->Sqrt[r2],r2]]//Simplify*)
(**)
(*(\[Xi]/.sameT/.sameL/.constTime/.r->#)&/@%//Simplify*)


(* ::Item:: *)
(*The second solution is the one:*)


(* ::Input::Initialization:: *)
RTsameT=r->T Sqrt[(Cosh[L T]+Cosh[2 T x])/(Cosh[L T]-Cosh[2 T x])];


(* ::Subsubsection::Closed:: *)
(*Geodesic: general case*)


(* ::Item:: *)
(*We use the special case above to filter out the correct solution!*)


(* ::Input:: *)
(*\[Xi]/.RTv2u//FullSimplify;*)
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


(* ::Input::Initialization:: *)
RTuv=(r->Sqrt[Tu Tv] Sqrt[Sinh[Tu lu]/Sinh[Tv lv] (Cosh[Tv lv]+Cosh[2Tv v])/(Cosh[Tu lu]-Cosh[2Tu u])]);
RTu=RTuv/.RTv2u//FullSimplify


(* ::Input:: *)
(*\[Xi]/.RTuv/.RTv2u//Simplify*)


(* ::Input:: *)
(**)


(* ::Item:: *)
(*Now we temporarily take Subscript[T, u]=Subscript[T, v]=T and Subscript[l, u]=Subscript[l, v]=L to simplify our calculations; check surface gravity (for same T):*)


(* ::Input:: *)
(*metric=static`metric=btz`metric/.sameT//Simplify*)
(*<<diffgeoM`*)
(**)
(*covD[lower[\[Xi]/.sameT/.sameL]];*)
(*contract[%**%,{1,3},{2,4}];*)
(**)
(*Simplify[%/.RTsameT/.constTime]*)
(**)
(*metric=btz`metric;*)
(*<<diffgeoM`*)


(* ::Input:: *)
(**)


(* ::Item:: *)
(*Mathematica refuses to handle the general case if we eliminate v or u and compute with brute force.*)


(* ::Item:: *)
(*However, it can work with the more symmetric expression RTuv:*)


(* ::Input:: *)
(*RTuv*)


(* ::Input:: *)
(*metric=btz`metric;*)
(*<<diffgeoM`*)
(**)
(*covD[lower[\[Xi]]]/.RTuv;*)
(*contract[%**%,{1,3},{2,4}]/.RTuv;*)
(**)
(*PrintTemporary["# Done! Simplifying ..."];*)
(**)
(*Simplify[%%]*)
(**)
(*%/.RTv2u//Simplify*)


(* ::Subsubsection::Closed:: *)
(*Geodesic: numeric check*)


(* ::Input:: *)
(*RTu*)


(* ::Item:: *)
(*Check that it is indeed a geodesic:*)


(* ::Input:: *)
(*x={u,v,r}/.RTv2u/.RTu//Simplify*)
(*dx=lower[\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(u\)]x\)]/.RTv2u/.RTu;*)
(**)
(*rhs=\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(u\)]x\)*)
(*lhs=\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(u\)]\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(u\)]x\)\)+contract[Christoffel**dx**dx,{2,4},{3,5}]/.RTv2u/.RTu;*)
(*diff=lhs-lhs[[1]]rhs;*)
(**)
(*RandomReal[1,4,WorkingPrecision->50];*)
(*Thread[{Tu,Tv,lu,lv}->%]*)
(**)
(*Plot[*)
(*Norm[diff/.%,\[Infinity]],*)
(*{u,-1,1}*)
(*,WorkingPrecision->30*)
(*,PlotRange->{-10^-#,10^-#}&@200//Evaluate*)
(*,PlotStyle->Thickness[.01]*)
(*]*)


(* ::Subsubsection::Closed:: *)
(*Geodesic: radial parameter*)


(* ::Input:: *)
(*Solve[(r/.RTuv/.RTv2u/.u->1/(2Tu) ArcCosh[x]//Simplify)==r,x]//Simplify*)
(*Solve[(r/.RTuv/.RTu2v/.v->1/(2Tv) ArcCosh[y]//Simplify)==r,y]//Simplify*)


(* ::Input::Initialization:: *)
uRTr=u->1/(2Tu) ArcCosh[((r^4+Tu^2 Tv^2) Cosh[lu Tu]-2 r^2 Tu Tv Coth[lv Tv] Sinh[lu Tu])/(r^4-Tu^2 Tv^2)];
vRTr=v->1/(2Tv) ArcCosh[((r^4+Tu^2 Tv^2) Cosh[lv Tv]-2 r^2 Tu Tv Coth[lu Tu] Sinh[lv Tv])/(r^4-Tu^2 Tv^2)];


(* ::Input:: *)
(*r/.RTuv/.uRTr/.vRTr//Simplify*)


(* ::Input:: *)
(*RTslice*)


(* ::Input:: *)
(*((((r^4+Tu^2 Tv^2) Cosh[lu Tu]-2 r^2 Tu Tv Coth[lv Tv] Sinh[lu Tu])/(r^4-Tu^2 Tv^2))^2-1)/Sinh[Tu lu]^2-((((r^4+Tu^2 Tv^2) Cosh[lv Tv]-2 r^2 Tu Tv Coth[lu Tu] Sinh[lv Tv])/(r^4-Tu^2 Tv^2))^2-1)/Sinh[Tv lv]^2//Simplify*)


(* ::Subsubsection::Closed:: *)
(*Geodesic: analytic check*)


(* ::Item:: *)
(*We can check the geodesic equations analytically:*)


(* ::Input:: *)
(*x={u,v,r}/.uRTr/.vRTr;*)
(*dx=lower[\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(r\)]x\)]/.uRTr/.vRTr;*)
(**)
(*rhs=\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(r\)]x\)*)
(*lhs=\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(r\)]\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(r\)]x\)\)+contract[Christoffel**dx**dx,{2,4},{3,5}]/.uRTr/.vRTr;*)
(**)
(*Thread[lhs-(lhs[[-1]]rhs)=={0,0,0}]//Simplify*)


(* ::Input:: *)
(*{-\[Sqrt](r^4-Tu^2 Tv^2+(r^4+Tu^2 Tv^2) Cosh[lu Tu]-2 r^2 Tu Tv Coth[lv Tv] Sinh[lu Tu]) \[Sqrt](-r^4+Tu^2 Tv^2+(r^4+Tu^2 Tv^2) Cosh[lu Tu]-2 r^2 Tu Tv Coth[lv Tv] Sinh[lu Tu])+Csch[lv Tv] Sinh[lu Tu] \[Sqrt](r^4-Tu^2 Tv^2+(r^4+Tu^2 Tv^2) Cosh[lv Tv]-2 r^2 Tu Tv Coth[lu Tu] Sinh[lv Tv]) \[Sqrt](-r^4+Tu^2 Tv^2+(r^4+Tu^2 Tv^2) Cosh[lv Tv]-2 r^2 Tu Tv Coth[lu Tu] Sinh[lv Tv])==0,*)
(*Csch[lu Tu] \[Sqrt](r^4-Tu^2 Tv^2+(r^4+Tu^2 Tv^2) Cosh[lu Tu]-2 r^2 Tu Tv Coth[lv Tv] Sinh[lu Tu]) \[Sqrt](-r^4+Tu^2 Tv^2+(r^4+Tu^2 Tv^2) Cosh[lu Tu]-2 r^2 Tu Tv Coth[lv Tv] Sinh[lu Tu]) Sinh[lv Tv]-Sqrt[r^4-Tu^2 Tv^2+(r^4+Tu^2 Tv^2) Cosh[lv Tv]-2 r^2 Tu Tv Coth[lu Tu] Sinh[lv Tv]] \[Sqrt](-r^4+Tu^2 Tv^2+(r^4+Tu^2 Tv^2) Cosh[lv Tv]-2 r^2 Tu Tv Coth[lu Tu] Sinh[lv Tv])==0}//Simplify*)
(**)


(* ::Input:: *)
(*{(\[Sqrt](r^4-Tu^2 Tv^2+(r^4+Tu^2 Tv^2) Cosh[lu Tu]-2 r^2 Tu Tv Coth[lv Tv] Sinh[lu Tu]) \[Sqrt](-r^4+Tu^2 Tv^2+(r^4+Tu^2 Tv^2) Cosh[lu Tu]-2 r^2 Tu Tv Coth[lv Tv] Sinh[lu Tu]))^2==(Csch[lv Tv] Sinh[lu Tu] \[Sqrt](r^4-Tu^2 Tv^2+(r^4+Tu^2 Tv^2) Cosh[lv Tv]-2 r^2 Tu Tv Coth[lu Tu] Sinh[lv Tv]) \[Sqrt](-r^4+Tu^2 Tv^2+(r^4+Tu^2 Tv^2) Cosh[lv Tv]-2 r^2 Tu Tv Coth[lu Tu] Sinh[lv Tv]))^2,(\[Sqrt](r^4-Tu^2 Tv^2+(r^4+Tu^2 Tv^2) Cosh[lv Tv]-2 r^2 Tu Tv Coth[lu Tu] Sinh[lv Tv]) \[Sqrt](-r^4+Tu^2 Tv^2+(r^4+Tu^2 Tv^2) Cosh[lv Tv]-2 r^2 Tu Tv Coth[lu Tu] Sinh[lv Tv]))^2==(Csch[lu Tu] \[Sqrt](r^4-Tu^2 Tv^2+(r^4+Tu^2 Tv^2) Cosh[lu Tu]-2 r^2 Tu Tv Coth[lv Tv] Sinh[lu Tu]) \[Sqrt](-r^4+Tu^2 Tv^2+(r^4+Tu^2 Tv^2) Cosh[lu Tu]-2 r^2 Tu Tv Coth[lv Tv] Sinh[lu Tu]) Sinh[lv Tv])^2}//Simplify*)


(* ::Subsubsection:: *)
(*Charges: basics*)


(* ::Item:: *)
(*Check that we've used the correct metric, and define the variation:*)


(* ::Input::Initialization:: *)
coord
metric=btz`metric
<<diffgeoM`

\[Delta]=(\[Delta]Tv \!\(
\*SubscriptBox[\(\[PartialD]\), \(Tv\)]#\)+\[Delta]Tu \!\(
\*SubscriptBox[\(\[PartialD]\), \(Tu\)]#\))&;
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
(*Charges: black hole*)


(* ::Text:: *)
(*Quick check (finite Tu, Tv):*)


(* ::Input:: *)
(*Simplify[2\[Pi] \[Delta]\[Chi][#,\[Delta]g]&/@{\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(u\)]coord\),-\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(v\)]coord\)}]*)


(* ::Text:: *)
(*Energy: \!\( *)
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
(*Entropy: usual AdS*)


(* ::Input::Initialization:: *)
\[Delta]\[Chi]0=\[Delta]\[Chi][\[Xi],\[Delta]g]//Simplify


(* ::Input::Initialization:: *)
Dt@ReleaseHold@RTslice/.constParams
Solve[%,Dt[v]]//Flatten
RTdv2du=%[[1]];

Solve[%%%,Dt[u]]//Flatten
RTdu2dv=%[[1]];


(* ::Input::Initialization:: *)
\[Delta]dq=\[Delta]\[Chi]0.{Dt[u],Dt[v],Dt[r]}/.constParams//Simplify


(* ::Item:: *)
(*If we integrate along the RT surface and convert \[DifferentialD]x^\[Alpha]->\[DifferentialD]u, then the integrand is given by:*)


(* ::Input::Initialization:: *)
\[Delta]dqRT=\[Delta]dq/.RTuv/.constParams;


(* ::Input::Initialization:: *)
\[Delta]dqRTu=\[Delta]dqRT/.RTdv2du/.RTv2u//Simplify


(* ::Item:: *)
(*Similarly, we can do  \!\(TraditionalForm\`\[DifferentialD]*)
(*\*SuperscriptBox[\(x\), \(\[Alpha]\)] -> \[DifferentialD]v\):*)


(* ::Input::Initialization:: *)
\[Delta]dqRTv=\[Delta]dqRT/.RTdu2dv/.RTu2v//Simplify


(* ::Item:: *)
(*If we integrate along some constant \[Rho] line, we get:*)


(* ::Input::Initialization:: *)
\[Delta]dqStokes=\[Delta]\[Chi]0.{Dt[u],Dt[v],0}/.RTdv2du/.RTv2u//Simplify
(\[Delta]dqStokes==\[Delta]dqRTu)//Simplify


(* ::Item:: *)
(*They actually agree with each other; this is the first law!*)


(* ::Item:: *)
(*We then integrate along the u direction:*)


(* ::Input:: *)
(*\[Integral]Evaluate[\[Delta]dqRTu/.Dt[u]->1]\[DifferentialD]u//Simplify*)
(**)
(*%/.{{u->lu/2},{u->-lu/2}}*)
(**)
(*%//Apply[Subtract]//Simplify*)


(* ::Input::Initialization:: *)
\[Delta]q\[Infinity]=(-Tv \[Delta]Tu-Tu \[Delta]Tv+lu Tu Tv \[Delta]Tu Coth[lu Tu]+lv Tu Tv \[Delta]Tv Coth[lv Tv])/(4 G Tu Tv);


(* ::Input::Initialization:: *)
q\[Infinity]=1/(4G) (Log[Sinh[Tu lu]/(\[Epsilon] Tu)]+Log[Sinh[Tv lv]/(\[Epsilon] Tv)]);


(* ::Input:: *)
(*\[Delta]@q\[Infinity]==\[Delta]q\[Infinity]//Simplify*)


(* ::Input:: *)
(*q\[Infinity]/.sameT/.sameL*)
(**)
(*Limit[%,T->0]*)


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


(* ::Input::Initialization:: *)
\[Integral]1/Sqrt[(r^2+Tu^2) (r^2+Tv^2)] \[DifferentialD]r//Exp//FullSimplify
conformal[r_]=Re[%];


(* ::Item:: *)
(*This is a conformally flat radial coordinate!*)


(* ::Input:: *)
(*Plot[conformal[r]/.sameT/.T->.3,{r,-1,1}]*)


(* ::Input:: *)
(**)


(* ::Subsubsection::Closed:: *)
(*Radial cutoff*)


(* ::Input:: *)
(*Plot[r/.RTsameT/.{L->2,T->1}//Evaluate,{x,-\[Pi],\[Pi]},PlotRange->{0,Automatic}]*)


(* ::Item:: *)
(*The radial cutoff:*)


(* ::Input:: *)
(*r2/.Solve[(r/.karlToBanados/.r->Sqrt[r2])==rKarl,r2]//Flatten*)
(**)
(*(%[[2]]>%[[1]])//Simplify*)


(* ::Input::Initialization:: *)
banadosToKarl=r->Sqrt[1/2 (r^2-Tu^2-Tv^2+Sqrt[-4 Tu^2 Tv^2+(-r^2+Tu^2+Tv^2)^2])];


(* ::Input::Initialization:: *)
\[Rho]c=r/.banadosToKarl/.r->rc//FullSimplify


(* ::Subsubsection::Closed:: *)
(*Cutoff geometry: pictures*)


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


(* ::Subsubsection::Closed:: *)
(*Cutoff coordinates from RT*)


(* ::Item:: *)
(*Cutoff with same Tand L:*)


(* ::Input:: *)
(*rc->(r/.karlToBanados/.sameT/.RTsameT//Simplify)*)
(**)
(*rc/Sqrt[rc^2-4T^2]/.%//Simplify*)


(* ::Item:: *)
(*For the general case, again we use the special case to pick out the right solution:*)


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
luc->1/Tu ArcCosh[Cosh[Tu lu] (rc^2-Tu^2-Tv^2-2 Tu Tv Tanh[lu Tu]/Tanh[lv Tv])/Sqrt[rc^4+(Tu^2-Tv^2)^2-2 rc^2 (Tu^2+Tv^2)]],
lvc->1/Tv ArcCosh[Cosh[Tv lv] (rc^2-Tu^2-Tv^2-2 Tu Tv Tanh[lv Tv]/Tanh[lu Tu])/Sqrt[rc^4+(Tu^2-Tv^2)^2-2 rc^2 (Tu^2+Tv^2)]]
};

atCutoff={u->luc/2,v->lvc/2};


(* ::Item:: *)
(*Check that this is indeed the solution:*)


(* ::Input:: *)
(*#^2&/@ReleaseHold[RTslice]/.atCutoff/.lc//Simplify*)


(* ::Input:: *)
(*r^2/.karlToBanados/.RTuv/.atCutoff/.lc//FullSimplify*)


(* ::Item:: *)
(*This agrees with the r parametrization of the RT surface:*)


(* ::Input:: *)
(*\[Rho]c*)


(* ::Input:: *)
(*uRTr/.r->\[Rho]c//FullSimplify*)


(* ::Subsubsection:: *)
(*From Subscript[l, u] to Subscript[l, u,c]: TODO!*)


(* ::Subsection:: *)
(*Entropy: cutoff AdS*)


(* ::Subsubsection::Closed:: *)
(*Charge variation: na\[IDoubleDot]ve*)


(* ::Input:: *)
(*\[Delta]dq*)


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
(*\[Integral]Evaluate[\[Delta]dqRTu/.Dt[u]->1]\[DifferentialD]u//Simplify;*)
(*%/.{{u->luc/2},{u->-luc/2}};*)
(*%//Apply[Subtract]//Simplify*)
(**)
(*(RTu2v/.atCutoff)//Map[2#&]*)
(*ArcSinh[Csch[lu Tu] Sinh[luc Tu] Sinh[lv Tv]]/.%//Simplify*)


(* ::Input:: *)
(*\[Integral]Evaluate[\[Delta]dqRTv/.Dt[v]->1]\[DifferentialD]v//Simplify;*)
(*%/.{{v->lvc/2},{v->-lvc/2}};*)
(*%//Apply[Subtract]//Simplify*)
(**)
(*(RTv2u/.atCutoff)//Map[2#&]*)
(*ArcSinh[Csch[lv Tv] Sinh[lu Tu] Sinh[lvc Tv]]/.%//Simplify*)


(* ::Input:: *)
(*\[Delta]qc=1/(4 G) (luc \[Delta]Tu Coth[lu Tu]+lvc \[Delta]Tv Coth[lv Tv]-((Tv \[Delta]Tu+Tu \[Delta]Tv)/(Tu Tv)) (Sinh[Tu luc]/Sinh[Tu lu]+Sinh[Tv lvc]/Sinh[Tv lv])/2);*)


(* ::Input::Initialization:: *)
\[Delta]qc=1/(4 G) (luc \[Delta]Tu Coth[lu Tu]+lvc \[Delta]Tv Coth[lv Tv]-((Tv \[Delta]Tu+Tu \[Delta]Tv)/(Tu Tv))Sqrt[Sinh[Tu luc]/Sinh[Tu lu] Sinh[Tv lvc]/Sinh[Tv lv]]);


(* ::Input:: *)
(*(\[Delta]qc/.{luc->lu,lvc->lv})==\[Delta]q\[Infinity]//Simplify*)


(* ::Item:: *)
(*Note that here Subscript[l, u] depends on Subscript[T, u,v] implicitly!*)


(* ::Subsubsection::Closed:: *)
(*Charge proposal*)


(* ::Item:: *)
(*A guess for the integrated charge:*)


(* ::Input:: *)
(*q\[Infinity]*)


(* ::Input:: *)
(*Series[ArcSinh[x/2],{x,\[Infinity],1}]*)


(* ::Input:: *)
(*qc=1/(4G) (ArcSinh[(rc Sinh[Tu luc])/(2Tu)]+ArcSinh[(rc Sinh[Tv lvc])/(2Tv)])//Simplify;*)
(*qc/.sameT/.{luc->lc,lvc->lc}*)


(* ::Input::Initialization:: *)
qc=1/(2G) (ArcSinh[rc/2 Sqrt[ Sinh[Tu luc]Sinh[Tv lvc]]/Sqrt[Tu Tv]])//Simplify;
qc/.sameT/.{luc->lc,lvc->lc}//FullSimplify


(* ::Input:: *)
(*ArcSinh[(rc Sinh[lc T])/(2 T)]/(2 G)==ArcSinh[(rc Sinh[lc T])/(2 T)]/(2 G)//Simplify*)


(* ::Subsubsection::Closed:: *)
(*Same T check: failed*)


(* ::Item:: *)
(*First look at the special case:*)


(* ::Input:: *)
(*lc2l*)


(* ::Input:: *)
(*\[Delta]@qc/.{luc->lc,lvc->lc}/.sameT/.{\[Delta]Tu->\[Delta]T,\[Delta]Tv->\[Delta]T}//Simplify*)


(* ::Input:: *)
(*\[Delta]qc*)


(* ::Item:: *)
(*Relation for constant T:*)


(* ::Input:: *)
(*rc->(r/.karlToBanados/.sameT/.RTsameT//Simplify)*)
(**)
(*rc/Sqrt[rc^2-4T^2]/.%/.x->luc/2//Simplify*)


(* ::Input:: *)
(*(\[Delta]@qc/.lc2l/.sameL/.sameT/.{\[Delta]Tu->\[Delta]T,\[Delta]Tv->\[Delta]T})/(1/(2G) \[Delta]T/T)//FullSimplify[#,{-rc^2+(rc^2-4 T^2) Cosh[L T]^2>0}]&*)
(*(\[Delta]qc/.lc2l/.sameL/.sameT/.{\[Delta]Tu->\[Delta]T,\[Delta]Tv->\[Delta]T})/(1/(2G) \[Delta]T/T)//FullSimplify*)


(* ::Item:: *)
(*Unfortunately, they do not agree! Explicit checks:*)


(* ::Input:: *)
(*Sech[-I ArcSec[x]]//Simplify*)


(* ::Input:: *)
(*(-(rc^2/(rc^2-4 T^2))+Cosh[L T]^2 )Csch[L T]^2==(rc^2-4 T^2-4 T^2 Csch[L T]^2)/rc^2//Simplify*)


(* ::Input:: *)
(*Cosh[2 L T]==2Cosh[L T]^2-1//Simplify*)


(* ::Input:: *)
(*(rc^2-4 T^2) (2Cosh[L T]^2-1)-(rc^2+4 T^2)//Expand//Simplify*)


(* ::Input:: *)
(* rc^2== (rc^2-4 T^2) Cosh[L T]^2*)


(* ::Item:: *)
(*Not the same! It seems that one needs to*)


(* ::Subitem:: *)
(*keep track of the change of cutoff Subscript[\[Rho], c], or*)


(* ::Subitem:: *)
(*switch to a better coord*)


(* ::Subsubsection:: *)
(*Banados variation*)


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
(*u->(u+v)/2+(Tu^2-Tv^2+Sqrt[r^4+(Tu^2-Tv^2)^2-2 r^2 (Tu^2+Tv^2)])/r^2 (u-v)/2,*)
(*v->(u+v)/2+(Tu^2-Tv^2-Sqrt[r^4+(Tu^2-Tv^2)^2-2 r^2 (Tu^2+Tv^2)])/r^2 (u-v)/2*)
(*}/.karlToBanados//Simplify*)


(* ::Item:: *)
(*Conventional: \[Rho] independent:*)


(* ::Input::Initialization:: *)
uvNewByOld={
u->(u+v)/2+(Tu^2-Tv^2+Sqrt[rc^4+(Tu^2-Tv^2)^2-2 rc^2 (Tu^2+Tv^2)])/rc^2 (u-v)/2,
v->(u+v)/2+(Tu^2-Tv^2-Sqrt[rc^4+(Tu^2-Tv^2)^2-2 rc^2 (Tu^2+Tv^2)])/rc^2 (u-v)/2
};


(* ::Input::Initialization:: *)
uvOldByNew=Solve[{
U==u,V==v
}/.uvNewByOld//Evaluate,
{u,v}]/.{U->u,V->v}//Flatten//Simplify


(* ::Item::Closed:: *)
(*Banados with rescaled u,v and unchanged \[Rho]: not good!*)


(* ::Input:: *)
(*metricFormRescaled=#.g.#&[Dt/@coord]/.uvOldByNew/.constParams/.Dt[rc]->0//Simplify*)
(**)
(*%/.r->\[Rho]c/.constParams/.Dt[rc]->0//Simplify*)
(**)
(*Coefficient[metricFormRescaled,Dt[r]^2]*)
(*Coefficient[metricFormRescaled,Dt[u]^2]//Simplify*)
(*Coefficient[metricFormRescaled,Dt[u]Dt[v]]//Simplify*)


(* ::Input:: *)
(*\[Delta][metricFormRescaled]/.uvNewByOld/.constParams/.Dt[rc]->0//Simplify;*)
(*\[Delta]gc=Table[1/2 D[%,Dt[i],Dt[j]],{i,coord},{j,coord}]//Simplify*)
(**)
(*#.(%/.r->\[Rho]c).#&[Dt/@coord]//Simplify*)
(*Coefficient[%,Dt[u]Dt[v]]//Simplify*)


(* ::Item:: *)
(*Note that [\[Delta]] and [restriction to the boundary] do not commute!*)


(* ::Subitem:: *)
(*This is not unexpected, since Subscript[\[Rho], c] moves as we vary Subscript[T, u,v]*)


(* ::Subitem:: *)
(*\[Rho] should be rescaled as well to cancel this!*)


(* ::Input:: *)
(*banadosToKarl*)


(* ::Item:: *)
(*Rescale \[Rho] linearly such that the new Subscript[\[Rho], c] is just Subscript[r, c]*)


(* ::Item:: *)
(*This differs from Schwarzschild; for in that case the relation is non-linear, but here it is linear*)


(* ::Input::Initialization:: *)
metricFormRescaled=#.g.#&[Dt/@coord]/.uvOldByNew/.r->\[Rho]c/rc r/.constParams/.Dt[rc]->0//Simplify
%/.r->rc/.constParams/.Dt[rc]->0//Simplify
Coefficient[metricFormRescaled,Dt[r]^2]//Simplify


(* ::Item:: *)
(*Vary new coord >> convert back to old*)


(* ::Input::Initialization:: *)
\[Delta][metricFormRescaled]/.r->rc/\[Rho]c r/.uvNewByOld/.constParams/.Dt[rc]->0//Simplify;
\[Delta]gc=Table[1/2 D[%,Dt[i],Dt[j]],{i,coord},{j,coord}]//Simplify

#.(%/.r->\[Rho]c).#&[Dt/@coord]//Simplify


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
(*Schwarzschild variation*)


(* ::Item:: *)
(*Or maybe just use Schwarzschild (need to understand why it works: what's the boundary condition & phase space):*)


(* ::Input:: *)
(*\[Delta][#.g.#&[Dt/@coord]/.banadosToKarl/.constParams//Simplify];*)
(*Table[1/2 D[%,Dt[i],Dt[j]],{i,coord},{j,coord}]//Simplify*)
(**)
(*%%/.karlToBanados/.constParams/.Dt[rc]->0//Simplify;*)
(*\[Delta]gc=Table[1/2 D[%,Dt[i],Dt[j]],{i,coord},{j,coord}]//Simplify*)
(**)
(*%/.r->\[Rho]c//Simplify*)


(* ::Input:: *)
(*\[Delta]g/.r->\[Rho]c//Simplify*)


(* ::Subsubsection:: *)
(*Charge*)


(* ::Input::Initialization:: *)
\[Delta]dqC=\[Delta]\[Chi][\[Xi],\[Delta]gc].{Dt[u],Dt[v],Dt[r]}/.constParams//Simplify


(* ::Input:: *)
(*(Csch[lu Tu] Sinh[2 Tu u]-Csch[lv Tv] Sinh[2 Tv v])/.RTv2u*)


(* ::Item:: *)
(*First let's try to integrate along the cutoff:*)


(* ::Input::Initialization:: *)
\[Delta]dqCt=\[Delta]dqC/.Dt[r]->0/.r->\[Rho]c/.{
u->1/(2Tu) ArcSinh[t Sinh[Tu lu]],
v->1/(2Tv) ArcSinh[t Sinh[Tv lv]]
}/.constParams/.Dt[t]->1//Simplify[#,{t>0}]&


(* ::Input:: *)
(*ArcSinh[Csch[lu Tu] Sinh[2 u Tu] Sinh[lv Tv]]/.RTu2v/.atCutoff//Simplify*)


(* ::Input::Initialization:: *)
\[Integral]Evaluate[\[Delta]dqCt/.Dt[t]->1]\[DifferentialD]t//Simplify;
%/.{{t->Sinh[Tu luc]/Sinh[Tu lu]},{t->-(Sinh[Tu luc]/Sinh[Tu lu])}};
\[Delta]qCu=((%//Apply[Subtract])/.ArcSinh[Csch[lu Tu] Sinh[luc Tu] Sinh[lv Tv]]->lvc Tv)//Simplify//Apart//ExpandAll//Simplify


(* ::Input::Initialization:: *)
\[Delta]qBanados=(Tu Tv (luc (rc^2-Tu^2-Tv^2) (rc^2 \[Delta]Tu-Tu^2 \[Delta]Tu-Tv^2 \[Delta]Tu+2 Tu Tv \[Delta]Tv)+2 lvc Tu Tv (-2 Tu Tv \[Delta]Tu+Tu^2 \[Delta]Tv+(-rc^2+Tv^2) \[Delta]Tv)) Coth[lu Tu]+Tu Tv (2 luc Tu Tv (-rc^2 \[Delta]Tu+Tu^2 \[Delta]Tu+Tv^2 \[Delta]Tu-2 Tu Tv \[Delta]Tv)+lvc (rc^2-Tu^2-Tv^2) (2 Tu Tv \[Delta]Tu-Tu^2 \[Delta]Tv+(rc^2-Tv^2) \[Delta]Tv)) Coth[lv Tv]-Sqrt[rc^2 Hold[f][rc]] (-(Tu^2-Tv^2) (-Tv \[Delta]Tu+Tu \[Delta]Tv)+rc^2 (Tv \[Delta]Tu+Tu \[Delta]Tv)) Csch[lu Tu] Sinh[luc Tu])/(4 G Tu Tv (rc^2 Hold[f][rc]))//FullSimplify


(* ::Input:: *)
(*\[Delta]qKarl-\[Delta]qBanados//ReleaseHold//FullSimplify*)


(* ::Item:: *)
(*They agree if Subscript[l, u]=Subscript[l, v]! For Subscript[l, u]!=Subscript[l, v] it's not obvious:*)


(* ::Input:: *)
(*((-2 Tu Tv \[Delta]Tu+(-rc^2+Tu^2+Tv^2) \[Delta]Tv) Coth[lu Tu]+(rc^2 \[Delta]Tu-(Tu^2+Tv^2) \[Delta]Tu+2 Tu Tv \[Delta]Tv) Coth[lv Tv])==0//FullSimplify*)


(* ::Item:: *)
(*Try to do the phase space integral:*)


(* ::Input:: *)
(*\[Delta]qBanados//ReleaseHold//Simplify;*)
(*coefTu=Coefficient[%,\[Delta]Tu];*)
(*coefTv=Coefficient[%%,\[Delta]Tv];*)
(**)
(*coefTu \[Delta]Tu+coefTv \[Delta]Tv==%%%//Reduce*)


(* ::Input:: *)
(*\[Integral]coefTu \[DifferentialD]Tu*)


(* ::Subsubsection:: *)
(*Compare with proposal*)


(* ::Input:: *)
(*\[Delta]@qc//Simplify*)


(* ::Input:: *)
(*Csch[luc Tu] Csch[lvc Tv]/.lc2l*)


(* ::Input:: *)
(*(-1+(Cosh[lu Tu] (rc^2-Tu^2-Tv^2-2 Tu Tv Coth[lv Tv] Tanh[lu Tu]))/Sqrt[rc^4+(Tu^2-Tv^2)^2-2 rc^2 (Tu^2+Tv^2)])(1+(Cosh[lu Tu] (rc^2-Tu^2-Tv^2-2 Tu Tv Coth[lv Tv] Tanh[lu Tu]))/Sqrt[rc^4+(Tu^2-Tv^2)^2-2 rc^2 (Tu^2+Tv^2)])  (-1+(Cosh[lv Tv] (rc^2-Tu^2-Tv^2-2 Tu Tv Coth[lu Tu] Tanh[lv Tv]))/Sqrt[rc^4+(Tu^2-Tv^2)^2-2 rc^2 (Tu^2+Tv^2)])(1+(Cosh[lv Tv] (rc^2-Tu^2-Tv^2-2 Tu Tv Coth[lu Tu] Tanh[lv Tv]))/Sqrt[rc^4+(Tu^2-Tv^2)^2-2 rc^2 (Tu^2+Tv^2)])//ExpandAll//FullSimplify*)


(* ::Input:: *)
(*(8 Tu Tv (-rc^2+Tu^2+Tv^2) Cosh[lu Tu] Cosh[lv Tv]+8 Tu^2 Tv^2 Csch[lv Tv] Sinh[lu Tu]+(-(rc-Tu-Tv) (rc+Tu-Tv) (rc-Tu+Tv) (rc+Tu+Tv)+(rc^4+Tu^4+6 Tu^2 Tv^2+Tv^4-2 rc^2 (Tu^2+Tv^2)) Cosh[2 lu Tu]) Csch[lu Tu] Sinh[lv Tv])/(2(rc+Tu-Tv) (rc-Tu+Tv) (-rc+Tu+Tv)(rc+Tu+Tv));*)
(**)
(*(rc (-Tv \[Delta]Tu-Tu \[Delta]Tv+luc Tu Tv \[Delta]Tu Coth[luc Tu]+lvc Tu Tv \[Delta]Tv Coth[lvc Tv]))/(4 G Tu Tv Sqrt[rc^2+4 Tu Tv/%])//FullSimplify*)


(* ::Subsubsection:: *)
(*Result from Schwarzschild*)


(* ::Input:: *)
(*\[Delta]qSchwarzschild=(luc Tu Tv (rc^4+Tu^4-2 Tu^2 Tv^2+Tv^4-Tu^2 Sqrt[rc^4+(Tu^2-Tv^2)^2-2 rc^2 (Tu^2+Tv^2)]-Tv^2 Sqrt[rc^4+(Tu^2-Tv^2)^2-2 rc^2 (Tu^2+Tv^2)]+rc^2 (-2 Tu^2-2 Tv^2+Sqrt[rc^4+(Tu^2-Tv^2)^2-2 rc^2 (Tu^2+Tv^2)])) \[Delta]Tu Coth[lu Tu]+Tu (rc^4+Tu^4-2 Tu^2 Tv^2+Tv^4-Tu^2 Sqrt[rc^4+(Tu^2-Tv^2)^2-2 rc^2 (Tu^2+Tv^2)]-Tv^2 Sqrt[rc^4+(Tu^2-Tv^2)^2-2 rc^2 (Tu^2+Tv^2)]+rc^2 (-2 Tu^2-2 Tv^2+Sqrt[rc^4+(Tu^2-Tv^2)^2-2 rc^2 (Tu^2+Tv^2)])) \[Delta]Tv lvc Tv Coth[lv Tv]-(rc^2-Tu^2-Tv^2+Sqrt[rc^4+(Tu^2-Tv^2)^2-2 rc^2 (Tu^2+Tv^2)]) (-(Tu^2-Tv^2) (-Tv \[Delta]Tu+Tu \[Delta]Tv)+rc^2 (Tv \[Delta]Tu+Tu \[Delta]Tv)) Csch[lu Tu] Sinh[luc Tu])/(4 G Tu Tv (-4 Tu^2 Tv^2+(-rc^2+Tu^2+Tv^2)^2+rc^2 Sqrt[rc^4+(Tu^2-Tv^2)^2-2 rc^2 (Tu^2+Tv^2)]-Tu^2 Sqrt[rc^4+(Tu^2-Tv^2)^2-2 rc^2 (Tu^2+Tv^2)]-Tv^2 Sqrt[rc^4+(Tu^2-Tv^2)^2-2 rc^2 (Tu^2+Tv^2)]));*)


(* ::Input::Initialization:: *)
f=Function[r,r^2-2(Tu^2+Tv^2)+(Tu^2-Tv^2)^2/r^2];


(* ::Input:: *)
(*Sqrt[rc^2 Hold[f][rc]]==Sqrt[rc^4+(Tu^2-Tv^2)^2-2 rc^2 (Tu^2+Tv^2)]//ReleaseHold//Simplify*)


(* ::Input:: *)
(*Hold[f][r]//ReleaseHold*)


(* ::Input:: *)
(*(luc Tu Tv (rc^4+Tu^4-2 Tu^2 Tv^2+Tv^4-Tu^2 Sqrt[rc^2 Hold[f][rc]]-Tv^2 Sqrt[rc^2 Hold[f][rc]]+rc^2 (-2 Tu^2-2 Tv^2+Sqrt[rc^2 Hold[f][rc]])) \[Delta]Tu Coth[lu Tu]+Tu (rc^4+Tu^4-2 Tu^2 Tv^2+Tv^4-Tu^2 Sqrt[rc^2 Hold[f][rc]]-Tv^2 Sqrt[rc^2 Hold[f][rc]]+rc^2 (-2 Tu^2-2 Tv^2+Sqrt[rc^2 Hold[f][rc]])) \[Delta]Tv Tv lvc Coth[lv Tv]-(rc^2-Tu^2-Tv^2+Sqrt[rc^2 Hold[f][rc]]) (-(Tu^2-Tv^2) (-Tv \[Delta]Tu+Tu \[Delta]Tv)+rc^2 (Tv \[Delta]Tu+Tu \[Delta]Tv)) Csch[lu Tu] Sinh[luc Tu])/(4 G Tu Tv (-4 Tu^2 Tv^2+(-rc^2+Tu^2+Tv^2)^2+rc^2 Sqrt[rc^2 Hold[f][rc]]-Tu^2 Sqrt[rc^2 Hold[f][rc]]-Tv^2 Sqrt[rc^2 Hold[f][rc]]))//Simplify*)
(**)
(*\[Delta]qSchwarzschild==%//ReleaseHold//Simplify*)


(* ::Input::Initialization:: *)
\[Delta]qKarl=1/(4G) (luc \[Delta]Tu Coth[lu Tu]+lvc \[Delta]Tv Coth[lv Tv]
-(((-Tu^2+Tv^2) (-Tv \[Delta]Tu+Tu \[Delta]Tv)+rc^2 (Tv \[Delta]Tu+Tu \[Delta]Tv)) Csch[lu Tu] Sinh[luc Tu] (rc^2-Tu^2-Tv^2+Sqrt[rc^2 Hold[f][rc]]))/(Tu Tv (rc^2 Hold[f][rc]+(rc^2-Tu^2-Tv^2) Sqrt[rc^2 Hold[f][rc]])));


(* ::Input:: *)
(*\[Delta]qKarl==\[Delta]qSchwarzschild//ReleaseHold//Simplify*)


(* ::Item:: *)
(*This is a simplified version of \[Delta]qC.*)


(* ::Item:: *)
(*Pick out the coefficient:*)


(* ::Input:: *)
(*\[Delta]qC;*)
(*Coefficient[%,\[Delta]Tu]\[Delta]Tu+Coefficient[%,\[Delta]Tv]\[Delta]Tv==%//Simplify*)
(**)
(*tuCoeff=Coefficient[\[Delta]qC,\[Delta]Tu]//Simplify*)
(*tvCoeff=Coefficient[\[Delta]qC,\[Delta]Tv]//Simplify*)


(* ::Input:: *)
(**)


(* ::Input:: *)
(*qc*)


(* ::Input:: *)
(*\[Delta]@qc//Simplify*)
(*Coefficient[%,\[Delta]Tu]\[Delta]Tu+Coefficient[%,\[Delta]Tv]\[Delta]Tv==%//Simplify*)
(**)
(*tuRef=Coefficient[%%,\[Delta]Tu]*)


(* ::Input:: *)
(*(rc luc Tu Tv Coth[luc Tu])/(4 G Tu Tv Sqrt[rc^2+4 Tu Tv Csch[luc Tu] Csch[lvc Tv]])==(luc Coth[lu Tu])/(4G)//Simplify[#,G>0]&*)


(* ::Input:: *)
(*Csch[luc Tu] Csch[lvc Tv]/.{luc->ArcCosh[(Cosh[lu Tu] (rc^2-Tu^2-Tv^2-2 Tu Tv Coth[lv Tv] Tanh[lu Tu]))/Sqrt[rc^2 Hold[f][rc]]]/Tu,lvc->ArcCosh[(Cosh[lv Tv] (rc^2-Tu^2-Tv^2-2 Tu Tv Coth[lu Tu] Tanh[lv Tv]))/Sqrt[rc^2 Hold[f][rc]]]/Tv}*)


(* ::Input:: *)
(*1/(\[Sqrt]((-1+(Cosh[lu Tu] (rc^2-Tu^2-Tv^2-2 Tu Tv Coth[lv Tv] Tanh[lu Tu]))/Sqrt[rc^2 Hold[f][rc]])(1+(Cosh[lu Tu] (rc^2-Tu^2-Tv^2-2 Tu Tv Coth[lv Tv] Tanh[lu Tu]))/Sqrt[rc^2 Hold[f][rc]]))  \[Sqrt]((-1+(Cosh[lv Tv] (rc^2-Tu^2-Tv^2-2 Tu Tv Coth[lu Tu] Tanh[lv Tv]))/Sqrt[rc^2 Hold[f][rc]])(1+(Cosh[lv Tv] (rc^2-Tu^2-Tv^2-2 Tu Tv Coth[lu Tu] Tanh[lv Tv]))/Sqrt[rc^2 Hold[f][rc]])) )//Simplify*)


(* ::Input:: *)
(*(rc^2 Hold[f][rc])/(\[Sqrt]((1/4 (2 (-rc^2+Tu^2+Tv^2) Cosh[lv Tv]+4 Tu Tv Coth[lu Tu] Sinh[lv Tv])^2-rc^2 Hold[f][rc])(1/4 (2 (-rc^2+Tu^2+Tv^2) Cosh[lu Tu]+4 Tu Tv Coth[lv Tv] Sinh[lu Tu])^2-rc^2 Hold[f][rc])))//Simplify*)


(* ::Input:: *)
(*Coth[lu Tu]==(rc Coth[luc Tu])/Sqrt[rc^2+4 Tu Tv Csch[luc Tu] Csch[lvc Tv]]/.{luc->ArcCosh[(Cosh[lu Tu] (rc^2-Tu^2-Tv^2-2 Tu Tv Coth[lv Tv] Tanh[lu Tu]))/Sqrt[rc^2 Hold[f][rc]]]/Tu,lvc->ArcCosh[(Cosh[lv Tv] (rc^2-Tu^2-Tv^2-2 Tu Tv Coth[lu Tu] Tanh[lv Tv]))/Sqrt[rc^2 Hold[f][rc]]]/Tv}//Simplify*)


(* ::Input:: *)
(*lc2l*)


(* ::Input:: *)
(*{luc->ArcCosh[(Cosh[lu Tu] (rc^2-Tu^2-Tv^2-2 Tu Tv Coth[lv Tv] Tanh[lu Tu]))/Sqrt[rc^2 Hold[f][rc]]]/Tu,lvc->ArcCosh[(Cosh[lv Tv] (rc^2-Tu^2-Tv^2-2 Tu Tv Coth[lu Tu] Tanh[lv Tv]))/Sqrt[rc^2 Hold[f][rc]]]/Tv}*)


(* ::Input:: *)
(*tuRef/.lc2l//Simplify*)


(* ::Input:: *)
(*tuCoeff/.lc2l*)


(* ::Input:: *)
(*\[Sqrt]((1+(Cosh[lu Tu] (rc^2-Tu^2-Tv^2-2 Tu Tv Coth[lv Tv] Tanh[lu Tu]))/Sqrt[rc^4+(Tu^2-Tv^2)^2-2 rc^2 (Tu^2+Tv^2)])^2 (-1+(Cosh[lu Tu] (rc^2-Tu^2-Tv^2-2 Tu Tv Coth[lv Tv] Tanh[lu Tu]))/Sqrt[rc^4+(Tu^2-Tv^2)^2-2 rc^2 (Tu^2+Tv^2)])/(1+(Cosh[lu Tu] (rc^2-Tu^2-Tv^2-2 Tu Tv Coth[lv Tv] Tanh[lu Tu]))/Sqrt[rc^4+(Tu^2-Tv^2)^2-2 rc^2 (Tu^2+Tv^2)]))//Simplify*)


(* ::Input:: *)
(*Cosh[lu Tu]^2 (-rc^2+Tu^2+Tv^2+2 Tu Tv Coth[lv Tv] Tanh[lu Tu])^2-(rc^4+(Tu^2-Tv^2)^2-2 rc^2 (Tu^2+Tv^2))//Simplify*)


(* ::Input:: *)
(*Sqrt[rc^4+(Tu^2-Tv^2)^2-2 rc^2 (Tu^2+Tv^2)]//Simplify*)


(* ::Input:: *)
(*(ArcCosh[(Cosh[lu Tu] (rc^2-Tu^2-Tv^2-2 Tu Tv Coth[lv Tv] Tanh[lu Tu]))/Sqrt[rc^4+(Tu^2-Tv^2)^2-2 rc^2 (Tu^2+Tv^2)]] Coth[lu Tu])/(4 G Tu)+((rc^2+Tu^2-Tv^2) (-rc^2+Tu^2+Tv^2-Sqrt[rc^4+(Tu^2-Tv^2)^2-2 rc^2 (Tu^2+Tv^2)]) Csch[lu Tu] ((\[Sqrt](-rc^4-(Tu^2-Tv^2)^2+2 rc^2 (Tu^2+Tv^2)+Cosh[lu Tu]^2 (-rc^2+Tu^2+Tv^2+2 Tu Tv Coth[lv Tv] Tanh[lu Tu])^2))/(Sqrt[rc^4+(Tu^2-Tv^2)^2-2 rc^2 (Tu^2+Tv^2)])) ((Sqrt[rc^4+(Tu^2-Tv^2)^2-2 rc^2 (Tu^2+Tv^2)]+Cosh[lu Tu] (rc^2-Tu^2-Tv^2-2 Tu Tv Coth[lv Tv] Tanh[lu Tu]))/(Sqrt[rc^4+(Tu^2-Tv^2)^2-2 rc^2 (Tu^2+Tv^2)])))/(4 G Tu (rc^4+Tu^4-2 Tu^2 Tv^2+Tv^4-Tu^2 Sqrt[rc^4+(Tu^2-Tv^2)^2-2 rc^2 (Tu^2+Tv^2)]-Tv^2 Sqrt[rc^4+(Tu^2-Tv^2)^2-2 rc^2 (Tu^2+Tv^2)]+rc^2 (-2 Tu^2-2 Tv^2+Sqrt[rc^4+(Tu^2-Tv^2)^2-2 rc^2 (Tu^2+Tv^2)])))//Simplify*)


(* ::Input:: *)
(*(\[Delta]@qc-\[Delta]qC)/.lc2l//Simplify*)


(* ::Input:: *)
(*((\[Delta]@qc/.lc2l)/(1/(2G) 1/T))(*//Simplify(*[#,{-rc^2+(rc^2-4 T^2) Cosh[L T]^2>0}]&*)*)*)


(* ::Input:: *)
(*(\[Delta]qC/.lc2l)/(1/(2G) 1/T)//Simplify*)


(* ::Subsubsection::Closed:: *)
(*Same T case in Schwarzschild*)


(* ::Input:: *)
(*ref=ArcSech[(rc Sech[L T])/Sqrt[rc^2-4 T^2]] Coth[L T]-Sqrt[-(rc^2/(rc^2-4 T^2))+Cosh[L T]^2] Csch[L T];*)
(*(\[Delta]qKarl/.lc2l/.sameL/.sameT/.{\[Delta]Tu->\[Delta]T,\[Delta]Tv->\[Delta]T})/(1/(2G) \[Delta]T/T)//Simplify*)
(*%//FullSimplify*)
(*%-ref//Simplify*)


(* ::Input:: *)
(*-Cosh[L T]+(-rc Sqrt[1/(rc^2-4 T^2)]+Sqrt[(-(rc^2/(rc^2-4 T^2))+Cosh[L T]^2)((rc+Sqrt[rc^2-4 T^2] Cosh[L T])/(-rc+Sqrt[rc^2-4 T^2] Cosh[L T]))])==0//FullSimplify*)


(* ::Input:: *)
(*rc^2+Cosh[L T] (2 rc Sqrt[rc^2-4 T^2]+(rc^2-4 T^2) Cosh[L T])==(rc+Sqrt[rc^2-4 T^2] Cosh[L T])^2//FullSimplify*)


(* ::Input:: *)
(*rc^2/(rc^2-4T^2)==Cosh[L T]^2/Cosh[lc T]^2*)


(* ::Subsection:: *)
(*Ending*)


(* ::Input:: *)
(**)


(* ::Input:: *)
(**)


(* ::Input:: *)
(**)


(* ::Input:: *)
(**)


(* ::Input:: *)
(**)


(* ::Input:: *)
(**)


(* ::Input:: *)
(**)


(* ::Input:: *)
(**)


(* ::Input:: *)
(**)


(* ::Input:: *)
(**)


(* ::Input:: *)
(**)


(* ::Input:: *)
(**)


(* ::Input:: *)
(**)


(* ::Input:: *)
(**)


(* ::Input:: *)
(**)


(* ::Input:: *)
(**)


(* ::Input:: *)
(**)


(* ::Input::Initialization:: *)
<<"Physica/MathUtils.wl"
saveScript[];



