#!/usr/bin/env wolframscript
(* ::Package:: *)

(* ::Subsubsection:: *)
(*Synopsis - Poincar\[EAcute]*)


(* ::Text:: *)
(*This notebook:*)
(* - calculates the modular flow of a generic boosted interval in Poincar\[EAcute] Subscript[AdS, 3],*)
(* - ... from first principle, starting from the basic isometries;*)
(* - it matches Luis's results and further confirms them for a generic boosted interval.*)


(* ::Subsubsection:: *)
(*Setup*)


(* ::Input::Initialization:: *)
SetDirectory@NotebookDirectory[];
<<"Physica/MathUtils.wl"


(* ::Input::Initialization:: *)
<<Notation`
Symbolize[ParsedBoxWrapper[SubscriptBox["_", "_"]]];


(* ::Input::Initialization:: *)
dX[n_]:=Symbol["d"<>ToString[coord[[n]]]]


(* ::Input::Initialization:: *)
$Assumptions=And[u\[Element]Reals,v\[Element]Reals,z>=0,zc>0,L>0,Linf>0,\[Tau]\[Element]Reals,\[Theta]\[Element]Reals,r>Tu Tv,Tu>0,Tv>0,r>T^2,T>0,r>0,r^2>T^4];


(* ::Input::Initialization:: *)
coord={u,v,z};
dim=Length[coord];
metricSign=-1;

ds2=(+du dv+dz^2)/z^2;

metric=Table[1/2 D[ds2,dX[i],dX[j]],{i,1,dim},{j,1,dim}]
<<diffgeoM`


(* ::Input:: *)
(**)


(* ::Input:: *)
(*Einstein- metric*)


(* ::Input:: *)
(**)


(* ::Input:: *)
(*("DefaultPlotStyle"/.(Method/.Charting`ResolvePlotTheme[#2,#1]))/.Directive[x_,__]:>x&[Plot,"Web"]*)


(* ::Input:: *)
(**)


(* ::Subsubsection::Closed:: *)
(*Killing vectors*)


(* ::Text:: *)
(*Usual bulk Killings:*)


(* ::Item:: *)
(*Translation:*)


(* ::Input::Initialization:: *)
pu={1,0,0};
pv={0,1,0};


(* ::Input:: *)
(*lieD[#,g]&/@{pu,pv}*)


(* ::Item:: *)
(*Rotation (boost):*)


(* ::Input::Initialization:: *)
muv={u,-v,0};


(* ::Input:: *)
(*lieD[muv,g]*)


(* ::Text:: *)
(*Note: rotation / boost is very similar to scaling in lightcone coordinates. *)


(* ::Item:: *)
(*Scaling:*)


(* ::Input::Initialization:: *)
\[CapitalDelta]={u,v,z};


(* ::Input:: *)
(*lieD[\[CapitalDelta],g]*)


(* ::Item:: *)
(*Special conformal:*)


(* ::Input::Initialization:: *)
ku={z^2+u*v,0,0}-v \[CapitalDelta]
kv={0,z^2+u*v,0}-u \[CapitalDelta]


(* ::Input:: *)
(*lieD[#,g]&/@{ku,kv}*)


(* ::Input:: *)
(**)


(* ::Text:: *)
(*Combine bulk Killings to match the expected ones at the boundary:*)


(* ::Input::Initialization:: *)
bulkKillings=Function[
{u,v,z,apu,apv,a0,auv,aku,akv},
pu apu+ pv apv+\[CapitalDelta] a0+muv auv+ku aku+kv akv//Evaluate
];

bulkToBoundaryKillings=Function[
{u,v,apu,apv,a0,auv,aku,akv},
(bulkKillings[u,v,0,apu,apv,a0,auv,aku,akv]//Collect[#,{u,v}]&)[[;;-2]]//Evaluate
];


(* ::Text::Closed:: *)
(*Get coefficients for Subscript[j, u]*)


(* ::Input:: *)
(*(-u^(#+1){1,0})&/@Range[-1,1]*)
(*{*)
(*#==bulkToBoundaryKillings[u,v,apu,apv,a0,auv,aku,akv]*)
(*}&/@%*)
(*bulkKillings[[1]]*)


(* ::Input::Initialization:: *)
ju=Function[{n},\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
RowBox[{"bulkKillings", "[", 
RowBox[{"u", ",", "v", ",", "z", ",", 
RowBox[{"-", "1"}], ",", "0", ",", "0", ",", "0", ",", "0", ",", "0"}], "]"}], 
RowBox[{"n", "==", 
RowBox[{"-", "1"}]}]},
{
RowBox[{"bulkKillings", "[", 
RowBox[{"u", ",", "v", ",", "z", ",", "0", ",", "0", ",", 
RowBox[{
RowBox[{"-", "1"}], "/", "2"}], ",", 
RowBox[{
RowBox[{"-", "1"}], "/", "2"}], ",", "0", ",", "0"}], "]"}], 
RowBox[{"n", "==", "0"}]},
{
RowBox[{"bulkKillings", "[", 
RowBox[{"u", ",", "v", ",", "z", ",", "0", ",", "0", ",", "0", ",", "0", ",", "0", ",", "1"}], "]"}], 
RowBox[{"n", "==", "1"}]}
},
AllowedDimensions->{2, Automatic},
Editable->True,
GridBoxAlignment->{"Columns" -> {{Left}}, "ColumnsIndexed" -> {}, "Rows" -> {{Baseline}}, "RowsIndexed" -> {}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "ColumnsIndexed" -> {}, "Rows" -> {{1.}}, "RowsIndexed" -> {}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.84]}, Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}, "RowsIndexed" -> {}},
Selectable->True]}
},
GridBoxAlignment->{"Columns" -> {{Left}}, "ColumnsIndexed" -> {}, "Rows" -> {{Baseline}}, "RowsIndexed" -> {}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "ColumnsIndexed" -> {}, "Rows" -> {{1.}}, "RowsIndexed" -> {}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.35]}, Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}, "RowsIndexed" -> {}}],
"Piecewise",
DeleteWithContents->True,
Editable->False,
SelectWithContents->True,
Selectable->False,
StripWrapperBoxes->True]\)//Evaluate]


(* ::Text::Closed:: *)
(*Get coefficients for Subscript[j, v]*)


(* ::Input:: *)
(*(-v^(#+1){0,1})&/@Range[-1,1]*)
(*{*)
(*#==bulkToBoundaryKillings[u,v,apu,apv,a0,auv,aku,akv]*)
(*}&/@%*)
(*bulkKillings[[1]]*)


(* ::Input::Initialization:: *)
jv=Function[{n},\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
RowBox[{"bulkKillings", "[", 
RowBox[{"u", ",", "v", ",", "z", ",", "0", ",", 
RowBox[{"-", "1"}], ",", "0", ",", "0", ",", "0", ",", "0"}], "]"}], 
RowBox[{"n", "==", 
RowBox[{"-", "1"}]}]},
{
RowBox[{"bulkKillings", "[", 
RowBox[{"u", ",", "v", ",", "z", ",", "0", ",", "0", ",", 
RowBox[{
RowBox[{"-", "1"}], "/", "2"}], ",", 
RowBox[{"1", "/", "2"}], ",", "0", ",", "0"}], "]"}], 
RowBox[{"n", "==", "0"}]},
{
RowBox[{"bulkKillings", "[", 
RowBox[{"u", ",", "v", ",", "z", ",", "0", ",", "0", ",", "0", ",", "0", ",", "1", ",", "0"}], "]"}], 
RowBox[{"n", "==", "1"}]}
},
AllowedDimensions->{2, Automatic},
Editable->True,
GridBoxAlignment->{"Columns" -> {{Left}}, "ColumnsIndexed" -> {}, "Rows" -> {{Baseline}}, "RowsIndexed" -> {}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "ColumnsIndexed" -> {}, "Rows" -> {{1.}}, "RowsIndexed" -> {}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.84]}, Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}, "RowsIndexed" -> {}},
Selectable->True]}
},
GridBoxAlignment->{"Columns" -> {{Left}}, "ColumnsIndexed" -> {}, "Rows" -> {{Baseline}}, "RowsIndexed" -> {}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "ColumnsIndexed" -> {}, "Rows" -> {{1.}}, "RowsIndexed" -> {}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.35]}, Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}, "RowsIndexed" -> {}}],
"Piecewise",
DeleteWithContents->True,
Editable->False,
SelectWithContents->True,
Selectable->False,
StripWrapperBoxes->True]\)//Evaluate]


(* ::Text:: *)
(**)


(* ::Text::Closed:: *)
(*Check Killing's equation:*)


(* ::Input:: *)
(*lieD[ju[#1],metric]&/@Range[-1,1]*)


(* ::Input:: *)
(*lieD[jv[#1],metric]&/@Range[-1,1]*)


(* ::Input:: *)
(**)


(* ::Text::Closed:: *)
(*Check the algebra: [Subscript[L, m],Subscript[L, n]]=(m-n)Subscript[L, m+n]*)


(* ::Input:: *)
(*Subsets[{-1,0,1},{2}]*)


(* ::Input:: *)
(*( *)
(*lieD[ju[#1],ju[#2],{up}]*)
(*-(#1-#2)ju[#1+#2]*)
(*)&@@@Subsets[{-1,0,1},{2}]//Simplify*)


(* ::Input:: *)
(*( *)
(*lieD[jv[#1],jv[#2],{up}]*)
(*-(#1-#2)jv[#1+#2]*)
(*)&@@@Subsets[{-1,0,1},{2}]//Simplify*)


(* ::Input:: *)
(**)


(* ::Input:: *)
(*lieD[ju[#1],jv[#2],{up}]&@@@Tuples[{-1,0,1},2]*)


(* ::Input:: *)
(**)


(* ::Input:: *)
(**)


(* ::Subsubsection::Closed:: *)
(*Derivation of bulk modular flow*)


(* ::Input::Initialization:: *)
coeffNames={lm,l0,lp,Lm,L0,Lp};

\[Chi]def=HoldForm[2\[Pi](
{lm,l0,lp}.(ju/@Range[-1,1])
-{Lm,L0,Lp}.(jv/@Range[-1,1])
)];

\[Chi]=FullSimplify[ReleaseHold[\[Chi]def]]


(* ::Input:: *)
(*lieD[\[Chi],metric]*)


(* ::Input:: *)
(**)


(* ::Input::Initialization:: *)
uv2xt={u->x+t,v->x-t};
xt2uv=Solve[uv2xt/.Rule->Equal,{x,t}]//First


(* ::Input:: *)
(**)


(* ::Input:: *)
(*xt2uv/.{x->SubMinus[x],t->SubMinus[t],u->SubMinus[u],v->SubMinus[v]}*)


(* ::Text:: *)
(*RT surface:*)


(* ::Input::Initialization:: *)
rtSurfaceGeneral=Function[{t,x,z},
(z^2+(x-(SubPlus[x]+SubMinus[x])/2)^2-(t-(SubPlus[t]+SubMinus[t])/2)^2)
-(((SubPlus[x]-SubMinus[x])/2)^2-((SubPlus[t]-SubMinus[t])/2)^2)
];

rtSurfaceLightcone=Function[{u,v,z},
rtSurfaceGeneral[t,x,z]/.xt2uv
/.(xt2uv/.{x->SubMinus[x],t->SubMinus[t],u->SubMinus[u],v->SubMinus[v]})
/.(xt2uv/.{x->SubPlus[x],t->SubPlus[t],u->SubPlus[u],v->SubPlus[v]})//Simplify//Evaluate
]


(* ::Input::Initialization:: *)
length2uv=(l->Sqrt[(SubPlus[u]-SubMinus[u]) (SubPlus[v]-SubMinus[v])]);


(* ::Input:: *)
(*( *)
(*(((SubPlus[x]-SubMinus[x])/2)^2-((SubPlus[t]-SubMinus[t])/2)^2-(l/2)^2)*)
(*/.(xt2uv/.{x->SubMinus[x],t->SubMinus[t],u->SubMinus[u],v->SubMinus[v]})*)
(*/.(xt2uv/.{x->SubPlus[x],t->SubPlus[t],u->SubPlus[u],v->SubPlus[v]})*)
(*/.length2uv*)
(*)//Simplify*)


(* ::Input:: *)
(*rtSurfaceGeneral[t,x,z]-( *)
(**)
(*z^2+(x-SubPlus[x])(x-SubMinus[x])-(t-SubPlus[t])(t-SubMinus[t])*)
(**)
(*)//Simplify*)


(* ::Input:: *)
(*rtSurfaceGeneral[t,x,z]-( *)
(**)
(*z^2+{t-SubPlus[t],x-SubPlus[x]}.({*)
(* {-1, 0},*)
(* {0, 1}*)
(*}).{t-SubMinus[t],x-SubMinus[x]}*)
(**)
(*)//Simplify*)


(* ::Input:: *)
(*rtSurfaceLightcone[u,v,z]-( *)
(**)
(*z^2+{u-SubPlus[u],v-SubPlus[v]}.({*)
(* {0, 1/2},*)
(* {1/2, 0}*)
(*}).{u-SubMinus[u],v-SubMinus[v]}*)
(**)
(*)//Simplify*)


(* ::Input:: *)
(*rtSurfaceLightcone[u,v,z]-( *)
(**)
(*z^2+((u-SubPlus[u])(v-SubMinus[v])+(u-SubMinus[u])(v-SubPlus[v]))/2*)
(**)
(*)//Simplify*)


(* ::Input:: *)
(**)


(* ::Item::Closed:: *)
(*Constant t=0 slice experiments:*)


(* ::Input:: *)
(*( *)
(*rtSurfaceGeneral[0,x,z]==z^2+(x-SubPlus[x])(x-SubMinus[x])*)
(*)/.{SubMinus[t]->0,SubPlus[t]->0}//Simplify*)


(* ::Input:: *)
(**)


(* ::Text:: *)
(*Modular flow should vanish along the RT surface:*)


(* ::Input:: *)
(*Collect[\[Chi]/.{u->x,v->x}/.z->Sqrt[-(x-SubPlus[x])(x-SubMinus[x])],{x},Simplify]*)
(**)
(*coeffRules={*)
(*L0->l0,Lp->lp,*)
(*l0->-(SubPlus[x]+SubMinus[x])lp,Lm->SubPlus[x] SubMinus[x] lp,lm->SubPlus[x] SubMinus[x] lp*)
(*};*)
(*%%//.coeffRules//Simplify*)


(* ::Input:: *)
(**)


(* ::Text:: *)
(*T=\[Kappa]/(2\[Pi])=1, \[Kappa]^2=(2\[Pi])^2=-(1/2)(\!\( *)
(*\*SubscriptBox[\(\[Del]\), \(\[Mu]\)]*)
(*\*SubscriptBox[\(\[Xi]\), \(\[Nu]\)]\))(\[Del]^\[Mu]\[Xi]^\[Nu])*)


(* ::Input:: *)
(*Simplify@Together[*)
(*contract[*)
(*covD[lower[\[Chi]]]**covD[lower[\[Chi]]],*)
(*{1,3},{2,4}*)
(*]/.{u->x,v->x}/.z->Sqrt[-(x-SubPlus[x])(x-SubMinus[x])]//.coeffRules]/(-2 (2\[Pi])^2)*)


(* ::Input:: *)
(**)


(* ::Input:: *)
(*collectCoefficient[*)
(*{lm,l0,lp,Lm,L0,Lp}//.coeffRules,*)
(*lp*)
(*]*)


(* ::Input:: *)
(**)


(* ::Item:: *)
(*General RT surface:*)


(* ::Text:: *)
(*Modular flow should vanish along the RT surface:*)


(* ::Input::Initialization:: *)
rtZ2uv=(z^2->-(((u-SubPlus[u])(v-SubMinus[v])+(u-SubMinus[u])(v-SubPlus[v]))/2))


(* ::Input:: *)
(*rtSurfaceLightcone[u,v,z]/.rtZ2uv//Simplify*)


(* ::Input::Initialization:: *)
Thread[Sqrt[rtZ2uv],Rule]//Simplify

rtZuv=%;


(* ::Text:: *)
(*Also: u,v linear along the RT surface!*)


(* ::Input::Initialization:: *)
rtV2U=(v->(SubPlus[v]+SubMinus[v])/2+(SubPlus[v]-SubMinus[v])/(SubPlus[u]-SubMinus[u]) (u-(SubPlus[u]+SubMinus[u])/2)//FullSimplify);


(* ::Input:: *)
(*CoefficientList[\[Chi]//.rtZ2uv/.rtV2U,u]//FullSimplify//Column*)


(* ::Input::Initialization:: *)
Collect[\[Chi]//.rtZ2uv/.rtV2U,u,FullSimplify]//Column[#,Frame->All]&

CoefficientList[\[Chi]//.rtZ2uv/.rtV2U,u]//FullSimplify;

coeff2lp=Quiet[Solve[Flatten[%]==0,{lm,l0,(*lp,*)Lm,L0,Lp},Reals]]//Simplify//First

%%%//.coeff2lp//Simplify


(* ::Input:: *)
(*length2uv*)


(* ::Input::Initialization:: *)
lpRule=lp->-1/(SubMinus[u]-SubPlus[u]);

coeffs=coeffNames/.coeff2lp/.lpRule//Simplify;
coeffRules=Thread[coeffNames->coeffs]


(* ::Input::Initialization:: *)
\[Xi]=\[Chi]/.coeffRules//FullSimplify


(* ::Input:: *)
(*(\[Xi]/.coeffRules/.rtZ2uv/.rtV2U)//FullSimplify*)


(* ::Input:: *)
(**)


(* ::Subsubsection:: *)
(*Bulk modular flow generator*)


(* ::Input:: *)
(*{ju,jv}*)


(* ::Input:: *)
(*Framed[\[Chi]def]*)


(* ::Input:: *)
(*Framed[coeffRules]*)


(* ::Input:: *)
(*Framed[\[Xi]]*)


(* ::Input:: *)
(*\[Xi][[3]]/(2\[Pi] z)//Apart//Apart*)


(* ::Input:: *)
(*\[Xi]/.{SubPlus[u]->lu/2,SubMinus[u]->-(lu/2),SubPlus[v]->lv/2,SubMinus[v]->-(lv/2)}//Simplify*)
(*collectCoefficient[%,2\[Pi],Expand]*)
(**)


(* ::Input:: *)
(**)


(* ::Text:: *)
(*Check surface gravity:*)


(* ::Text:: *)
(*T=\[Kappa]/(2\[Pi])=1, \[Kappa]^2=(2\[Pi])^2=-(1/2)(\!\( *)
(*\*SubscriptBox[\(\[Del]\), \(\[Mu]\)]*)
(*\*SubscriptBox[\(\[Xi]\), \(\[Nu]\)]\))(\[Del]^\[Mu]\[Xi]^\[Nu])*)


(* ::Input:: *)
(*Simplify[*)
(*contract[*)
(*covD[lower[\[Xi]]]**covD[lower[\[Xi]]],*)
(*{1,3},{2,4}*)
(*]/.rtZ2uv/.rtZuv/.rtV2U]*)


(* ::Input:: *)
(**)


(* ::Text:: *)
(*Fixed t = 0 slice (first in {u,v,z} coordinates, then in {x,t,z} coordinates). Note that the RT surface for general xp, xm is (x-SubPlus[x])(x-SubMinus[x])+z^2=0*)


(* ::Input:: *)
(*\[Xi]t0=Simplify[\[Xi]/.{u->x+t,v->x-t,SubPlus[u]->xp,SubPlus[v]->xp,SubMinus[u]->xm,SubMinus[v]->xm}/.t->0]*)


(* ::Input:: *)
(*(1/2){\[Xi]t0.{1,1,0},\[Xi]t0.{1,-1,0},0}/.{}//FullSimplify*)


(* ::Input:: *)
(**)


(* ::Text::Closed:: *)
(*3D Plot (folded to improve performance of the notebook)*)


(* ::Input:: *)
(*fig0=Show[ParametricPlot3D[{Cos[\[Theta]],\[Lambda],Sin[\[Theta]]},{\[Theta],0,\[Pi]},{\[Lambda],-1.5,1.5},PlotRange->{{-1.25,1.25},{-1.5,1.5},{0,1.25}},Mesh->None,PlotStyle->Directive[RGBColor[0.12582006271512805`, 0.5293439498278976, 0.7809840752581536],Opacity[.1]]],ParametricPlot3D[{Cos[\[Theta]],0,Sin[\[Theta]]},{\[Theta],0,\[Pi]},Mesh->None,PlotStyle->Directive[RGBColor[0.790588, 0.201176, 0.]]],*)
(*ParametricPlot3D[{r Cos[\[Theta]],0,r Sin[\[Theta]]},{\[Theta],0,\[Pi]},{r,0,1},Mesh->None,PlotStyle->Directive[RGBColor[0.790588, 0.201176, 0.],Opacity[.2]]],Graphics3D[{Gray,Thickness[0.005],Opacity[.15],Polygon[{{-1,0,0},{0,1,0},{1,0,0},{0,-1,0}}]}],*)
(*VectorPlot3D[{(1/2)\[Xi].{1,1,0},(1/2)\[Xi].{1,-1,0},\[Xi][[3]]}/.{u->x+t,v->x-t,SubPlus[u]->xp,SubPlus[v]->xp,SubMinus[u]->xm,SubMinus[v]->xm}/.{xp->1,xm->-1},{x,-1.25,1.25},{t,-1.5,1.5},{z,0,1.25},PlotTheme->"Detailed",VectorPoints->7,VectorSizes->.45,VectorAspectRatio->.25],*)
(*ImageSize->400]*)


(* ::Input:: *)
(*Export["~/Library/Mobile\ Documents/com~apple~CloudDocs/iPad/bulkflow.png",Show[fig0],"PNG"]*)


(* ::Input:: *)
(**)


(* ::Subsubsection:: *)
(*Boundary modular flow generator*)


(* ::Text:: *)
(*This is the modular flow generator at the boundary:*)


(* ::Input::Initialization:: *)
\[Xi]b=Simplify[\[Xi][[1;;2]]/.z->0]


(* ::Input:: *)
(**)


(* ::Text:: *)
(*Check it reduces to the generator of boosts near one of the endpoints:*)


(* ::Input:: *)
(*Normal[Series[\[Xi]b,{SubMinus[u],\[Infinity],0},{SubMinus[v],\[Infinity],0}]]*)


(* ::Input:: *)
(**)


(* ::Text:: *)
(*Plot in Lorentzian signature:*)


(* ::Input::Initialization:: *)
\[Xi]bplot[x_,t_,xm_,xp_,region_:1,signature_:-1]:=Module[{xgen},
xgen=If[signature==1,
Simplify[(I/2){\[Xi]b.{1,1},-I \[Xi]b.{1,-1}}/.{u->x+I t,v->x-I t,SubPlus[u]->xp,SubPlus[v]->xp,SubMinus[u]->xm,SubMinus[v]->xm}],
Simplify[(1/2){\[Xi]b.{1,1},\[Xi]b.{1,-1}}/.{u->x+t,v->x-t,SubPlus[u]->xp,SubPlus[v]->xp,SubMinus[u]->xm,SubMinus[v]->xm}]];
If[region==1,
Piecewise[{{xgen,xm<t+x<xp&&-xp<t-x<-xm},{{0,0},t+x<xm||x+t>xp||t-x<-xp||t-x>-xm}}],
If[region==2,
xgen,
Piecewise[{{{0,0},xm<t+x<xp&&-xp<t-x<-xm},{xgen,t+x<xm||x+t>xp||t-x<-xp||t-x>-xm}}]
]
]
]


(* ::Input:: *)
(**)


(* ::Item::Closed:: *)
(*Flow streams: Lorentzian*)


(* ::Input:: *)
(*fig1=Show[*)
(*StreamPlot[\[Xi]bplot[x,t,-1,1,0,-1],{x,-1.25,1.25},{t,-1.25,1.25},*)
(*StreamScale-> {.075,Automatic,Automatic},StreamStyle->Directive[RGBColor[1., 0.607843, 0.]],FrameTicks->None,GridLines->None],*)
(*Graphics[{Gray,Thickness[0.005],Opacity[.15],Polygon[{{-1,0},{0,1},{1,0},{0,-1}}]}],*)
(*StreamPlot[\[Xi]bplot[x,t,-1,1],{x,-1.15,1.15},{t,-1.15,1.15},*)
(*StreamScale-> {.075,Automatic,Automatic},StreamStyle->Directive[RGBColor[0.790588, 0.201176, 0.]],FrameTicks->None,GridLines->None],*)
(*Graphics[{RGBColor[0.192157, 0.388235, 0.807843],Thickness[0.005],Opacity[1],Line[{{-1,0},{1,0}}]}],*)
(*Graphics[{RGBColor[0.192157, 0.388235, 0.807843],Thick,Opacity[1],Disk[{-1,0},.01]}],*)
(*Graphics[{RGBColor[0.192157, 0.388235, 0.807843],Thick,Opacity[1],Disk[{1,0},.01]}],*)
(*ImageSize->400*)
(*]*)


(* ::Input:: *)
(*Export["~/Library/Mobile\ Documents/com~apple~CloudDocs/iPad/Lflow.png",Show[fig1],"PNG"]*)


(* ::Input:: *)
(**)


(* ::Item::Closed:: *)
(*Flow streams: Euclidean*)


(* ::Input:: *)
(*fig2=Show[*)
(*StreamPlot[\[Xi]bplot[x,t,-1,1,2,1],{x,-1.25,1.25},{t,-1.25,1.25},*)
(*StreamScale-> {.075,Automatic,Automatic},StreamStyle->Directive[RGBColor[0.790588, 0.201176, 0.]],FrameTicks->None,GridLines->None],*)
(*Graphics[{RGBColor[0.192157, 0.388235, 0.807843],Thickness[0.005],Opacity[1],Line[{{-1,0},{1,0}}]}],*)
(*Graphics[{RGBColor[0.192157, 0.388235, 0.807843],Thick,Opacity[1],Disk[{-1,0},.01]}],*)
(*Graphics[{RGBColor[0.192157, 0.388235, 0.807843],Thick,Opacity[1],Disk[{1,0},.01]}],*)
(*ImageSize->400*)
(*]*)


(* ::Input:: *)
(*Export["~/Library/Mobile\ Documents/com~apple~CloudDocs/iPad/Eflow.png",Show[fig2],"PNG"]*)


(* ::Input:: *)
(**)


(* ::Input:: *)
(* {1/2 \[Xi].{1,1,0},1/2 \[Xi].{1,-1,0},\[Xi][[-1]]}/. {u->x+t,v->x-t,SubPlus[u]->1,SubPlus[v]->1,SubMinus[u]->-1,SubMinus[v]->-1}//Simplify*)


(* ::Input:: *)
(*fig3=Show[*)
(*ContourPlot[*)
(*1/2 \[Xi].{1,-1,0}/. {u->x,v->x,SubPlus[u]->1,SubPlus[v]->1,SubMinus[u]->-1,SubMinus[v]->-1}//Evaluate,*)
(*{x,-1.25,1.25},{z,0.01,1.25},*)
(*AspectRatio->Automatic,*)
(*ContourLabels->(Text["\!\(\*SuperscriptBox[\(\[Xi]\), \(t\)]\) = "<>ToString@#3,{0,Sqrt[#1^2+#2^2]},{0,-1},Background->{White,Opacity[.5]}]&)*)
(*],*)
(*ImageSize->400*)
(*]*)


(* ::Item:: *)
(*Note that \[Xi]^x,\[Xi]^z both vanish in the t=0 plane. \[Xi]^t=0 corresponds to the RT surface, where \[Xi]^\[Mu] vanish entirely.*)


(* ::Item::Closed:: *)
(*Flow lines in x, z plane (slow)*)


(* ::Input:: *)
(*fig3more=Show[*)
(*VectorPlot[*)
(*{1/2 \[Xi].{1,1,0},\[Xi][[-1]]}/. {u->x+t,v->x-t,SubPlus[u]->1,SubPlus[v]->1,SubMinus[u]->-1,SubMinus[v]->-1}/.t->.9//Simplify//Evaluate,*)
(*{x,-1.25,1.25},{z,0.01,1.25},*)
(*AspectRatio->Automatic,VectorScaling->"Log"*)
(*(*ContourLabels\[Rule](Text["\[Xi]^t = "<>ToString@#3,{0,Sqrt[#1^2+#2^2]},{0,-1},Background\[Rule]{White,Opacity[.5]}]&)*)*)
(*],*)
(*ImageSize->400*)
(*]*)


(* ::Input:: *)
(**)


(* ::Input:: *)
(* {1/2 \[Xi].{1,-1,0},\[Xi][[-1]]}/. {u->x+t,v->x-t,SubPlus[u]->1,SubPlus[v]->1,SubMinus[u]->-1,SubMinus[v]->-1}//Simplify*)


(* ::Item::Closed:: *)
(*Flow lines in t, z plane (slow)*)


(* ::Input:: *)
(*fig4=GraphicsRow[*)
(*StreamPlot[*)
(*{\[Xi][[-1]],1/2 \[Xi].{1,-1,0}}/. {u->x+t,v->x-t,SubPlus[u]->1,SubPlus[v]->1,SubMinus[u]->-1,SubMinus[v]->-1}/.x->#//Simplify//Evaluate,*)
(*{z,0.01,1.25},{t,-1.1,1.1},*)
(*AspectRatio->Automatic,*)
(*StreamPoints->50,*)
(*PlotLabel->HoldForm[x==#],*)
(*FrameTicks->{Automatic,If[#==0,Automatic,None]},*)
(*FrameLabel->{HoldForm[z],If[#==0,HoldForm[t],None]}*)
(*]&/@{0.,.8,.95},*)
(*ImageSize->640*)
(*]*)
(*(*exportPlot[fig4,""]*)*)


(* ::Input:: *)
(**)


(* ::Input::Initialization:: *)
saveScript[];
